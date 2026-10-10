//! The heap: blocks, counting references to them, and collecting the ones
//! that counting cannot.
//!
//! # Blocks
//!
//! A block is some words. The first is its header: the kind in the low 8
//! bits, flags in the next 8, a constructor's tag in the next 16, and a count
//! in the upper 32. The second is how many references there are to it. What
//! follows depends on the kind:
//!
//! * `DATA`: the fields, as many as the count.
//! * `CLOSURE`: its code, and then what it captured, as many as the count.
//! * `THUNK`: the same as a closure.
//! * `WORKING`: a thunk whose code is running. What it captured has been
//!   taken by the code, and the words are stale.
//! * `DONE`: a thunk that has been forced. Its value is where its code was.
//! * `INTEGER`: the address of the number, which Rust keeps.
//! * `TEXT`: its length in bytes, and then the bytes, as UTF-8.
//!
//! These layouts are also written in `compiler/LLVM/FromAxCut.hs`. The two
//! must agree.
//!
//! # Counting
//!
//! The compiled program owns every reference it holds and says when it makes
//! another (`hazy_share`) and when it gives one up (`hazy_erase`). A block
//! whose count reaches zero is not taken apart at once: it goes on a list,
//! and a few blocks of the list are taken apart at each allocation. So giving
//! up the head of a long list does not stop the program.
//!
//! A definition at the top of a program is not counted. Its count is a large
//! number that means so.
//!
//! # Collecting
//!
//! Counting never frees blocks that refer to each other in a circle, and a
//! lazy language makes those freely. So now and then everything that can
//! still be reached is marked, and what is not marked is freed.
//!
//! What can be reached is known exactly, and cheaply, because a compiled
//! program has no stack: every call is a tail call. At the start of a
//! function the only values alive are the ones the function was given, and
//! the values of definitions at the top that have been forced. The program
//! checks at the start of each function whether a collection is due and, if
//! it is, hands over what it was given.
//!
//! At a collection every count can also be worked out again from nothing and
//! compared with what was counted. That is what `HAZY_GC_VERIFY` asks for.

use std::collections::HashMap;

pub const DATA: u64 = 0;
pub const CLOSURE: u64 = 1;
pub const THUNK: u64 = 2;
pub const WORKING: u64 = 3;
pub const DONE: u64 = 4;
pub const INTEGER: u64 = 5;
pub const TEXT: u64 = 6;
/// A slot that holds no block.
const FREE: u64 = 255;

const KIND: u64 = 0xff;
/// The block was reached in the collection under way.
const MARK: u64 = 1 << 8;
/// The block's count reached zero and it is waiting to be taken apart.
const PENDING: u64 = 1 << 9;

/// The count of a block that is not counted.
pub const IMMORTAL: u64 = 1 << 62;

pub type Value = u64;

pub fn header(kind: u64, tag: u64, count: u64) -> u64 {
    kind | tag << 16 | count << 32
}

fn count(header: u64) -> usize {
    (header >> 32) as usize
}

/// The block a value is, if it is one that is counted.
fn counted(value: Value) -> Option<*mut u64> {
    if value & 1 == 1 || value == 0 {
        return None;
    }
    let block = value as *mut u64;
    if unsafe { *block.add(1) } >= IMMORTAL {
        None
    } else {
        Some(block)
    }
}

/// How many words a block is.
unsafe fn words(block: *mut u64) -> usize {
    let head = *block;
    match head & KIND {
        DATA => 2 + count(head),
        CLOSURE | THUNK | WORKING | DONE => 3 + count(head),
        INTEGER => 3,
        TEXT => 3 + (*block.add(2) as usize + 7) / 8,
        kind => panic!("a block of an unknown kind: {kind}"),
    }
}

/// The values a block holds references to.
unsafe fn children(block: *mut u64) -> &'static [Value] {
    let head = *block;
    match head & KIND {
        DATA => std::slice::from_raw_parts(block.add(2), count(head)),
        CLOSURE | THUNK => std::slice::from_raw_parts(block.add(3), count(head)),
        DONE => std::slice::from_raw_parts(block.add(2), 1),
        _ => &[],
    }
}

/// Blocks of up to this many words are kept in slots of their own size.
const SMALL: usize = 32;
/// How many words of slots are asked for at a time.
const CHUNK: usize = 1 << 13;

struct Class {
    /// The first free slot. Each free slot holds the next in its second word.
    free: *mut u64,
    /// The slots there are: where each run starts and how many it has.
    chunks: Vec<(*mut u64, usize)>,
}

struct Heap {
    /// One for each size, from zero words to `SMALL`.
    classes: Vec<Class>,
    /// The blocks too big for a slot, each with its size.
    large: Vec<(*mut u64, usize)>,
    /// The blocks waiting to be taken apart, each holding the next in its
    /// second word.
    pending: *mut u64,
    /// The thunks at the top of the program that have been forced.
    roots: Vec<*mut u64>,
    /// How many bytes have been allocated since the last collection.
    since: usize,
    /// How many bytes may be allocated before the next collection.
    threshold: usize,
    floor: usize,
    verify: bool,
    report: bool,
    counted: u64,
    traced: u64,
    collections: u64,
}

static mut HEAP: Option<Heap> = None;

/// Whether a collection is due. The program reads this at the start of every
/// function.
#[no_mangle]
pub static mut hazy_gc_pending: u64 = 0;

fn heap() -> &'static mut Heap {
    // The program has one thread.
    unsafe {
        let heap = &mut *std::ptr::addr_of_mut!(HEAP);
        heap.get_or_insert_with(|| {
            let setting = |name: &str| std::env::var(name).ok().filter(|value| !value.is_empty());
            let floor = setting("HAZY_GC_THRESHOLD")
                .and_then(|value| value.parse().ok())
                .unwrap_or(8 << 20);
            Heap {
                classes: (0..=SMALL)
                    .map(|_| Class {
                        free: std::ptr::null_mut(),
                        chunks: Vec::new(),
                    })
                    .collect(),
                large: Vec::new(),
                pending: std::ptr::null_mut(),
                roots: Vec::new(),
                since: 0,
                threshold: floor,
                floor,
                verify: setting("HAZY_GC_VERIFY").is_some(),
                report: setting("HAZY_GC_STATS").is_some(),
                counted: 0,
                traced: 0,
                collections: 0,
            }
        })
    }
}

impl Heap {
    unsafe fn take(&mut self, size: usize) -> *mut u64 {
        if size > SMALL {
            let block = Vec::<u64>::with_capacity(size).leak().as_mut_ptr();
            self.large.push((block, size));
            return block;
        }
        let class = &mut self.classes[size];
        if class.free.is_null() {
            let slots = (CHUNK / size).max(1);
            let chunk = Vec::<u64>::with_capacity(slots * size).leak().as_mut_ptr();
            for index in (0..slots).rev() {
                let slot = chunk.add(index * size);
                *slot = FREE;
                *slot.add(1) = class.free as u64;
                class.free = slot;
            }
            class.chunks.push((chunk, slots));
        }
        let block = class.free;
        class.free = *block.add(1) as *mut u64;
        block
    }

    /// Give a block's words back. What it refers to is not looked at.
    unsafe fn give(&mut self, block: *mut u64) {
        let size = words(block);
        if (*block & KIND) == INTEGER {
            drop(Box::from_raw(*block.add(2) as *mut num_bigint::BigInt));
        }
        if size > SMALL {
            let place = self.large.iter().position(|(other, _)| *other == block);
            let (_, size) = self.large.swap_remove(place.expect("a large block that is not known"));
            drop(Vec::from_raw_parts(block, 0, size));
        } else {
            let class = &mut self.classes[size];
            *block = FREE;
            *block.add(1) = class.free as u64;
            class.free = block;
        }
    }

    /// Take apart some of the blocks whose count reached zero.
    unsafe fn settle(&mut self, mut budget: usize) {
        while budget > 0 && !self.pending.is_null() {
            let block = self.pending;
            self.pending = *block.add(1) as *mut u64;
            *block &= !PENDING;
            for &child in children(block) {
                self.erase(child);
            }
            self.give(block);
            self.counted += 1;
            budget -= 1;
        }
    }

    unsafe fn erase(&mut self, value: Value) {
        if let Some(block) = counted(value) {
            let references = *block.add(1) - 1;
            if references == 0 {
                *block |= PENDING;
                *block.add(1) = self.pending as u64;
                self.pending = block;
            } else {
                *block.add(1) = references;
            }
        }
    }

    /// Every block there is, whether it can be reached or not.
    unsafe fn blocks(&self) -> Vec<*mut u64> {
        let mut found = Vec::new();
        for (size, class) in self.classes.iter().enumerate() {
            for &(chunk, slots) in &class.chunks {
                for index in 0..slots {
                    let slot = chunk.add(index * size);
                    if (*slot & KIND) != FREE {
                        found.push(slot);
                    }
                }
            }
        }
        found.extend(self.large.iter().map(|(block, _)| *block));
        found
    }

    unsafe fn collect(&mut self, given: &[Value]) {
        // So that every block there is has its references counted in full.
        self.settle(usize::MAX);
        self.collections += 1;

        let mut stack: Vec<*mut u64> = Vec::new();
        let reach = |value: Value, stack: &mut Vec<*mut u64>| {
            if let Some(block) = counted(value) {
                if *block & MARK == 0 {
                    *block |= MARK;
                    stack.push(block);
                }
            }
        };
        for &value in given {
            reach(value, &mut stack);
        }
        for &thunk in &self.roots {
            reach(*thunk.add(2), &mut stack);
        }
        while let Some(block) = stack.pop() {
            for &child in children(block) {
                reach(child, &mut stack);
            }
        }

        let blocks = self.blocks();
        if self.verify {
            self.check(&blocks, given);
        }

        // What was not reached is in a circle, or hangs from one. It still
        // holds references to blocks that were reached, which are given up
        // before it goes.
        for &block in &blocks {
            if *block & MARK == 0 {
                for &child in children(block) {
                    if let Some(child) = counted(child) {
                        if *child & MARK != 0 {
                            *child.add(1) -= 1;
                        }
                    }
                }
            }
        }
        let mut live = 0;
        for &block in &blocks {
            if *block & MARK == 0 {
                self.give(block);
                self.traced += 1;
            } else {
                *block &= !MARK;
                live += words(block) * 8;
            }
        }

        self.since = 0;
        self.threshold = self.floor.max(live);
        hazy_gc_pending = 0;
    }

    /// Work every count out again and compare it with what was counted.
    unsafe fn check(&self, blocks: &[*mut u64], given: &[Value]) {
        let mut expected: HashMap<*mut u64, u64> = HashMap::new();
        let mut refer = |value: Value, from: &str| {
            if let Some(block) = counted(value) {
                if (*block & KIND) == FREE {
                    wrong(&format!("{from} refers to a block that was freed"));
                }
                *expected.entry(block).or_insert(0) += 1;
            }
        };
        for &block in blocks {
            for &child in children(block) {
                refer(child, "a block");
            }
        }
        for &value in given {
            refer(value, "the program");
        }
        for &thunk in &self.roots {
            refer(*thunk.add(2), "a definition");
        }
        for &block in blocks {
            let references = *block.add(1);
            let found = expected.get(&block).copied().unwrap_or(0);
            if references != found {
                // For an object, say where its code is, to find it by.
                let code = match *block & KIND {
                    CLOSURE | THUNK | WORKING => format!(", code at {:#x}", *block.add(2)),
                    _ => String::new(),
                };
                wrong(&format!(
                    "a block of kind {} and tag {} is counted {} times and referred to {} times{}",
                    *block & KIND,
                    (*block >> 16) & 0xffff,
                    references,
                    found,
                    code
                ));
            }
        }
    }
}

fn wrong(message: &str) -> ! {
    use std::io::Write;
    let _ = std::io::stdout().flush();
    eprintln!("the heap is wrong: {message}");
    std::process::exit(2)
}

/// A new block of some words with a header, and one reference to it.
#[no_mangle]
pub extern "C" fn hazy_alloc(size: u64, head: u64) -> *mut u64 {
    let heap = heap();
    let size = size as usize;
    unsafe {
        heap.settle(8);
        let block = heap.take(size);
        *block = head;
        *block.add(1) = 1;
        heap.since += size * 8;
        if heap.since >= heap.threshold {
            hazy_gc_pending = 1;
        }
        block
    }
}

/// There is one more reference to a value.
#[no_mangle]
pub extern "C" fn hazy_share(value: Value) {
    if let Some(block) = counted(value) {
        unsafe { *block.add(1) += 1 };
    }
}

/// There is one reference fewer to a value.
#[no_mangle]
pub extern "C" fn hazy_erase(value: Value) {
    unsafe { heap().erase(value) }
}

/// A reference to a block is given up by someone who has read what the block
/// holds and now owns it. If that was the last reference the block's words
/// are freed and nothing else is done. Otherwise what it holds is shared.
#[no_mangle]
pub extern "C" fn hazy_open(value: Value) {
    if let Some(block) = counted(value) {
        unsafe {
            let references = *block.add(1);
            if references == 1 {
                heap().give(block);
                heap().counted += 1;
            } else {
                for &child in children(block) {
                    hazy_share(child);
                }
                // Read again: a block that captures itself was just shared.
                *block.add(1) -= 1;
            }
        }
    }
}

/// A thunk has been given its value, by something that held a reference to
/// the thunk. A thunk at the top of the program is remembered, since its
/// value is alive from now on.
#[no_mangle]
pub extern "C" fn hazy_settle(thunk: Value) {
    let block = thunk as *mut u64;
    unsafe {
        if *block.add(1) >= IMMORTAL {
            heap().roots.push(block);
        } else {
            heap().erase(thunk);
        }
    }
}

/// Free what cannot be reached from some values, which are everything the
/// program holds.
#[no_mangle]
pub extern "C" fn hazy_collect(given: *const Value, length: u64) {
    let given = if length == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(given, length as usize) }
    };
    unsafe { heap().collect(given) }
}

/// The program is over and holds nothing.
pub fn finish() {
    let heap = heap();
    if heap.verify {
        unsafe { heap.collect(&[]) };
    }
    if heap.report {
        eprintln!(
            "collections: {}, freed by counting: {}, freed by tracing: {}",
            heap.collections, heap.counted, heap.traced
        );
    }
}
