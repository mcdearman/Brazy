//! The heap: blocks, and counting references to them.
//!
//! # Blocks
//!
//! A block is some words. The first is its header, which holds in one word
//! what the block is and how many references there are to it:
//!
//! * bits 0 to 3: the kind;
//! * bits 4 and 5: flags used while blocks are freed and checked;
//! * bits 6 to 18: a constructor's tag;
//! * bits 19 to 31: a count, of fields or of captured values;
//! * bits 32 to 63: how many references there are. A block with the top bit
//!   set is not counted.
//!
//! What follows the header depends on the kind:
//!
//! * `DATA`: the fields, as many as the count.
//! * `CLOSURE`: its code, and then what it captured, as many as the count.
//! * `THUNK`: the same as a closure.
//! * `WORKING`: a thunk whose code is running. What it captured has been
//!   taken by the code, and the words are stale.
//! * `DONE`: a thunk that has been forced. Its value is where its code was.
//! * `INTEGER`: the address of the number, which Rust keeps.
//! * `TEXT`: its length in bytes, and then the bytes, as UTF-8.
//! * `SLICE`: part of a text: the text, where the part starts in its bytes,
//!   and how many bytes it is. The text is a `TEXT`, never another part.
//! * `FLOAT`: a floating-point number, as the bits of a double. A `Float`
//!   is kept as the double it is exactly.
//! * `ARRAY`: how many places it has, and then the places, each a value as
//!   it was given, evaluated or not. Its length has a word of its own: a
//!   header has room for fewer fields than an array may have.
//!
//! A constructor with no fields is never allocated. There is one block for
//! each tag in `hazy_nullary`, and it is not counted.
//!
//! These layouts are also written in `compiler/LLVM/FromAxCut.hs`. The two
//! must agree.
//!
//! # Counting
//!
//! The compiled program owns every reference it holds. It adds to a count and
//! takes from it in its own code, and calls here only when a count reaches
//! zero (`hazy_free`). The block is taken apart then, and what it held that
//! is no longer held goes on a list, of which a few blocks are taken apart
//! then and at each allocation. So giving up the head of a long list does
//! not stop the program.
//!
//! # Allocating
//!
//! A small block is taken from a list of free blocks of its size, and the
//! compiled program does that itself (`hazy_take`, in
//! `compiler/LLVM/FromAxCut.hs`), from the lists here
//! (`hazy_free_lists`). It calls here when a list is empty, or when there is
//! more to do at an allocation (`hazy_alloc_slow`): blocks to take apart,
//! allocations to sample, or counts to check.
//!
//! A definition at the top of a program is not counted.
//!
//! # No collector
//!
//! Counting never frees blocks that refer to each other in a circle. There
//! was a collector here that traced what could be reached and freed the
//! rest. It is gone: the compiler now sees to it that a program makes no
//! circle outside the definitions at its top, which are never freed anyway
//! (`compiler/Sequent/Hoist.hs`). So a program pays for counting and for
//! nothing else.
//!
//! # Checking
//!
//! What a program holds is known exactly, and cheaply, because a compiled
//! program has no stack: every call is a tail call. At the start of a
//! function the only values alive are the ones the function was given, and
//! the values of definitions at the top that have been forced.
//!
//! So a program that was compiled to be checked (`--checked`) hands over
//! what it was given at the start of each function, when it is asked to,
//! and then every count is worked out again from nothing and compared with
//! what was counted. A block that nothing reaches is an error too: either a
//! reference to it was lost, or it is part of a circle. `HAZY_CHECK` asks
//! for this, at every chance or, given a number, after every so many
//! allocations. It is also done once at the end, where nothing is held, by
//! any program.

use std::collections::HashMap;

pub const DATA: u64 = 0;
pub const CLOSURE: u64 = 1;
pub const THUNK: u64 = 2;
pub const WORKING: u64 = 3;
pub const DONE: u64 = 4;
pub const INTEGER: u64 = 5;
pub const TEXT: u64 = 6;
/// What is left of a value that was taken apart, kept for one of as many
/// fields to be built in. It holds nothing.
pub const HUSK: u64 = 7;
/// Part of a text, which it holds a reference to.
pub const SLICE: u64 = 8;
/// Places one after another, which a vector is made of.
pub const ARRAY: u64 = 9;
/// A floating-point number.
pub const FLOAT: u64 = 10;
/// A slot that holds no block.
const FREE: u64 = 15;

const KIND: u64 = 0xf;
/// The block was reached in the collection under way.
const MARK: u64 = 1 << 4;
/// The block's count reached zero and it is waiting to be taken apart.
const PENDING: u64 = 1 << 5;
/// One reference, as it is added to a header.
const ONE: u64 = 1 << 32;
/// The bit of a header that says the block is not counted.
pub const IMMORTAL: u64 = 1 << 63;
/// The most tags and the most fields there can be.
pub const LIMIT: usize = 1 << 13;

pub type Value = u64;

/// The header of a block with no references yet.
pub const fn header(kind: u64, tag: u64, count: u64) -> u64 {
    kind | tag << 6 | count << 19
}

/// The tag of an `Integer` that is kept in its block.
pub const SMALL_INTEGER: u64 = 1;

pub fn tag(header: u64) -> u64 {
    (header >> 6) & (LIMIT as u64 - 1)
}

fn count(header: u64) -> usize {
    ((header >> 19) & (LIMIT as u64 - 1)) as usize
}

fn references(header: u64) -> u64 {
    header >> 32
}

/// The constructors with no fields, one for each tag.
#[no_mangle]
pub static hazy_nullary: [u64; LIMIT] = {
    let mut blocks = [0; LIMIT];
    let mut tag = 0;
    while tag < LIMIT {
        blocks[tag] = header(DATA, tag as u64, 0) | IMMORTAL;
        tag += 1;
    }
    blocks
};

/// A constructor with no fields.
pub fn nullary(tag: u64) -> Value {
    std::ptr::addr_of!(hazy_nullary[tag as usize]) as Value
}

/// The block a value is, if it is one that is counted.
fn counted(value: Value) -> Option<*mut u64> {
    if value & 1 == 1 || value == 0 {
        return None;
    }
    let block = value as *mut u64;
    if unsafe { *block } & IMMORTAL != 0 {
        None
    } else {
        Some(block)
    }
}

/// How many words a block is.
unsafe fn words(block: *mut u64) -> usize {
    let head = *block;
    match head & KIND {
        DATA | HUSK => 1 + count(head),
        CLOSURE | THUNK | WORKING | DONE => 2 + count(head),
        INTEGER => 2,
        TEXT => 2 + (*block.add(1) as usize + 7) / 8,
        SLICE => 4,
        ARRAY => 2 + *block.add(1) as usize,
        FLOAT => 2,
        kind => panic!("a block of an unknown kind: {kind}"),
    }
}

/// Whether a block holds nothing, and is two words: a floating-point
/// number, or an `Integer` kept in its block. It can go straight back.
fn leaf(head: u64) -> bool {
    match head & KIND {
        FLOAT => true,
        INTEGER => tag(head) == SMALL_INTEGER,
        _ => false,
    }
}

/// Give back a block that holds nothing and is two words.
#[inline(always)]
unsafe fn give_leaf(block: *mut u64) {
    let free = free_list(2);
    *block = FREE;
    *block.add(1) = *free as u64;
    *free = block;
    hazy_freed += 1;
}

/// The values a block holds references to.
unsafe fn children(block: *mut u64) -> &'static [Value] {
    let head = *block;
    match head & KIND {
        DATA => std::slice::from_raw_parts(block.add(1), count(head)),
        CLOSURE | THUNK => std::slice::from_raw_parts(block.add(2), count(head)),
        DONE | SLICE => std::slice::from_raw_parts(block.add(1), 1),
        ARRAY => std::slice::from_raw_parts(block.add(2), *block.add(1) as usize),
        _ => &[],
    }
}

/// Blocks of up to this many words are kept in slots of their own size.
const SMALL: usize = 32;
/// How many words of slots are asked for at a time.
const CHUNK: usize = 1 << 13;

struct Class {
    /// The slots there are: where each run starts and how many it has.
    chunks: Vec<(*mut u64, usize)>,
}

/// The first free slot of each size, from zero words to `SMALL`. Each free
/// slot holds the next in its second word. The compiled program takes from
/// these itself.
#[no_mangle]
pub static mut hazy_free_lists: [*mut u64; SMALL + 1] = [std::ptr::null_mut(); SMALL + 1];

/// Whether an allocation has more to do than take a free block, and so has
/// to be done here.
#[no_mangle]
pub static mut hazy_alloc_slow: u64 = 0;

/// How many blocks have been allocated. The compiled program counts these
/// itself.
#[no_mangle]
pub static mut hazy_allocated: u64 = 0;

/// How many blocks have been given back. The compiled program counts the
/// ones it gives back itself.
#[no_mangle]
pub static mut hazy_freed: u64 = 0;

fn free_list(size: usize) -> &'static mut *mut u64 {
    unsafe { &mut (*std::ptr::addr_of_mut!(hazy_free_lists))[size] }
}

struct Heap {
    /// One for each size, from zero words to `SMALL`.
    classes: Vec<Class>,
    /// The blocks too big for a slot, each with its size. A block is found
    /// here by its address when it is given back: this was a list, gone
    /// through from the start each time, and a program that holds many long
    /// texts spent most of its time there.
    large: HashMap<*mut u64, usize>,
    /// The blocks waiting to be taken apart.
    pending: Vec<*mut u64>,
    /// The thunks at the top of the program that have been forced.
    roots: Vec<*mut u64>,
    /// What catches a thrown value, the last begun last: a handler and where
    /// to go on with what it gives. The program holds these through here.
    catching: Vec<(Value, Value)>,
    /// The functions of C that wrappers made, each by its address, with the
    /// place of the two values it calls, which the program holds there.
    wrapped: HashMap<u64, *mut Value>,
    /// Whether every allocation is to be done here, to be sampled or
    /// counted for a check.
    always: u64,
    /// After how many allocations every count is checked, if they are.
    check: Option<u64>,
    /// How many allocations there have been since the last check.
    since: u64,
    report: bool,
    /// Every how many allocations the function that allocated is noted,
    /// if that is asked for, and how many times each was.
    sample: Option<u64>,
    sampled: HashMap<String, u64>,
}

/// How many times a block was built in the place of one just taken apart.
/// The program counts these itself.
#[no_mangle]
pub static mut hazy_reused: u64 = 0;

/// How many times a function was called with what it only reads lent to it.
/// The program counts these itself.
#[no_mangle]
pub static mut hazy_lent: u64 = 0;

static mut HEAP: Option<Heap> = None;

/// Whether a check is due. A program that was compiled to be checked reads
/// this at the start of every function.
#[no_mangle]
pub static mut hazy_check_due: u64 = 0;

fn heap() -> &'static mut Heap {
    // The program has one thread.
    unsafe {
        let heap = &mut *std::ptr::addr_of_mut!(HEAP);
        heap.get_or_insert_with(|| {
            let setting = |name: &str| std::env::var(name).ok().filter(|value| !value.is_empty());
            Heap {
                classes: (0..=SMALL).map(|_| Class { chunks: Vec::new() }).collect(),
                large: HashMap::new(),
                pending: Vec::new(),
                roots: Vec::new(),
                catching: Vec::new(),
                wrapped: HashMap::new(),
                always: (setting("HAZY_CHECK").is_some() || setting("HAZY_PROFILE").is_some()) as u64,
                check: setting("HAZY_CHECK").map(|value| value.parse().unwrap_or(1).max(1)),
                since: 0,
                report: setting("HAZY_STATS").is_some(),
                sample: setting("HAZY_PROFILE").map(|value| value.parse().unwrap_or(1 << 20).max(1)),
                sampled: HashMap::new(),
            }
        })
    }
}

impl Heap {
    unsafe fn take(&mut self, size: usize) -> *mut u64 {
        if size > SMALL {
            let block = Vec::<u64>::with_capacity(size).leak().as_mut_ptr();
            self.large.insert(block, size);
            return block;
        }
        let free = free_list(size);
        if free.is_null() {
            let slots = (CHUNK / size).max(1);
            let chunk = Vec::<u64>::with_capacity(slots * size).leak().as_mut_ptr();
            for index in (0..slots).rev() {
                let slot = chunk.add(index * size);
                *slot = FREE;
                *slot.add(1) = *free as u64;
                *free = slot;
            }
            self.classes[size].chunks.push((chunk, slots));
        }
        let block = *free;
        *free = *block.add(1) as *mut u64;
        block
    }

    /// Give a block's words back. What it refers to is not looked at.
    #[inline(always)]
    unsafe fn give(&mut self, block: *mut u64) {
        let size = words(block);
        if (*block & KIND) == INTEGER && tag(*block) != SMALL_INTEGER {
            drop(Box::from_raw(*block.add(1) as *mut num_bigint::BigInt));
        }
        if size > SMALL {
            let size = self.large.remove(&block).expect("a large block that is not known");
            drop(Vec::from_raw_parts(block, 0, size));
        } else {
            let free = free_list(size);
            *block = FREE;
            *block.add(1) = *free as u64;
            *free = block;
        }
    }

    /// Take apart some of the blocks whose count reached zero.
    unsafe fn settle(&mut self, mut budget: usize) {
        while budget > 0 {
            let Some(block) = self.pending.pop() else { break };
            *block &= !PENDING;
            self.take_apart(block);
            budget -= 1;
        }
        if self.pending.is_empty() {
            hazy_alloc_slow = self.always;
        }
    }

    /// Give up what a block holds, and then its words.
    #[inline(always)]
    unsafe fn take_apart(&mut self, block: *mut u64) {
        for &child in children(block) {
            self.erase(child);
        }
        self.give(block);
        hazy_freed += 1;
    }

    #[inline(always)]
    unsafe fn erase(&mut self, value: Value) {
        if let Some(block) = counted(value) {
            *block -= ONE;
            if references(*block) == 0 {
                if leaf(*block) {
                    give_leaf(block);
                } else {
                    self.free(block);
                }
            }
        }
    }

    /// A block's count has reached zero.
    #[inline(always)]
    unsafe fn free(&mut self, block: *mut u64) {
        *block |= PENDING;
        self.pending.push(block);
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
        found.extend(self.large.keys().copied());
        found
    }

    /// Check every count, given everything the program holds, and that
    /// every block there is can be reached from that.
    unsafe fn examine(&mut self, given: &[Value]) {
        // Inside a call from C into the program, what the program held
        // before it called C is held where this cannot see it.
        if self.catching.iter().any(|&(handler, _)| handler == BARRIER) {
            self.since = 0;
            hazy_check_due = 0;
            return;
        }
        // So that every block there is has its references counted in full.
        self.settle(usize::MAX);

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
            reach(*thunk.add(1), &mut stack);
        }
        for &(handler, resume) in &self.catching {
            reach(handler, &mut stack);
            reach(resume, &mut stack);
        }
        for &slot in self.wrapped.values() {
            reach(*slot, &mut stack);
            reach(*slot.add(1), &mut stack);
        }
        while let Some(block) = stack.pop() {
            for &child in children(block) {
                reach(child, &mut stack);
            }
        }

        let blocks = self.blocks();
        self.check(&blocks, given);
        for &block in &blocks {
            if *block & MARK == 0 {
                let code = match *block & KIND {
                    CLOSURE | THUNK | WORKING => format!(", code at {:#x}", *block.add(1)),
                    _ => String::new(),
                };
                wrong(&format!(
                    "a block of kind {} and tag {}, counted {} times, is reached by nothing the program holds{}",
                    *block & KIND,
                    tag(*block),
                    references(*block),
                    code
                ));
            }
            *block &= !MARK;
        }

        self.since = 0;
        hazy_check_due = 0;
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
            refer(*thunk.add(1), "a definition");
        }
        for &(handler, resume) in &self.catching {
            refer(handler, "a catch");
            refer(resume, "a catch");
        }
        for &slot in self.wrapped.values() {
            refer(*slot, "a wrapper");
            refer(*slot.add(1), "a wrapper");
        }
        for &block in blocks {
            let counted = references(*block);
            let found = expected.get(&block).copied().unwrap_or(0);
            if counted != found {
                // For an object, say where its code is, to find it by.
                let code = match *block & KIND {
                    CLOSURE | THUNK | WORKING => format!(", code at {:#x}", *block.add(1)),
                    _ => String::new(),
                };
                wrong(&format!(
                    "a block of kind {} and tag {} is counted {} times and referred to {} times{}",
                    *block & KIND,
                    tag(*block),
                    counted,
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

/// One more reference to a value, for a primitive that keeps what it was
/// only lent, or gives back what something else still holds.
pub fn share(value: Value) -> Value {
    if let Some(block) = counted(value) {
        unsafe { *block += ONE };
    }
    value
}

/// A reference to a value is given up by a primitive that held it.
pub fn release(value: Value) {
    unsafe { heap().erase(value) }
}

/// What a catch is where C called into the program: the end of what can be
/// seen from there.
pub const BARRIER: Value = 0;

/// What catches a thrown value.
pub fn catching() -> &'static mut Vec<(Value, Value)> {
    &mut heap().catching
}

/// The functions of C that wrappers made.
pub fn wrapped() -> &'static mut HashMap<u64, *mut Value> {
    &mut heap().wrapped
}

/// A new block of some words with a header, and one reference to it.
#[no_mangle]
pub extern "C" fn hazy_alloc(size: u64, head: u64) -> *mut u64 {
    let heap = heap();
    let size = size as usize;
    unsafe {
        heap.settle(8);
        let block = heap.take(size);
        *block = head | ONE;
        hazy_allocated += 1;
        if let Some(every) = heap.sample {
            if hazy_allocated % every == 0 {
                let caller = caller();
                *heap.sampled.entry(caller).or_insert(0) += 1;
            }
        }
        if let Some(every) = heap.check {
            heap.since += 1;
            if heap.since >= every {
                hazy_check_due = 1;
            }
        }
        block
    }
}

/// The last reference to a block has been given up. It is taken apart now,
/// and so are a few of what it held that are no longer held either.
#[no_mangle]
pub extern "C" fn hazy_free(value: Value) {
    let block = value as *mut u64;
    unsafe {
        // A number holds nothing, and goes straight back.
        if leaf(*block) {
            give_leaf(block);
            return;
        }
        let heap = heap();
        heap.take_apart(block);
        heap.settle(8);
    }
}

/// The last reference to a block has been given up by someone who has read
/// what it holds and now owns that. Only the block's words are freed.
#[no_mangle]
pub extern "C" fn hazy_give(value: Value) {
    let heap = heap();
    unsafe { heap.give(value as *mut u64) };
    unsafe { hazy_freed += 1 };
}

/// A reference to a block is given up by someone who has read what it holds
/// and now owns that, while others still refer to the block. What it holds is
/// shared.
#[no_mangle]
pub extern "C" fn hazy_split(value: Value) {
    let block = value as *mut u64;
    unsafe {
        for &child in children(block) {
            if let Some(child) = counted(child) {
                *child += ONE;
            }
        }
        // After the children: a block that captures itself was just shared.
        *block -= ONE;
    }
}

/// A thunk has been given its value, by something that held a reference to
/// the thunk. A thunk at the top of the program is remembered, since its
/// value is alive from now on.
#[no_mangle]
pub extern "C" fn hazy_settle(thunk: Value) {
    let block = thunk as *mut u64;
    unsafe {
        if *block & IMMORTAL != 0 {
            heap().roots.push(block);
        } else {
            heap().erase(thunk);
        }
    }
}

/// Check every count of references, given some values, which are everything
/// the program holds.
#[no_mangle]
pub extern "C" fn hazy_check(given: *const Value, length: u64) {
    let given = if length == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(given, length as usize) }
    };
    unsafe { heap().examine(given) }
}

/// The function that called `hazy_alloc`, by name. There is no stack to
/// speak of, every call being a tail call, so finding it is cheap.
#[inline(never)]
fn caller() -> String {
    let trace = std::backtrace::Backtrace::force_capture().to_string();
    let names: Vec<&str> = trace
        .lines()
        .filter_map(|line| {
            let line = line.trim_start();
            let (number, name) = line.split_once(": ")?;
            number.parse::<u32>().ok()?;
            Some(name)
        })
        .collect();
    match names.iter().position(|name| name.contains("hazy_alloc")) {
        Some(at) => names.get(at + 1).unwrap_or(&"(nothing)").to_string(),
        None => "(not found)".to_string(),
    }
}

/// The program is over and holds nothing.
pub fn finish() {
    let heap = heap();
    if heap.sample.is_some() {
        let mut sampled: Vec<(&String, &u64)> = heap.sampled.iter().collect();
        sampled.sort_by(|left, right| right.1.cmp(left.1));
        let total: u64 = heap.sampled.values().sum();
        eprintln!("allocations, a sample of {total}:");
        for (name, count) in sampled.into_iter().take(60) {
            eprintln!("{:6.2}% {count:8} {name}", 100.0 * *count as f64 / total as f64);
        }
    }
    if heap.check.is_some() {
        unsafe { heap.examine(&[]) };
    }
    if heap.report {
        eprintln!(
            "allocated: {}, reused: {}, lent: {}, freed: {}",
            unsafe { hazy_allocated },
            unsafe { hazy_reused },
            unsafe { hazy_lent },
            unsafe { hazy_freed }
        );
    }
}
