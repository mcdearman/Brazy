//! The runtime of natively compiled Hazy programs.
//!
//! The compiler emits LLVM IR that does all of the control flow itself: every
//! call between pieces of a program is a tail call, which Rust cannot make.
//! What is here is what that code calls out to and comes back from: the
//! allocator and the primitives of `Sequent.Primitive`.
//!
//! # Values
//!
//! A value is one 64-bit word.
//!
//! * An odd word is an `Int` or a `Char`: the number shifted left by one,
//!   with the low bit set.
//! * An even word is the address of a block. The first word of a block is
//!   its header: the kind in the low 8 bits, a constructor's tag in the next
//!   24, and the number of fields in the upper 32.
//!
//! These layouts are also written in `compiler/LLVM/FromAxCut.hs`. The two
//! must agree.
//!
//! Nothing is freed yet. Counting references is a later milestone.

use std::io::Write;

/// A constructed value: the header and then its fields.
pub const DATA: u64 = 0;
/// A function or a consumer: the header, its code and what it captured.
pub const CLOSURE: u64 = 1;
/// A thunk that has not been forced: the header, its code and what it captured.
pub const THUNK: u64 = 2;
/// A thunk that is being forced.
pub const WORKING: u64 = 3;
/// A thunk that has been forced: the header and its value.
pub const DONE: u64 = 4;
/// An `Integer`: the header and the number.
pub const INTEGER: u64 = 5;
/// A `Text`: the header, its length in bytes and then the bytes, as UTF-8.
pub const TEXT: u64 = 6;

type Value = u64;

const CHUNK: usize = 1 << 20;

struct Heap {
    next: *mut u64,
    left: usize,
}

static mut HEAP: Heap = Heap {
    next: std::ptr::null_mut(),
    left: 0,
};

/// A block of some words. Nothing is ever given back.
#[no_mangle]
pub extern "C" fn hazy_alloc(words: u64) -> *mut u64 {
    let words = words as usize;
    // The program has one thread, and this is the only place the heap is used.
    unsafe {
        let heap = &mut *std::ptr::addr_of_mut!(HEAP);
        if heap.left < words {
            let size = words.max(CHUNK);
            let chunk: Vec<u64> = Vec::with_capacity(size);
            heap.next = chunk.leak().as_mut_ptr();
            heap.left = size;
        }
        let block = heap.next;
        heap.next = heap.next.add(words);
        heap.left -= words;
        block
    }
}

fn header(kind: u64, tag: u64, count: u64) -> u64 {
    kind | tag << 8 | count << 32
}

fn kind(block: Value) -> u64 {
    unsafe { *(block as *const u64) & 0xff }
}

fn int(value: Value) -> i32 {
    debug_assert!(value & 1 == 1, "not an Int");
    ((value as i64) >> 1) as i32
}

fn from_int(number: i32) -> Value {
    (((number as i64) << 1) | 1) as u64
}

/// A constructor with no fields: `False`, `True`, `()` and the like.
fn nullary(tag: u64) -> Value {
    let block = hazy_alloc(1);
    unsafe { *block = header(DATA, tag, 0) };
    block as Value
}

fn boolean(value: bool) -> Value {
    nullary(value as u64)
}

fn fail(message: &str) -> ! {
    let _ = std::io::stdout().flush();
    eprintln!("{message}");
    std::process::exit(1)
}

/// No pattern matched.
#[no_mangle]
pub extern "C" fn hazy_abort() -> ! {
    fail("bottom")
}

/// A thunk was forced while it was being forced.
#[no_mangle]
pub extern "C" fn hazy_loop() -> ! {
    fail("<<loop>>")
}

/// The program is over.
#[no_mangle]
pub extern "C" fn hazy_finish() {
    let _ = std::io::stdout().flush();
}

// Text ------------------------------------------------------------------------

fn text(value: Value) -> &'static str {
    debug_assert!(kind(value) == TEXT, "not a Text");
    unsafe {
        let block = value as *const u64;
        let length = *block.add(1) as usize;
        let bytes = std::slice::from_raw_parts(block.add(2) as *const u8, length);
        std::str::from_utf8_unchecked(bytes)
    }
}

fn from_text(string: &str) -> Value {
    let length = string.len();
    let block = hazy_alloc(2 + (length as u64 + 7) / 8);
    unsafe {
        *block = header(TEXT, 0, 0);
        *block.add(1) = length as u64;
        std::ptr::copy_nonoverlapping(string.as_ptr(), block.add(2) as *mut u8, length);
    }
    block as Value
}

/// A `Text` from bytes in the program, which are UTF-8.
#[no_mangle]
pub extern "C" fn hazy_text(bytes: *const u8, length: u64) -> Value {
    let bytes = unsafe { std::slice::from_raw_parts(bytes, length as usize) };
    from_text(unsafe { std::str::from_utf8_unchecked(bytes) })
}

#[no_mangle]
pub extern "C" fn hazy_prim_error(message: Value) -> Value {
    fail(text(message))
}

#[no_mangle]
pub extern "C" fn hazy_prim_put_str_ln(value: Value) -> Value {
    let mut out = std::io::stdout().lock();
    let _ = out.write_all(text(value).as_bytes());
    let _ = out.write_all(b"\n");
    nullary(0)
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_snoc(value: Value, character: Value) -> Value {
    // A code point that is not a character, such as half of a surrogate
    // pair, becomes the replacement character.
    let character = char::from_u32(int(character) as u32).unwrap_or('\u{fffd}');
    let mut longer = String::with_capacity(text(value).len() + 4);
    longer.push_str(text(value));
    longer.push(character);
    from_text(&longer)
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_null(value: Value) -> Value {
    boolean(text(value).is_empty())
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_head(value: Value) -> Value {
    match text(value).chars().next() {
        Some(character) => from_int(character as i32),
        None => fail("head of an empty Text"),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_tail(value: Value) -> Value {
    let mut characters = text(value).chars();
    match characters.next() {
        Some(_) => from_text(characters.as_str()),
        None => fail("tail of an empty Text"),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_general_category(_character: Value) -> Value {
    fail("generalCategory is not in the native runtime yet")
}

// Int -------------------------------------------------------------------------

#[no_mangle]
pub extern "C" fn hazy_prim_int_equal(left: Value, right: Value) -> Value {
    boolean(int(left) == int(right))
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_less_then_equal(left: Value, right: Value) -> Value {
    boolean(int(left) <= int(right))
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_add(left: Value, right: Value) -> Value {
    from_int(int(left).wrapping_add(int(right)))
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_minus(left: Value, right: Value) -> Value {
    from_int(int(left).wrapping_sub(int(right)))
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_multiply(left: Value, right: Value) -> Value {
    from_int(int(left).wrapping_mul(int(right)))
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_negate(value: Value) -> Value {
    from_int(int(value).wrapping_neg())
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_abs(value: Value) -> Value {
    from_int(int(value).wrapping_abs())
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_signum(value: Value) -> Value {
    from_int(int(value).signum())
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_quot(left: Value, right: Value) -> Value {
    if int(right) == 0 {
        fail("divide by zero")
    }
    from_int(int(left).wrapping_div(int(right)))
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_rem(left: Value, right: Value) -> Value {
    if int(right) == 0 {
        fail("divide by zero")
    }
    from_int(int(left).wrapping_rem(int(right)))
}

// Integer ---------------------------------------------------------------------
//
// An Integer is held in 64 bits for now, and the program stops if one does
// not fit. Numbers of any size are a later step.

fn integer(value: Value) -> i64 {
    debug_assert!(kind(value) == INTEGER, "not an Integer");
    unsafe { *(value as *const i64).add(1) }
}

fn from_integer(number: i64) -> Value {
    let block = hazy_alloc(2);
    unsafe {
        *block = header(INTEGER, 0, 0);
        *(block as *mut i64).add(1) = number;
    }
    block as Value
}

fn fits(number: Option<i64>) -> Value {
    match number {
        Some(number) => from_integer(number),
        None => fail("an Integer does not fit in 64 bits, which is all the native runtime has yet"),
    }
}

/// An `Integer` written in the program.
#[no_mangle]
pub extern "C" fn hazy_integer(number: i64) -> Value {
    from_integer(number)
}

/// An `Integer` written in the program that does not fit in 64 bits.
#[no_mangle]
pub extern "C" fn hazy_integer_big(_digits: *const u8, _length: u64) -> Value {
    fits(None)
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_to_integer(value: Value) -> Value {
    from_integer(int(value) as i64)
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_cast_to_int(value: Value) -> Value {
    match i32::try_from(integer(value)) {
        Ok(number) => from_int(number),
        Err(_) => fail("Enum Integer Overflow"),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_truncate_to_int(value: Value) -> Value {
    from_int(integer(value) as i32)
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_equal(left: Value, right: Value) -> Value {
    boolean(integer(left) == integer(right))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_less_then_equal(left: Value, right: Value) -> Value {
    boolean(integer(left) <= integer(right))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_add(left: Value, right: Value) -> Value {
    fits(integer(left).checked_add(integer(right)))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_minus(left: Value, right: Value) -> Value {
    fits(integer(left).checked_sub(integer(right)))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_multiply(left: Value, right: Value) -> Value {
    fits(integer(left).checked_mul(integer(right)))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_negate(value: Value) -> Value {
    fits(integer(value).checked_neg())
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_abs(value: Value) -> Value {
    fits(integer(value).checked_abs())
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_signum(value: Value) -> Value {
    from_integer(integer(value).signum())
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_quot(left: Value, right: Value) -> Value {
    if integer(right) == 0 {
        fail("divide by zero")
    }
    fits(integer(left).checked_div(integer(right)))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_rem(left: Value, right: Value) -> Value {
    if integer(right) == 0 {
        fail("divide by zero")
    }
    fits(integer(left).checked_rem(integer(right)))
}
