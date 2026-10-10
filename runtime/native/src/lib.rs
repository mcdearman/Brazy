//! The runtime of natively compiled Hazy programs.
//!
//! The compiler emits LLVM IR that does all of the control flow itself: every
//! call between pieces of a program is a tail call, which Rust cannot make.
//! What is here is what that code calls out to and comes back from: the heap,
//! in `heap`, and the primitives of `Sequent.Primitive`.
//!
//! # Values
//!
//! A value is one 64-bit word.
//!
//! * An odd word is an `Int` or a `Char`: the number shifted left by one,
//!   with the low bit set.
//! * An even word is the address of a block. See `heap` for what a block is.
//!
//! A primitive borrows its arguments: the program still owns them afterwards
//! and gives them up itself. What a primitive gives back is the program's.

mod heap;

use heap::{hazy_alloc, header, Value, DATA, INTEGER, TEXT};
use num_bigint::BigInt;
use num_traits::{Signed, ToPrimitive, Zero};
use std::io::Write;
use unicode_properties::{GeneralCategory, UnicodeGeneralCategory};

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
    hazy_alloc(2, header(DATA, tag, 0)) as Value
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
    heap::finish();
}

// Text ------------------------------------------------------------------------

fn text(value: Value) -> &'static str {
    debug_assert!(kind(value) == TEXT, "not a Text");
    unsafe {
        let block = value as *const u64;
        let length = *block.add(2) as usize;
        let bytes = std::slice::from_raw_parts(block.add(3) as *const u8, length);
        std::str::from_utf8_unchecked(bytes)
    }
}

fn from_text(string: &str) -> Value {
    let length = string.len();
    let block = hazy_alloc(3 + (length as u64 + 7) / 8, header(TEXT, 0, 0));
    unsafe {
        *block.add(2) = length as u64;
        std::ptr::copy_nonoverlapping(string.as_ptr(), block.add(3) as *mut u8, length);
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

/// The general category of a character, as a constructor of
/// `GeneralCategory`, whose constructors are in the order of the Haskell
/// report.
#[no_mangle]
pub extern "C" fn hazy_prim_general_category(character: Value) -> Value {
    use GeneralCategory::*;
    let tag = match char::from_u32(int(character) as u32) {
        // Half of a surrogate pair is not a character to Rust.
        None => 27,
        Some(character) => match character.general_category() {
            UppercaseLetter => 0,
            LowercaseLetter => 1,
            TitlecaseLetter => 2,
            ModifierLetter => 3,
            OtherLetter => 4,
            NonspacingMark => 5,
            SpacingMark => 6,
            EnclosingMark => 7,
            DecimalNumber => 8,
            LetterNumber => 9,
            OtherNumber => 10,
            ConnectorPunctuation => 11,
            DashPunctuation => 12,
            OpenPunctuation => 13,
            ClosePunctuation => 14,
            InitialPunctuation => 15,
            FinalPunctuation => 16,
            OtherPunctuation => 17,
            MathSymbol => 18,
            CurrencySymbol => 19,
            ModifierSymbol => 20,
            OtherSymbol => 21,
            SpaceSeparator => 22,
            LineSeparator => 23,
            ParagraphSeparator => 24,
            Control => 25,
            Format => 26,
            Surrogate => 27,
            PrivateUse => 28,
            Unassigned => 29,
        },
    };
    nullary(tag)
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
// An Integer is a number of any size. The number itself is kept by Rust and
// the block holds its address. The number is freed with the block.

fn integer(value: Value) -> &'static BigInt {
    debug_assert!(kind(value) == INTEGER, "not an Integer");
    unsafe { &*(*(value as *const u64).add(2) as *const BigInt) }
}

fn from_integer(number: BigInt) -> Value {
    let block = hazy_alloc(3, header(INTEGER, 0, 0));
    unsafe { *block.add(2) = Box::into_raw(Box::new(number)) as u64 };
    block as Value
}

/// An `Integer` written in the program.
#[no_mangle]
pub extern "C" fn hazy_integer(number: i64) -> Value {
    from_integer(BigInt::from(number))
}

/// An `Integer` written in the program that does not fit in 64 bits, as its
/// decimal digits.
#[no_mangle]
pub extern "C" fn hazy_integer_big(digits: *const u8, length: u64) -> Value {
    let digits = unsafe { std::slice::from_raw_parts(digits, length as usize) };
    match BigInt::parse_bytes(digits, 10) {
        Some(number) => from_integer(number),
        None => fail("an Integer in the program is not a number"),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_int_to_integer(value: Value) -> Value {
    from_integer(BigInt::from(int(value)))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_cast_to_int(value: Value) -> Value {
    match integer(value).to_i32() {
        Some(number) => from_int(number),
        None => fail("Enum Integer Overflow"),
    }
}

/// The low 32 bits of an `Integer`, as two's complement.
#[no_mangle]
pub extern "C" fn hazy_prim_integer_truncate_to_int(value: Value) -> Value {
    let number = integer(value);
    let low = number.iter_u32_digits().next().unwrap_or(0);
    let low = if number.is_negative() { low.wrapping_neg() } else { low };
    from_int(low as i32)
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
    from_integer(integer(left) + integer(right))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_minus(left: Value, right: Value) -> Value {
    from_integer(integer(left) - integer(right))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_multiply(left: Value, right: Value) -> Value {
    from_integer(integer(left) * integer(right))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_negate(value: Value) -> Value {
    from_integer(-integer(value))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_abs(value: Value) -> Value {
    from_integer(integer(value).abs())
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_signum(value: Value) -> Value {
    from_integer(integer(value).signum())
}

/// Division that rounds toward zero.
#[no_mangle]
pub extern "C" fn hazy_prim_integer_quot(left: Value, right: Value) -> Value {
    if integer(right).is_zero() {
        fail("divide by zero")
    }
    from_integer(integer(left) / integer(right))
}

/// The remainder of division that rounds toward zero.
#[no_mangle]
pub extern "C" fn hazy_prim_integer_rem(left: Value, right: Value) -> Value {
    if integer(right).is_zero() {
        fail("divide by zero")
    }
    from_integer(integer(left) % integer(right))
}
