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

mod directory;
mod heap;

use heap::{hazy_alloc, header, nullary, Value, ARRAY, DATA, FLOAT, INTEGER, SLICE, TEXT};
use num_bigint::{BigInt, Sign};
use num_traits::{FromPrimitive, One, Signed, ToPrimitive, Zero};
use std::borrow::Cow;
use std::io::Write;
use unicode_properties::{GeneralCategory, UnicodeGeneralCategory};

fn kind(block: Value) -> u64 {
    unsafe { *(block as *const u64) & 0xf }
}

/// An Int is 63 bits, kept above a 1 in the word: so arithmetic on the
/// word's 64 bits and then this comes round at the Int's bounds.
fn int(value: Value) -> i64 {
    debug_assert!(value & 1 == 1, "not an Int");
    (value as i64) >> 1
}

fn from_int(number: impl Into<i64>) -> Value {
    (number.into().wrapping_shl(1) | 1) as u64
}

/// The bounds of an Int.
const INT_MIN: i64 = -(1 << 62);
const INT_MAX: i64 = (1 << 62) - 1;

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
    unsafe {
        let block = value as *const u64;
        if kind(value) == SLICE {
            let whole = *block.add(1) as *const u64;
            let start = *block.add(2) as usize;
            let length = *block.add(3) as usize;
            let bytes = std::slice::from_raw_parts((whole.add(2) as *const u8).add(start), length);
            return std::str::from_utf8_unchecked(bytes);
        }
        debug_assert!(kind(value) == TEXT, "not a Text");
        let length = *block.add(1) as usize;
        let bytes = std::slice::from_raw_parts(block.add(2) as *const u8, length);
        std::str::from_utf8_unchecked(bytes)
    }
}

/// A part shorter than this is copied. A longer one shares the text it is
/// part of, which is kept for as long as the part is: so the rest of a long
/// text is had without going over it, and a word taken from a file does not
/// keep the file.
const SHARED: usize = 256;

/// The bytes of a text from one place to another, as a text.
fn part(value: Value, from: usize, to: usize) -> Value {
    let string = text(value);
    if to - from < SHARED {
        return from_text(&string[from..to]);
    }
    let (whole, start) = unsafe {
        let block = value as *const u64;
        if kind(value) == SLICE {
            (*block.add(1), *block.add(2) as usize)
        } else {
            (value, 0)
        }
    };
    let block = hazy_alloc(4, header(SLICE, 0, 0));
    unsafe {
        *block.add(1) = heap::share(whole);
        *block.add(2) = (start + from) as u64;
        *block.add(3) = (to - from) as u64;
    }
    block as Value
}

fn from_text(string: &str) -> Value {
    let length = string.len();
    let block = hazy_alloc(2 + (length as u64 + 7) / 8, header(TEXT, 0, 0));
    unsafe {
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
        Some(character) => from_int(character as i64),
        None => fail("head of an empty Text"),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_tail(value: Value) -> Value {
    let string = text(value);
    match string.chars().next() {
        Some(first) => part(value, first.len_utf8(), string.len()),
        None => fail("tail of an empty Text"),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_append(left: Value, right: Value) -> Value {
    let mut longer = String::with_capacity(text(left).len() + text(right).len());
    longer.push_str(text(left));
    longer.push_str(text(right));
    from_text(&longer)
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_equal(left: Value, right: Value) -> Value {
    boolean(text(left) == text(right))
}

/// Which of two texts comes first. The bytes of UTF-8 are in the order of
/// the characters they stand for.
#[no_mangle]
pub extern "C" fn hazy_prim_text_compare(left: Value, right: Value) -> Value {
    from_int(match text(left).cmp(text(right)) {
        std::cmp::Ordering::Less => -1,
        std::cmp::Ordering::Equal => 0,
        std::cmp::Ordering::Greater => 1,
    })
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_length(value: Value) -> Value {
    from_int(text(value).chars().count() as i64)
}

/// Where in a text so many characters end, in bytes.
fn after(string: &str, count: i64) -> usize {
    if count <= 0 {
        return 0;
    }
    match string.char_indices().nth(count as usize) {
        Some((index, _)) => index,
        None => string.len(),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_take(count: Value, value: Value) -> Value {
    let string = text(value);
    part(value, 0, after(string, int(count)))
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_drop(count: Value, value: Value) -> Value {
    let string = text(value);
    part(value, after(string, int(count)), string.len())
}

#[no_mangle]
pub extern "C" fn hazy_prim_text_is_prefix_of(prefix: Value, value: Value) -> Value {
    boolean(text(value).starts_with(text(prefix)))
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

/// Division that rounds down.
#[no_mangle]
pub extern "C" fn hazy_prim_int_div(left: Value, right: Value) -> Value {
    if int(right) == 0 {
        fail("divide by zero")
    }
    let (quotient, remainder) = (int(left).wrapping_div(int(right)), int(left).wrapping_rem(int(right)));
    from_int(if remainder != 0 && (remainder < 0) != (int(right) < 0) { quotient - 1 } else { quotient })
}

/// What is left over from division that rounds down, of the divisor's sign.
#[no_mangle]
pub extern "C" fn hazy_prim_int_mod(left: Value, right: Value) -> Value {
    if int(right) == 0 {
        fail("divide by zero")
    }
    let remainder = int(left).wrapping_rem(int(right));
    from_int(if remainder != 0 && (remainder < 0) != (int(right) < 0) { remainder + int(right) } else { remainder })
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

/// An `Integer` that fits in 64 bits is kept in its block, whose tag says
/// so. A bigger one is kept apart, and its block points at it. Most are
/// small, and are worked out without ever being made big.
fn small(value: Value) -> Option<i64> {
    debug_assert!(kind(value) == INTEGER, "not an Integer");
    unsafe {
        let block = value as *const u64;
        if heap::tag(*block) == heap::SMALL_INTEGER {
            Some(*block.add(1) as i64)
        } else {
            None
        }
    }
}

fn integer(value: Value) -> Cow<'static, BigInt> {
    match small(value) {
        Some(number) => Cow::Owned(BigInt::from(number)),
        None => unsafe { Cow::Borrowed(&*(*(value as *const u64).add(1) as *const BigInt)) },
    }
}

fn from_small(number: i64) -> Value {
    let block = hazy_alloc(2, header(INTEGER, heap::SMALL_INTEGER, 0));
    unsafe { *block.add(1) = number as u64 };
    block as Value
}

fn from_integer(number: BigInt) -> Value {
    if let Some(number) = number.to_i64() {
        return from_small(number);
    }
    let block = hazy_alloc(2, header(INTEGER, 0, 0));
    unsafe { *block.add(1) = Box::into_raw(Box::new(number)) as u64 };
    block as Value
}

/// Two small `Integer`s, if they are.
fn smalls(left: Value, right: Value) -> Option<(i64, i64)> {
    Some((small(left)?, small(right)?))
}

/// An `Integer` written in the program.
#[no_mangle]
pub extern "C" fn hazy_integer(number: i64) -> Value {
    from_small(number)
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
    from_small(int(value))
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_cast_to_int(value: Value) -> Value {
    match small(value).or_else(|| integer(value).to_i64()).filter(|number| (INT_MIN..=INT_MAX).contains(number)) {
        Some(number) => from_int(number),
        None => fail("Enum Integer Overflow"),
    }
}

/// The low 32 bits of an `Integer`, as two's complement.
#[no_mangle]
pub extern "C" fn hazy_prim_integer_truncate_to_int(value: Value) -> Value {
    if let Some(number) = small(value) {
        return from_int(number);
    }
    let number = integer(value);
    let low = number.iter_u64_digits().next().unwrap_or(0);
    let low = if number.is_negative() { low.wrapping_neg() } else { low };
    from_int(low as i64)
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_equal(left: Value, right: Value) -> Value {
    match smalls(left, right) {
        Some((left, right)) => boolean(left == right),
        None => boolean(integer(left) == integer(right)),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_less_then_equal(left: Value, right: Value) -> Value {
    match smalls(left, right) {
        Some((left, right)) => boolean(left <= right),
        None => boolean(integer(left) <= integer(right)),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_add(left: Value, right: Value) -> Value {
    match smalls(left, right).and_then(|(left, right)| left.checked_add(right)) {
        Some(number) => from_small(number),
        None => from_integer(&*integer(left) + &*integer(right)),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_minus(left: Value, right: Value) -> Value {
    match smalls(left, right).and_then(|(left, right)| left.checked_sub(right)) {
        Some(number) => from_small(number),
        None => from_integer(&*integer(left) - &*integer(right)),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_multiply(left: Value, right: Value) -> Value {
    match smalls(left, right).and_then(|(left, right)| left.checked_mul(right)) {
        Some(number) => from_small(number),
        None => from_integer(&*integer(left) * &*integer(right)),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_negate(value: Value) -> Value {
    match small(value).and_then(i64::checked_neg) {
        Some(number) => from_small(number),
        None => from_integer(-&*integer(value)),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_abs(value: Value) -> Value {
    match small(value).and_then(i64::checked_abs) {
        Some(number) => from_small(number),
        None => from_integer(integer(value).abs()),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_integer_signum(value: Value) -> Value {
    match small(value) {
        Some(number) => from_small(number.signum()),
        None => from_integer(integer(value).signum()),
    }
}

/// Division that rounds toward zero.
#[no_mangle]
pub extern "C" fn hazy_prim_integer_quot(left: Value, right: Value) -> Value {
    if integer(right).is_zero() {
        fail("divide by zero")
    }
    match smalls(left, right).and_then(|(left, right)| left.checked_div(right)) {
        Some(number) => from_small(number),
        None => from_integer(&*integer(left) / &*integer(right)),
    }
}

/// The remainder of division that rounds toward zero.
#[no_mangle]
pub extern "C" fn hazy_prim_integer_rem(left: Value, right: Value) -> Value {
    if integer(right).is_zero() {
        fail("divide by zero")
    }
    match smalls(left, right).and_then(|(left, right)| left.checked_rem(right)) {
        Some(number) => from_small(number),
        None => from_integer(&*integer(left) % &*integer(right)),
    }
}

/// The bits of two numbers put together: both, either or one, as the
/// operation is 0, 1 or 2. A negative number goes on with ones for ever.
#[no_mangle]
pub extern "C" fn hazy_prim_integer_bits(operation: Value, left: Value, right: Value) -> Value {
    let (left, right) = (integer(left), integer(right));
    from_integer(match int(operation) {
        0 => &*left & &*right,
        1 => &*left | &*right,
        _ => &*left ^ &*right,
    })
}

/// A number's bits moved toward the high end by so many places, or toward
/// the low end if that is negative, where what goes past the end is gone.
#[no_mangle]
pub extern "C" fn hazy_prim_integer_shift(number: Value, places: Value) -> Value {
    let places = int(places);
    let number = integer(number);
    from_integer(if places >= 0 {
        &*number << places as u32
    } else {
        // Toward negative infinity, as an arithmetic shift is.
        &*number >> places.unsigned_abs()
    })
}

/// How many bits are ones; of a negative number, the negative of how many
/// its magnitude has, as GHC has it.
#[no_mangle]
pub extern "C" fn hazy_prim_integer_pop_count(number: Value) -> Value {
    let number = integer(number);
    let count = number.magnitude().count_ones() as i32;
    from_int(if number.is_negative() { -count } else { count })
}

// The world outside ---------------------------------------------------------
//
// These are the primitives that `Sequent.Primitive` calls foreign: each is
// named there and called by that name with `hazy_prim_` before it. A
// primitive borrows what it is given, so one that keeps a value takes a
// reference of its own to it.

/// A file that is open: what it was opened as, what reads it, which keeps
/// what it has read ahead of what the program has taken, and whether it may
/// be read and written.
struct Open {
    path: String,
    reader: std::io::BufReader<std::fs::File>,
    readable: bool,
    writable: bool,
}

/// The files that are open. A file is known to a program by where it is
/// here, after the three every program has.
static mut FILES: Vec<Option<Open>> = Vec::new();

/// What the system said of the last file that could not be opened.
static mut FAILURE: String = String::new();

fn files() -> &'static mut Vec<Option<Open>> {
    // The program has one thread.
    unsafe { &mut *std::ptr::addr_of_mut!(FILES) }
}

fn file(descriptor: i64) -> &'static mut Open {
    match files().get_mut((descriptor - 3) as usize) {
        Some(Some(file)) => file,
        _ => fail(&format!("file {descriptor} is not open")),
    }
}

/// Read from standard input or from a file that was opened, through what
/// keeps what was read ahead.
fn reading<T>(
    descriptor: i64,
    read: impl FnOnce(&mut dyn std::io::BufRead) -> std::io::Result<T>,
) -> T {
    let result = match descriptor {
        0 => read(&mut std::io::stdin().lock()),
        1 | 2 => fail(&format!("file {descriptor} is not open to read")),
        descriptor => read(&mut file(descriptor).reader),
    };
    match result {
        Ok(value) => value,
        Err(error) => fail(&format!("could not read: {error}")),
    }
}

/// What kind of failure the system's is, as `Hazy.PreludeIO` numbers them.
/// They are told apart as GHC tells them apart.
fn failure(error: &std::io::Error) -> i64 {
    use std::io::ErrorKind::*;
    match error.kind() {
        AlreadyExists => 1,
        NotFound | NotADirectory => 2,
        ResourceBusy => 3,
        StorageFull => 4,
        PermissionDenied | ReadOnlyFilesystem => 7,
        IsADirectory => 8,
        InvalidInput => 9,
        _ => 0,
    }
}

/// What the system says of a failure, without the number it knows it by.
fn description(error: &std::io::Error) -> String {
    let said = error.to_string();
    match said.find(" (os error ") {
        Some(end) => said[..end].to_string(),
        None => said,
    }
}

fn unit() -> Value {
    nullary(0)
}

/// A place that holds a value: a block of one field, which is changed where
/// it is.
#[no_mangle]
pub extern "C" fn hazy_prim_new_ref(value: Value) -> Value {
    let block = hazy_alloc(2, header(DATA, 0, 1));
    unsafe { *block.add(1) = heap::share(value) };
    block as Value
}

#[no_mangle]
pub extern "C" fn hazy_prim_read_ref(reference: Value) -> Value {
    heap::share(unsafe { *(reference as *const u64).add(1) })
}

#[no_mangle]
pub extern "C" fn hazy_prim_write_ref(reference: Value, value: Value) -> Value {
    let place = unsafe { (reference as *mut u64).add(1) };
    let old = unsafe { *place };
    unsafe { *place = heap::share(value) };
    heap::release(old);
    unit()
}

#[no_mangle]
pub extern "C" fn hazy_prim_same_ref(left: Value, right: Value) -> Value {
    boolean(left == right)
}

// Floating point ---------------------------------------------------------------
//
// A number is a block of the bits of a double. A Float is worked out in
// single precision and kept as the double it is exactly, so each of these
// is told which it is: 32 for a Float, 64 for a Double.

fn floating(value: Value) -> f64 {
    f64::from_bits(unsafe { *(value as *const u64).add(1) })
}

/// A `Double` or a `Float` as a function of C is given it.
#[no_mangle]
pub extern "C" fn hazy_ffi_double(value: Value) -> f64 {
    floating(value)
}

/// What a function of C gives as a `Double` or a `Float`, as a value.
#[no_mangle]
pub extern "C" fn hazy_ffi_from_double(number: f64) -> Value {
    from_floating(number)
}

/// A `Bool` as a function of C is given it: an `int` that is 1 for `True`.
#[no_mangle]
pub extern "C" fn hazy_ffi_bool(value: Value) -> i32 {
    (value == nullary(1)) as i32
}

/// What a function of C gives as a `Bool`, as a value: anything but 0 is
/// `True`.
#[no_mangle]
pub extern "C" fn hazy_ffi_from_bool(truth: i32) -> Value {
    boolean(truth != 0)
}

/// An `Integer` as a function of C is given it, as a number of a fixed
/// width: what it is modulo 2^64.
#[no_mangle]
pub extern "C" fn hazy_ffi_integer(value: Value) -> i64 {
    if let Some(number) = small(value) {
        return number;
    }
    let number = integer(value);
    let low = number.iter_u64_digits().next().unwrap_or(0) as i64;
    if number.sign() == Sign::Minus {
        low.wrapping_neg()
    } else {
        low
    }
}

/// What a function of C gives as a number of 64 bits with no sign, as an
/// `Integer`.
#[no_mangle]
pub extern "C" fn hazy_ffi_from_unsigned(number: u64) -> Value {
    from_integer(BigInt::from(number))
}

/// Reading and writing memory, which the library's Foreign.Storable does
/// through these as functions of C.
macro_rules! memory {
    ($peek:ident, $poke:ident, $type:ty) => {
        #[no_mangle]
        pub unsafe extern "C" fn $peek(address: *const $type) -> $type {
            address.read_unaligned()
        }

        #[no_mangle]
        pub unsafe extern "C" fn $poke(address: *mut $type, value: $type) {
            address.write_unaligned(value)
        }
    };
}

memory!(hazy_peek_i8, hazy_poke_i8, i8);
memory!(hazy_peek_u8, hazy_poke_u8, u8);
memory!(hazy_peek_i16, hazy_poke_i16, i16);
memory!(hazy_peek_u16, hazy_poke_u16, u16);
memory!(hazy_peek_i32, hazy_poke_i32, i32);
memory!(hazy_peek_u32, hazy_poke_u32, u32);
memory!(hazy_peek_i64, hazy_poke_i64, i64);
memory!(hazy_peek_u64, hazy_poke_u64, u64);
memory!(hazy_peek_f32, hazy_poke_f32, f32);
memory!(hazy_peek_f64, hazy_poke_f64, f64);
memory!(hazy_peek_ptr, hazy_poke_ptr, *mut u8);

// Where C keeps errno, which each system names its own way.
extern "C" {
    #[cfg_attr(target_os = "linux", link_name = "__errno_location")]
    #[cfg_attr(target_os = "windows", link_name = "_errno")]
    #[cfg_attr(target_os = "macos", link_name = "__error")]
    fn errno_location() -> *mut i32;
}

/// What C says went wrong last, which the library's Foreign.C.Error reads.
#[no_mangle]
pub extern "C" fn hazy_errno() -> i32 {
    unsafe { *errno_location() }
}

#[no_mangle]
pub extern "C" fn hazy_set_errno(number: i32) {
    unsafe { *errno_location() = number }
}

/// The bits of a floating-point number, and the number of some bits, which
/// the library's GHC.Float has as functions of C.
#[no_mangle]
pub extern "C" fn hazy_cast_u32_f32(bits: u32) -> f32 {
    f32::from_bits(bits)
}

#[no_mangle]
pub extern "C" fn hazy_cast_f32_u32(number: f32) -> u32 {
    number.to_bits()
}

#[no_mangle]
pub extern "C" fn hazy_cast_u64_f64(bits: u64) -> f64 {
    f64::from_bits(bits)
}

#[no_mangle]
pub extern "C" fn hazy_cast_f64_u64(number: f64) -> u64 {
    number.to_bits()
}

/// What a function of C that gives nothing gives, as a value.
#[no_mangle]
pub extern "C" fn hazy_ffi_unit() -> Value {
    unit()
}

fn from_floating(number: f64) -> Value {
    let block = hazy_alloc(2, header(FLOAT, 0, 0));
    unsafe { *block.add(1) = number.to_bits() };
    block as Value
}

fn single(precision: Value) -> bool {
    int(precision) == 32
}

#[no_mangle]
pub extern "C" fn hazy_prim_float_from_integer(precision: Value, number: Value) -> Value {
    let number = integer(number);
    from_floating(if single(precision) {
        number.to_f32().map(f64::from).unwrap_or(f64::NAN)
    } else {
        number.to_f64().unwrap_or(f64::NAN)
    })
}

#[no_mangle]
pub extern "C" fn hazy_prim_float_unary(precision: Value, operation: Value, value: Value) -> Value {
    let x = floating(value);
    let operation = int(operation);
    macro_rules! apply {
        ($function:ident) => {
            if single(precision) {
                f64::from((x as f32).$function())
            } else {
                x.$function()
            }
        };
    }
    from_floating(match operation {
        0 => -x,
        1 => x.abs(),
        2 => {
            if x > 0.0 {
                1.0
            } else if x < 0.0 {
                -1.0
            } else {
                x
            }
        }
        3 => apply!(exp),
        4 => apply!(ln),
        5 => apply!(sqrt),
        6 => apply!(sin),
        7 => apply!(cos),
        8 => apply!(tan),
        9 => apply!(asin),
        10 => apply!(acos),
        11 => apply!(atan),
        12 => apply!(sinh),
        13 => apply!(cosh),
        14 => apply!(tanh),
        15 => apply!(asinh),
        16 => apply!(acosh),
        17 => apply!(atanh),
        _ => fail(&format!("no such function of floating point: {operation}")),
    })
}

#[no_mangle]
pub extern "C" fn hazy_prim_float_binary(precision: Value, operation: Value, left: Value, right: Value) -> Value {
    let (x, y) = (floating(left), floating(right));
    let operation = int(operation);
    from_floating(if single(precision) {
        let (x, y) = (x as f32, y as f32);
        f64::from(match operation {
            0 => x + y,
            1 => x - y,
            2 => x * y,
            3 => x / y,
            4 => x.powf(y),
            _ => fail(&format!("no such operator of floating point: {operation}")),
        })
    } else {
        match operation {
            0 => x + y,
            1 => x - y,
            2 => x * y,
            3 => x / y,
            4 => x.powf(y),
            _ => fail(&format!("no such operator of floating point: {operation}")),
        }
    })
}

/// 0 if the first is less, 1 if they are equal, 2 if it is greater, and 3
/// if they are not in order, as with a number that is not one.
#[no_mangle]
pub extern "C" fn hazy_prim_float_compare(left: Value, right: Value) -> Value {
    let (x, y) = (floating(left), floating(right));
    from_int(if x < y {
        0
    } else if x == y {
        1
    } else if x > y {
        2
    } else {
        3
    })
}

#[no_mangle]
pub extern "C" fn hazy_prim_float_test(precision: Value, test: Value, value: Value) -> Value {
    let x = floating(value);
    boolean(match int(test) {
        0 => x.is_nan(),
        1 => x.is_infinite(),
        2 => {
            if single(precision) {
                (x as f32).is_subnormal()
            } else {
                x.is_subnormal()
            }
        }
        3 => x == 0.0 && x.is_sign_negative(),
        test => fail(&format!("no such test of floating point: {test}")),
    })
}

/// A number as a whole number and the power of two it is multiplied by. The
/// whole number has as many bits as the precision, unless it is nothing.
fn decode(single: bool, x: f64) -> (BigInt, i32) {
    if x == 0.0 {
        return (BigInt::zero(), 0);
    }
    let (negative, mut mantissa, mut exponent, digits) = if single {
        let bits = (x as f32).to_bits();
        let biased = ((bits >> 23) & 0xff) as i32;
        let fraction = (bits & 0x7f_ffff) as u64;
        let (mantissa, exponent) = if biased == 0 { (fraction, -149) } else { (fraction | 1 << 23, biased - 150) };
        (bits >> 31 == 1, mantissa, exponent, 24)
    } else {
        let bits = x.to_bits();
        let biased = ((bits >> 52) & 0x7ff) as i32;
        let fraction = bits & ((1 << 52) - 1);
        let (mantissa, exponent) = if biased == 0 { (fraction, -1074) } else { (fraction | 1 << 52, biased - 1075) };
        (bits >> 63 == 1, mantissa, exponent, 53)
    };
    while mantissa < 1 << (digits - 1) {
        mantissa <<= 1;
        exponent -= 1;
    }
    let mantissa = BigInt::from(mantissa);
    (if negative { -mantissa } else { mantissa }, exponent)
}

#[no_mangle]
pub extern "C" fn hazy_prim_float_mantissa(precision: Value, value: Value) -> Value {
    from_integer(decode(single(precision), floating(value)).0)
}

#[no_mangle]
pub extern "C" fn hazy_prim_float_exponent(precision: Value, value: Value) -> Value {
    from_int(decode(single(precision), floating(value)).1)
}

/// A whole number times a power of two, rounded to the nearest number of
/// the precision, ties to even.
#[no_mangle]
pub extern "C" fn hazy_prim_float_encode(precision: Value, mantissa: Value, exponent: Value) -> Value {
    let single = single(precision);
    let mantissa = integer(mantissa);
    if mantissa.is_zero() {
        return from_floating(0.0);
    }
    let digits: i64 = if single { 24 } else { 53 };
    let mut magnitude = mantissa.magnitude().clone();
    let mut exponent = int(exponent) as i64;
    // Two bits past the precision are kept, and whether anything below
    // them was not nothing: then the conversion rounds as the whole would.
    let bits = magnitude.bits() as i64;
    if bits > digits + 2 {
        let shift = bits - (digits + 2);
        let sticky = (magnitude.trailing_zeros().unwrap_or(0) as i64) < shift;
        magnitude >>= shift as usize;
        if sticky {
            magnitude |= num_bigint::BigUint::one();
        }
        exponent += shift;
    }
    let kept = magnitude.to_u64().unwrap_or(0);
    let mut x = if single { f64::from(kept as f32) } else { kept as f64 };
    while exponent > 1000 {
        x *= 2f64.powi(1000);
        exponent -= 1000;
    }
    while exponent < -1000 {
        x *= 2f64.powi(-1000);
        exponent += 1000;
    }
    x *= 2f64.powi(exponent as i32);
    if single {
        x = f64::from(x as f32);
    }
    from_floating(if mantissa.is_negative() { -x } else { x })
}

/// The whole number a number is, toward nothing.
#[no_mangle]
pub extern "C" fn hazy_prim_float_truncate(value: Value) -> Value {
    from_integer(BigInt::from_f64(floating(value).trunc()).unwrap_or_default())
}

/// An array of so many places, each holding the one value it is given.
#[no_mangle]
pub extern "C" fn hazy_prim_array_new(count: Value, fill: Value) -> Value {
    let count = int(count).max(0) as usize;
    let block = hazy_alloc(2 + count as u64, header(ARRAY, 0, 0));
    unsafe {
        *block.add(1) = count as u64;
        for index in 0..count {
            *block.add(2 + index) = heap::share(fill);
        }
    }
    block as Value
}

/// The place of an array at an index.
fn place(array: Value, index: Value) -> *mut u64 {
    let block = array as *mut u64;
    let length = unsafe { *block.add(1) } as i64;
    let index = int(index) as i64;
    if index < 0 || index >= length {
        fail(&format!("index {index} is out of the bounds of an array of {length}"));
    }
    unsafe { block.add(2 + index as usize) }
}

#[no_mangle]
pub extern "C" fn hazy_prim_array_read(array: Value, index: Value) -> Value {
    heap::share(unsafe { *place(array, index) })
}

/// The same, of an array that is no longer changed.
#[no_mangle]
pub extern "C" fn hazy_prim_array_index(array: Value, index: Value) -> Value {
    heap::share(unsafe { *place(array, index) })
}

#[no_mangle]
pub extern "C" fn hazy_prim_array_write(array: Value, index: Value, value: Value) -> Value {
    let place = place(array, index);
    let old = unsafe { *place };
    unsafe { *place = heap::share(value) };
    heap::release(old);
    unit()
}

#[no_mangle]
pub extern "C" fn hazy_prim_array_length(array: Value) -> Value {
    from_int(unsafe { *(array as *const u64).add(1) } as i64)
}

#[no_mangle]
pub extern "C" fn hazy_prim_arg_count() -> Value {
    from_int(arguments().len() as i64)
}

#[no_mangle]
pub extern "C" fn hazy_prim_arg(index: Value) -> Value {
    match arguments().get(int(index) as usize) {
        Some(argument) => from_text(argument),
        None => fail("there is no such argument"),
    }
}

/// The arguments the program is given, and what it is called, unless the
/// program has said they are others for a while (`withArgs`, `withProgName`).
static mut ARGUMENTS: Option<Vec<String>> = None;
static mut PROGRAM_NAME: Option<String> = None;

fn arguments() -> &'static Vec<String> {
    let place = unsafe { &mut *std::ptr::addr_of_mut!(ARGUMENTS) };
    place.get_or_insert_with(|| {
        std::env::args_os()
            .skip(1)
            .map(|argument| argument.to_string_lossy().into_owned())
            .collect()
    })
}

/// Arguments in place of the program's, each ended by a nothing.
#[no_mangle]
pub extern "C" fn hazy_prim_set_args(given: Value) -> Value {
    let given = text(given).split_terminator('\0').map(str::to_string).collect();
    unsafe { *std::ptr::addr_of_mut!(ARGUMENTS) = Some(given) };
    unit()
}

/// What the program is called: the last part of the name it was run by,
/// without `.exe`, as GHC has it.
#[no_mangle]
pub extern "C" fn hazy_prim_prog_name() -> Value {
    let place = unsafe { &mut *std::ptr::addr_of_mut!(PROGRAM_NAME) };
    let name = place.get_or_insert_with(|| {
        let run = std::env::args_os().next().unwrap_or_default();
        let path = std::path::Path::new(&run);
        let name = path.file_name().unwrap_or_default().to_string_lossy().into_owned();
        match name.strip_suffix(".exe") {
            Some(stem) if cfg!(windows) => stem.to_string(),
            _ => name,
        }
    });
    from_text(name)
}

#[no_mangle]
pub extern "C" fn hazy_prim_set_prog_name(name: Value) -> Value {
    unsafe { *std::ptr::addr_of_mut!(PROGRAM_NAME) = Some(text(name).to_string()) };
    unit()
}

/// Set an environment variable, or unset it if it is given nothing, as GHC
/// does.
#[no_mangle]
pub extern "C" fn hazy_prim_set_env(name: Value, value: Value) -> Value {
    // The program has one thread.
    if text(value).is_empty() {
        std::env::remove_var(text(name));
    } else {
        std::env::set_var(text(name), text(value));
    }
    unit()
}

#[no_mangle]
pub extern "C" fn hazy_prim_unset_env(name: Value) -> Value {
    std::env::remove_var(text(name));
    unit()
}

/// Every environment variable, as its name and its value, each ended by a
/// nothing.
#[no_mangle]
pub extern "C" fn hazy_prim_environment() -> Value {
    let mut all = String::new();
    for (name, value) in std::env::vars_os() {
        all.push_str(&name.to_string_lossy());
        all.push('\0');
        all.push_str(&value.to_string_lossy());
        all.push('\0');
    }
    from_text(&all)
}

/// The value of an environment variable after a 1, or a 0 alone if it is
/// not set.
#[no_mangle]
pub extern "C" fn hazy_prim_get_env(name: Value) -> Value {
    match std::env::var_os(text(name)) {
        Some(value) => from_text(&format!("1{}", value.to_string_lossy())),
        None => from_text("0"),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_executable_path() -> Value {
    match std::env::current_exe().and_then(|path| path.canonicalize()) {
        Ok(path) => from_text(&path.to_string_lossy()),
        Err(error) => fail(&format!("where the program is cannot be found: {error}")),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_exit(code: Value) -> Value {
    let _ = std::io::stdout().flush();
    let _ = std::io::stderr().flush();
    std::process::exit(int(code) as i32)
}

/// A file opened to read, to write, to add to, or to read and write, as the
/// mode is 0, 1, 2 or 3, or made to read and write if it was not there, as
/// it is 4. A file that cannot be opened is a negative number,
/// the kind of the failure, and the system's word for it is kept for
/// `last_failure`.
#[no_mangle]
pub extern "C" fn hazy_prim_open(path: Value, mode: Value) -> Value {
    let mode = int(mode);
    let mut options = std::fs::OpenOptions::new();
    match mode {
        0 => options.read(true),
        1 => options.write(true).create(true).truncate(true),
        2 => options.append(true).create(true),
        3 => options.read(true).write(true).create(true),
        _ => options.read(true).write(true).create_new(true),
    };
    let opened = options.open(text(path)).and_then(|file| {
        // A directory can be opened, but it is not read as a file is.
        if file.metadata()?.is_dir() {
            Err(std::io::Error::new(std::io::ErrorKind::IsADirectory, "is a directory"))
        } else {
            Ok(file)
        }
    });
    match opened {
        Ok(file) => {
            let open = Open {
                path: text(path).to_string(),
                reader: std::io::BufReader::new(file),
                readable: mode == 0 || mode >= 3,
                writable: mode != 0,
            };
            let files = files();
            let place = match files.iter().position(|slot| slot.is_none()) {
                Some(place) => {
                    files[place] = Some(open);
                    place
                }
                None => {
                    files.push(Some(open));
                    files.len() - 1
                }
            };
            from_int(place as i32 + 3)
        }
        Err(error) => {
            unsafe { *std::ptr::addr_of_mut!(FAILURE) = description(&error) };
            from_int(-failure(&error) - 1)
        }
    }
}

/// Take a file away: nothing, if it was, or else the negative of the kind
/// of failure, less one, as `open` has it.
#[no_mangle]
pub extern "C" fn hazy_prim_remove_file(path: Value) -> Value {
    match std::fs::remove_file(text(path)) {
        Ok(()) => from_int(0),
        Err(error) => {
            unsafe { *std::ptr::addr_of_mut!(FAILURE) = description(&error) };
            from_int(-failure(&error) - 1)
        }
    }
}

/// What a file that is open was opened as.
#[no_mangle]
pub extern "C" fn hazy_prim_file_name(descriptor: Value) -> Value {
    from_text(&file(int(descriptor)).path)
}

/// What the system said of the last file that could not be opened.
#[no_mangle]
pub extern "C" fn hazy_prim_last_failure() -> Value {
    from_text(unsafe { &*std::ptr::addr_of!(FAILURE) })
}

#[no_mangle]
pub extern "C" fn hazy_prim_close(descriptor: Value) -> Value {
    let descriptor = int(descriptor);
    if descriptor >= 3 {
        if let Some(slot) = files().get_mut((descriptor - 3) as usize) {
            *slot = None;
        }
    }
    unit()
}

/// Whether a file is open, and to what: 1 to read, 2 to write and 3 to do
/// both, or 0 if it is not.
#[no_mangle]
pub extern "C" fn hazy_prim_file_state(descriptor: Value) -> Value {
    from_int(match int(descriptor) {
        0 => 1,
        1 | 2 => 2,
        descriptor => match files().get((descriptor - 3) as usize) {
            Some(Some(open)) => open.readable as i32 + 2 * open.writable as i32,
            _ => 0,
        },
    })
}

#[no_mangle]
pub extern "C" fn hazy_prim_write(descriptor: Value, value: Value) -> Value {
    write_bytes(int(descriptor), text(value).as_bytes());
    unit()
}

/// Bytes from memory written to a file, as the library's hPutBuf has it.
#[no_mangle]
pub unsafe extern "C" fn hazy_put_buf(descriptor: i64, bytes: *const u8, count: i64) {
    write_bytes(descriptor, std::slice::from_raw_parts(bytes, count.max(0) as usize));
}

/// Bytes read from a file into memory: as many as are asked for, or fewer
/// at its end; or, if the last is not 0, as many as can be had at once.
/// How many were read.
#[no_mangle]
pub unsafe extern "C" fn hazy_get_buf(descriptor: i64, bytes: *mut u8, count: i64, some: i64) -> i64 {
    let buffer = std::slice::from_raw_parts_mut(bytes, count.max(0) as usize);
    reading(descriptor, |reader| {
        let mut filled = 0;
        while filled < buffer.len() {
            let read = reader.read(&mut buffer[filled..])?;
            if read == 0 {
                break;
            }
            filled += read;
            if some != 0 {
                break;
            }
        }
        Ok(filled as i64)
    })
}

fn write_bytes(descriptor: i64, bytes: &[u8]) {
    use std::io::Seek;
    let result = match descriptor {
        1 => std::io::stdout().lock().write_all(bytes),
        2 => {
            // What was written before a complaint is seen before it.
            let _ = std::io::stdout().flush();
            std::io::stderr().lock().write_all(bytes)
        }
        descriptor => {
            let open = file(descriptor);
            // What was read ahead is given back first, so that what is
            // written goes where the program has read to.
            let back = if open.reader.buffer().is_empty() {
                Ok(())
            } else {
                open.reader.seek(std::io::SeekFrom::Current(0)).map(|_| ())
            };
            back.and_then(|()| open.reader.get_mut().write_all(bytes))
        }
    };
    if let Err(error) = result {
        fail(&format!("could not write: {error}"));
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_flush(descriptor: Value) -> Value {
    let result = match int(descriptor) {
        0 => Ok(()),
        1 => std::io::stdout().flush(),
        2 => std::io::stderr().flush(),
        descriptor => file(descriptor).reader.get_mut().flush(),
    };
    if let Err(error) = result {
        fail(&format!("could not write: {error}"));
    }
    unit()
}

#[no_mangle]
pub extern "C" fn hazy_prim_read_all(descriptor: Value) -> Value {
    let bytes = reading(int(descriptor), |reader| {
        let mut bytes = Vec::new();
        reader.read_to_end(&mut bytes)?;
        Ok(bytes)
    });
    from_text(&String::from_utf8_lossy(&bytes))
}

/// A line read after a 1 and without its end, or a 0 alone if there is
/// nothing left to read.
#[no_mangle]
pub extern "C" fn hazy_prim_read_line(descriptor: Value) -> Value {
    let mut bytes = reading(int(descriptor), |reader| {
        let mut bytes = Vec::new();
        reader.read_until(10, &mut bytes)?;
        Ok(bytes)
    });
    if bytes.is_empty() {
        return from_text("0");
    }
    if bytes.last() == Some(&10) {
        bytes.pop();
    }
    from_text(&format!("1{}", String::from_utf8_lossy(&bytes)))
}

/// The character a run of bytes of UTF-8 is, or the one that stands for a
/// character that cannot be read.
fn decode_utf8(bytes: &[u8]) -> i32 {
    match std::str::from_utf8(bytes).ok().and_then(|text| text.chars().next()) {
        Some(character) => character as i32,
        None => 0xFFFD,
    }
}

/// How many bytes of UTF-8 a character is, from its first.
fn width(first: u8) -> usize {
    match first {
        0xC0..=0xDF => 2,
        0xE0..=0xEF => 3,
        0xF0..=0xF7 => 4,
        _ => 1,
    }
}

/// The next character, or -1 if there is nothing left to read.
#[no_mangle]
pub extern "C" fn hazy_prim_read_char(descriptor: Value) -> Value {
    from_int(reading(int(descriptor), |reader| {
        let mut bytes = [0u8; 4];
        let mut count = 0;
        loop {
            let buffer = reader.fill_buf()?;
            let Some(&byte) = buffer.first() else { break };
            // A character ends at the first byte that does not go on with it.
            if count > 0 && (count == width(bytes[0]) || byte & 0xC0 != 0x80) {
                break;
            }
            bytes[count] = byte;
            reader.consume(1);
            count += 1;
            if count == width(bytes[0]) {
                break;
            }
        }
        Ok(if count == 0 { -1 } else { decode_utf8(&bytes[..count]) })
    }))
}

/// The next character, which is left to be read, or -1 if there is nothing
/// left to read.
#[no_mangle]
pub extern "C" fn hazy_prim_peek_char(descriptor: Value) -> Value {
    from_int(reading(int(descriptor), |reader| {
        let buffer = reader.fill_buf()?;
        Ok(match buffer.first() {
            None => -1,
            Some(&first) => decode_utf8(&buffer[..width(first).min(buffer.len())]),
        })
    }))
}

#[no_mangle]
pub extern "C" fn hazy_prim_is_eof(descriptor: Value) -> Value {
    boolean(reading(int(descriptor), |reader| Ok(reader.fill_buf()?.is_empty())))
}

/// How long a file is, in bytes.
#[no_mangle]
pub extern "C" fn hazy_prim_file_size(descriptor: Value) -> Value {
    match file(int(descriptor)).reader.get_ref().metadata() {
        Ok(metadata) => from_integer(BigInt::from(metadata.len())),
        Err(error) => fail(&format!("could not find the size: {error}")),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_set_file_size(descriptor: Value, size: Value) -> Value {
    let size = integer(size).to_u64().unwrap_or_else(|| fail("a size that cannot be"));
    if let Err(error) = file(int(descriptor)).reader.get_ref().set_len(size) {
        fail(&format!("could not set the size: {error}"));
    }
    unit()
}

/// Go to a place in a file: from its start, from where it is read to, or
/// from its end, as the mode is 0, 1 or 2.
#[no_mangle]
pub extern "C" fn hazy_prim_seek(descriptor: Value, mode: Value, offset: Value) -> Value {
    use std::io::{Seek, SeekFrom};
    let offset = integer(offset).to_i64().unwrap_or_else(|| fail("a place that cannot be"));
    let place = match int(mode) {
        0 => SeekFrom::Start(offset.max(0) as u64),
        1 => SeekFrom::Current(offset),
        _ => SeekFrom::End(offset),
    };
    if let Err(error) = file(int(descriptor)).reader.seek(place) {
        fail(&format!("could not seek: {error}"));
    }
    unit()
}

/// Where in a file it is read to, in bytes from its start.
#[no_mangle]
pub extern "C" fn hazy_prim_tell(descriptor: Value) -> Value {
    use std::io::Seek;
    match file(int(descriptor)).reader.stream_position() {
        Ok(place) => from_integer(BigInt::from(place)),
        Err(error) => fail(&format!("could not tell where: {error}")),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_is_terminal(descriptor: Value) -> Value {
    use std::io::IsTerminal;
    from_int(match int(descriptor) {
        0 => std::io::stdin().is_terminal(),
        1 => std::io::stdout().is_terminal(),
        2 => std::io::stderr().is_terminal(),
        _ => false,
    } as i32)
}

/// The names in a directory, in order, each on a line.
#[no_mangle]
pub extern "C" fn hazy_prim_list_directory(path: Value) -> Value {
    match std::fs::read_dir(text(path)) {
        Ok(entries) => {
            let mut names: Vec<String> = entries
                .filter_map(|entry| entry.ok())
                .map(|entry| entry.file_name().to_string_lossy().into_owned())
                .collect();
            names.sort();
            from_text(&names.join("\n"))
        }
        Err(error) => fail(&format!("{}: {error}", text(path))),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_create_directory(path: Value) -> Value {
    if let Err(error) = std::fs::create_dir_all(text(path)) {
        fail(&format!("{}: {error}", text(path)));
    }
    unit()
}

/// Whether a path is there: 0 if not, 1 for a file, 2 for a directory.
#[no_mangle]
pub extern "C" fn hazy_prim_path_kind(path: Value) -> Value {
    from_int(match std::fs::metadata(text(path)) {
        Ok(found) if found.is_dir() => 2,
        Ok(_) => 1,
        Err(_) => 0,
    })
}

/// A program run to its end with arguments, each on a line, and what it
/// exited with.
#[no_mangle]
pub extern "C" fn hazy_prim_call_process(program: Value, arguments: Value) -> Value {
    let _ = std::io::stdout().flush();
    let mut command = std::process::Command::new(text(program));
    if !text(arguments).is_empty() {
        command.args(text(arguments).split("\n"));
    }
    match command.status() {
        Ok(status) => from_int(status.code().unwrap_or(1)),
        Err(error) => fail(&format!("{}: {error}", text(program))),
    }
}

/// Thousandths of a second since the first time this was asked.
#[no_mangle]
pub extern "C" fn hazy_prim_clock() -> Value {
    static START: std::sync::OnceLock<std::time::Instant> = std::sync::OnceLock::new();
    let start = START.get_or_init(std::time::Instant::now);
    from_int(start.elapsed().as_millis() as i64)
}

/// The system, as Haskell's `System.Info.os` names it.
#[no_mangle]
pub extern "C" fn hazy_prim_os() -> Value {
    from_text(if cfg!(target_os = "windows") {
        "mingw32"
    } else if cfg!(target_os = "macos") {
        "darwin"
    } else {
        std::env::consts::OS
    })
}

/// Texts that are being built, each known by where it is here. One is
/// begun, added to and finished, and others may be begun and finished while
/// it waits for what it is added next.
static mut BUILDING: Vec<Option<String>> = Vec::new();

fn building() -> &'static mut Vec<Option<String>> {
    // The program has one thread.
    unsafe { &mut *std::ptr::addr_of_mut!(BUILDING) }
}

fn built(buffer: Value) -> &'static mut String {
    match building().get_mut(int(buffer) as usize) {
        Some(Some(string)) => string,
        _ => fail("a text that is not being built"),
    }
}

#[no_mangle]
pub extern "C" fn hazy_prim_pack_new() -> Value {
    let building = building();
    let place = match building.iter().rposition(|slot| slot.is_none()) {
        Some(place) => place,
        None => {
            building.push(None);
            building.len() - 1
        }
    };
    building[place] = Some(String::new());
    from_int(place as i64)
}

#[no_mangle]
pub extern "C" fn hazy_prim_pack_push(buffer: Value, character: Value) -> Value {
    // A code point that is not a character becomes the replacement character.
    built(buffer).push(char::from_u32(int(character) as u32).unwrap_or('\u{fffd}'));
    unit()
}

#[no_mangle]
pub extern "C" fn hazy_prim_pack_append(buffer: Value, value: Value) -> Value {
    built(buffer).push_str(text(value));
    unit()
}

#[no_mangle]
pub extern "C" fn hazy_prim_pack_done(buffer: Value) -> Value {
    match building().get_mut(int(buffer) as usize).and_then(|slot| slot.take()) {
        Some(string) => from_text(&string),
        None => fail("a text that is not being built"),
    }
}

/// Begin to catch: what handles a thrown value, and where to go on with what
/// the handler gives.
#[no_mangle]
pub extern "C" fn hazy_prim_catch_push(handler: Value, resume: Value) -> Value {
    heap::catching().push((heap::share(handler), heap::share(resume)));
    unit()
}

/// Whether anything is catching. Inside a call from C into the program,
/// a catch begun before C was called is out of reach.
#[no_mangle]
pub extern "C" fn hazy_prim_catching() -> Value {
    boolean(matches!(heap::catching().last(), Some(&(handler, _)) if handler != heap::BARRIER))
}

/// C calls into the program: what is thrown there and not caught there is
/// not caught by a catch begun before C was called.
#[no_mangle]
pub extern "C" fn hazy_catch_barrier() {
    heap::catching().push((heap::BARRIER, heap::BARRIER));
}

/// The call from C into the program is over.
#[no_mangle]
pub extern "C" fn hazy_catch_unbarrier() {
    heap::catching().pop();
}

/// What a call from C into the program left for C, until C takes it.
static mut CALLBACK_RESULT: Value = 0;

/// Leave a value for C, at the end of a call from C into the program.
#[no_mangle]
pub extern "C" fn hazy_prim_callback_result(value: Value) -> Value {
    unsafe { CALLBACK_RESULT = heap::share(value) };
    unit()
}

/// A function of C made of a function of the program, as a wrapper does:
/// the first of the functions of C given whose place is free, which keeps
/// the code that applies the function to what C gives, and the function.
#[no_mangle]
pub extern "C" fn hazy_wrapper_new(
    slots: *mut Value,
    stubs: *const u64,
    count: u64,
    entry: Value,
    function: Value,
) -> u64 {
    unsafe {
        for index in 0..count as usize {
            let slot = slots.add(2 * index);
            if *slot == 0 {
                *slot = heap::share(entry);
                *slot.add(1) = heap::share(function);
                let address = *stubs.add(index);
                heap::wrapped().insert(address, slot);
                return address;
            }
        }
    }
    fail(&format!(
        "more than {count} functions of C of one type were made of functions of the program and not freed"
    ))
}

/// A function of C that a wrapper made is freed: what it called is given up
/// and its place is free again. Anything else is left as it is.
#[no_mangle]
pub extern "C" fn hazy_wrapper_free(address: u64) {
    if let Some(slot) = heap::wrapped().remove(&address) {
        unsafe {
            heap::release(*slot);
            heap::release(*slot.add(1));
            *slot = 0;
            *slot.add(1) = 0;
        }
    }
}

/// What the call from C into the program left. The reference is C's.
#[no_mangle]
pub extern "C" fn hazy_callback_take() -> Value {
    unsafe {
        let value = CALLBACK_RESULT;
        CALLBACK_RESULT = 0;
        value
    }
}

/// A value was thrown and nothing catches it. It said what the program
/// exits with, or else how it is shown.
#[no_mangle]
pub extern "C" fn hazy_prim_uncaught(status: Value, shown: Value) -> Value {
    if int(status) >= 0 {
        let _ = std::io::stdout().flush();
        std::process::exit(int(status) as i32)
    }
    fail(&format!("uncaught exception: {}", text(shown)))
}

/// The catch begun last, which is over: its handler and where it goes on,
/// as a constructor of two fields.
#[no_mangle]
pub extern "C" fn hazy_prim_catch_pop() -> Value {
    match heap::catching().pop() {
        Some((handler, resume)) => {
            let block = hazy_alloc(3, header(DATA, 0, 2));
            unsafe {
                *block.add(1) = handler;
                *block.add(2) = resume;
            }
            block as Value
        }
        None => fail("a value was thrown and nothing catches it"),
    }
}
