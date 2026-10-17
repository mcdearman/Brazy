//! What the library's System.Directory asks of the system, as functions of
//! C: each is given paths as strings of C in UTF-8, gives back -1 or a null
//! string when it fails, with errno saying why, and gives back a string it
//! makes with C's malloc, for the library to free.

use std::ffi::{c_char, CStr};
use std::io::ErrorKind;
use std::path::{Path, PathBuf};

use crate::hazy_set_errno;

extern "C" {
    fn malloc(size: usize) -> *mut u8;
    #[cfg(unix)]
    fn access(path: *const c_char, mode: i32) -> i32;
}

unsafe fn path<'a>(text: *const c_char) -> &'a Path {
    Path::new(CStr::from_ptr(text).to_str().unwrap_or(""))
}

/// The errno of what went wrong: the system's own on a POSIX system, and
/// the POSIX number of its kind on Windows.
fn failed(error: std::io::Error) {
    #[cfg(unix)]
    let number = error.raw_os_error();
    #[cfg(not(unix))]
    let number: Option<i32> = None;
    let number = number.unwrap_or(match error.kind() {
        ErrorKind::NotFound => 2,
        ErrorKind::PermissionDenied => 13,
        ErrorKind::AlreadyExists => 17,
        ErrorKind::NotADirectory => 20,
        ErrorKind::IsADirectory => 21,
        ErrorKind::InvalidInput => 22,
        ErrorKind::DirectoryNotEmpty => 39,
        _ => 5,
    });
    hazy_set_errno(number);
}

fn done(result: std::io::Result<()>) -> i32 {
    match result {
        Ok(()) => 0,
        Err(error) => {
            failed(error);
            -1
        }
    }
}

/// A string of C made with malloc, or null if there is none.
fn string(result: std::io::Result<PathBuf>) -> *mut c_char {
    match result {
        Ok(found) => {
            let text = found.to_string_lossy();
            // Windows gives a path made canonical as one past its limit.
            let text = text.strip_prefix(r"\\?\").unwrap_or(&text);
            unsafe {
                let made = malloc(text.len() + 1);
                std::ptr::copy_nonoverlapping(text.as_ptr(), made, text.len());
                *made.add(text.len()) = 0;
                made as *mut c_char
            }
        }
        Err(error) => {
            failed(error);
            std::ptr::null_mut()
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hazy_remove_directory(at: *const c_char) -> i32 {
    done(std::fs::remove_dir(path(at)))
}

#[no_mangle]
pub unsafe extern "C" fn hazy_remove_directory_recursive(at: *const c_char) -> i32 {
    done(std::fs::remove_dir_all(path(at)))
}

#[no_mangle]
pub unsafe extern "C" fn hazy_rename(from: *const c_char, to: *const c_char) -> i32 {
    done(std::fs::rename(path(from), path(to)))
}

#[no_mangle]
pub extern "C" fn hazy_current_directory() -> *mut c_char {
    string(std::env::current_dir())
}

#[no_mangle]
pub unsafe extern "C" fn hazy_set_current_directory(at: *const c_char) -> i32 {
    done(std::env::set_current_dir(path(at)))
}

#[no_mangle]
pub extern "C" fn hazy_home_directory() -> *mut c_char {
    let name = if cfg!(windows) { "USERPROFILE" } else { "HOME" };
    string(
        std::env::var_os(name)
            .map(PathBuf::from)
            .ok_or_else(|| std::io::Error::from(ErrorKind::NotFound)),
    )
}

#[no_mangle]
pub extern "C" fn hazy_temporary_directory() -> *mut c_char {
    string(Ok(std::env::temp_dir()))
}

#[no_mangle]
pub unsafe extern "C" fn hazy_copy_file(from: *const c_char, to: *const c_char) -> i32 {
    done(std::fs::copy(path(from), path(to)).map(|_| ()))
}

#[no_mangle]
pub unsafe extern "C" fn hazy_file_size(at: *const c_char) -> i64 {
    match std::fs::metadata(path(at)) {
        Ok(metadata) => metadata.len() as i64,
        Err(error) => {
            failed(error);
            -1
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hazy_canonicalize(at: *const c_char) -> *mut c_char {
    string(std::fs::canonicalize(path(at)))
}

/// A link to the first path, at the second: to a directory if the third is
/// not 0, which only Windows tells apart.
#[no_mangle]
pub unsafe extern "C" fn hazy_create_link(target: *const c_char, link: *const c_char, directory: i32) -> i32 {
    #[cfg(unix)]
    {
        let _ = directory;
        done(std::os::unix::fs::symlink(path(target), path(link)))
    }
    #[cfg(windows)]
    {
        if directory != 0 {
            done(std::os::windows::fs::symlink_dir(path(target), path(link)))
        } else {
            done(std::os::windows::fs::symlink_file(path(target), path(link)))
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn hazy_remove_link(at: *const c_char) -> i32 {
    let at = path(at);
    match std::fs::symlink_metadata(at) {
        Ok(metadata) if metadata.is_dir() => done(std::fs::remove_dir(at)),
        Ok(_) => done(std::fs::remove_file(at)),
        Err(error) => done(Err(error)),
    }
}

#[no_mangle]
pub unsafe extern "C" fn hazy_read_link(at: *const c_char) -> *mut c_char {
    string(std::fs::read_link(path(at)))
}

/// 1 if the path is a link, 0 if it is something else, and -1 if there is
/// nothing there.
#[no_mangle]
pub unsafe extern "C" fn hazy_is_link(at: *const c_char) -> i32 {
    match std::fs::symlink_metadata(path(at)) {
        Ok(metadata) => metadata.file_type().is_symlink() as i32,
        Err(error) => {
            failed(error);
            -1
        }
    }
}

/// What may be done with what is at a path, as bits: reading 1, writing 2,
/// running a file 4, and searching a directory 8. On a POSIX system it is
/// what access says the program may do.
#[no_mangle]
pub unsafe extern "C" fn hazy_permissions(at: *const c_char) -> i32 {
    let metadata = match std::fs::metadata(path(at)) {
        Ok(metadata) => metadata,
        Err(error) => {
            failed(error);
            return -1;
        }
    };
    #[cfg(unix)]
    {
        let may = |mode| (access(at, mode) == 0) as i32;
        let run = may(1);
        may(4) | may(2) << 1 | if metadata.is_dir() { run << 3 } else { run << 2 }
    }
    #[cfg(windows)]
    {
        let written = !metadata.permissions().readonly() as i32;
        let runs = path(at)
            .extension()
            .and_then(|extension| extension.to_str())
            .map(|extension| ["exe", "bat", "com", "cmd"].contains(&extension.to_ascii_lowercase().as_str()))
            .unwrap_or(false);
        1 | written << 1
            | if metadata.is_dir() { 1 << 3 } else { (runs as i32) << 2 }
    }
}

/// The owner's permissions set to the bits given; on Windows only whether
/// it may be written is kept.
#[no_mangle]
pub unsafe extern "C" fn hazy_set_permissions(at: *const c_char, bits: i32) -> i32 {
    let at = path(at);
    let metadata = match std::fs::metadata(at) {
        Ok(metadata) => metadata,
        Err(error) => return done(Err(error)),
    };
    let mut permissions = metadata.permissions();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let run = if metadata.is_dir() { bits & 8 != 0 } else { bits & 4 != 0 };
        let owner = (bits & 1 != 0) as u32 * 0o400 | (bits & 2 != 0) as u32 * 0o200 | run as u32 * 0o100;
        permissions.set_mode(permissions.mode() & !0o700 | owner);
    }
    #[cfg(windows)]
    {
        permissions.set_readonly(bits & 2 == 0);
    }
    done(std::fs::set_permissions(at, permissions))
}
