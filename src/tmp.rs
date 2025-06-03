use std::{
    collections::HashMap,
    ffi::OsStr,
    ops::Deref,
    path::{Path, PathBuf},
};

use parking_lot::{MappedMutexGuard, Mutex, MutexGuard};

static CLEANUP_FILES: Mutex<Option<Cleanup>> = Mutex::new(None);

fn files() -> MappedMutexGuard<'static, Cleanup> {
    let files = CLEANUP_FILES.lock();
    MutexGuard::map(files, |files| {
        loop {
            if let Some(files) = unsafe { &mut *(files as *mut Option<Cleanup>) }.as_mut() {
                return files;
            }
            *files = Some(Cleanup {
                index: 0,
                files: HashMap::new(),
            });

            ctrlc::set_handler(destroy).unwrap();
        }
    })
}

#[inline(never)]
#[cold]
fn destroy() {
    unsafe { (*CLEANUP_FILES.data_ptr()).as_mut().unwrap_unchecked() }.finalize();
    std::process::exit(130);
}

struct Cleanup {
    index: usize,
    files: HashMap<usize, PathBuf>,
}

impl Cleanup {
    pub fn insert(&mut self, path: PathBuf) -> usize {
        loop {
            let index = self.index;
            self.index += self.index.wrapping_add(1);
            match self.files.entry(index) {
                std::collections::hash_map::Entry::Occupied(_) => (),
                std::collections::hash_map::Entry::Vacant(vacant_entry) => {
                    vacant_entry.insert(path);
                    return index;
                }
            }
        }
    }

    pub fn remove(&mut self, index: usize) {
        _ = std::fs::remove_file(self.ignore(index));
    }

    pub fn ignore(&mut self, index: usize) -> PathBuf {
        self.files.remove(&index).unwrap()
    }

    fn finalize(&mut self) {
        for (_, path) in core::mem::take(&mut self.files) {
            _ = std::fs::remove_file(path);
        }
    }
}

#[derive(Debug)]
pub struct TempFile {
    key: usize,
    path: PathBuf,
}

impl Deref for TempFile {
    type Target = Path;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.path
    }
}

impl AsRef<Path> for TempFile {
    #[inline(always)]
    fn as_ref(&self) -> &Path {
        self
    }
}

impl std::borrow::Borrow<Path> for TempFile {
    #[inline(always)]
    fn borrow(&self) -> &Path {
        self
    }
}

impl TempFile {
    pub fn with_prefix_in<S: AsRef<OsStr>, P: AsRef<Path>>(
        suffix: S,
        dir: P,
    ) -> std::io::Result<TempFile> {
        let (f, path) = tempfile::NamedTempFile::with_prefix_in(suffix, dir)?.keep()?;
        drop(f);
        let key = files().insert(path.clone());
        Ok(Self { key, path })
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        files().remove(self.key);
    }
}
