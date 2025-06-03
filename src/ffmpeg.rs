use std::{
    collections::{HashMap, HashSet},
    io::{self, BufRead, BufReader},
    path::Path,
    process::{Command, Stdio},
    sync::Mutex,
};

use serde::{Deserialize, Serialize};

pub struct OnceLock<T>(Mutex<Option<T>>);

impl<T> OnceLock<T> {
    pub const fn new() -> Self {
        Self(Mutex::new(None))
    }

    pub fn init_and_mutate<F1, F2, R>(&self, init: F1, f: F2) -> R
    where
        F2: FnOnce(&mut T) -> R,
        F1: FnOnce() -> T,
    {
        let mut lock = self.0.lock().unwrap();
        if lock.is_none() {
            *lock = Some(init());
        }
        f(unsafe { &mut *(lock.as_mut().unwrap_unchecked() as *mut T) })
    }

    #[allow(dead_code)]
    pub fn get_or_init<F>(&self, f: F) -> &T
    where
        F: FnOnce() -> T,
    {
        let mut lock = self.0.lock().unwrap();
        if let Some(value) = unsafe { &*(&*lock as *const Option<T>) }.as_ref() {
            value
        } else {
            *lock = Some(f());
            unsafe { &*(lock.as_ref().unwrap_unchecked() as *const T) }
        }
    }

    pub fn get_or_try_init<F, E>(&self, f: F) -> Result<&T, E>
    where
        F: FnOnce() -> Result<T, E>,
    {
        let mut lock = self.0.lock().unwrap();
        if let Some(value) = unsafe { &*(&*lock as *const Option<T>) }.as_ref() {
            Ok(value)
        } else {
            match f() {
                Ok(value) => {
                    *lock = Some(value);
                    Ok(unsafe { &*(lock.as_ref().unwrap_unchecked() as *const T) })
                }
                Err(err) => Err(err),
            }
        }
    }
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct CodecFlags: u8 {
        const DECODING_SUPPORTED = 1 << 0;
        const ENCODING_SUPPORTED = 1 << 1;
        const INTRA_FRAME_ONLY_CODEC = 1 << 2;
        const LOSSY_COMPRESSION = 1 << 3;
        const LOSSLESS_COMPRESSION = 1 << 4;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Codecs {
    pub video: HashMap<Box<str>, CodecFlags>,
    pub audio: HashMap<Box<str>, CodecFlags>,
    pub subtitle: HashMap<Box<str>, CodecFlags>,
    pub data: HashMap<Box<str>, CodecFlags>,
    pub attachment: HashMap<Box<str>, CodecFlags>,
}

fn fetch_codecs() -> io::Result<Codecs> {
    let mut cmd = Command::new("ffmpeg")
        .arg("-hide_banner")
        .arg("-codecs")
        .stdout(Stdio::piped())
        .stdin(Stdio::null())
        .spawn()?;
    let mut codecs = Codecs {
        video: HashMap::new(),
        audio: HashMap::new(),
        subtitle: HashMap::new(),
        data: HashMap::new(),
        attachment: HashMap::new(),
    };

    {
        let mut stdout = BufReader::new(cmd.stdout.as_mut().unwrap());
        let mut line = String::new();
        let mut head = true;

        while {
            line.clear();
            stdout.read_line(&mut line)?
        } != 0
        {
            if line.ends_with('\n') {
                line.remove(line.len() - 1);
                if line.ends_with('\r') {
                    line.remove(line.len() - 1);
                }
            } else if line.ends_with("\n\r") {
                line.remove(line.len() - 1);
                line.remove(line.len() - 1);
            }
            let line = line.trim();
            if head {
                if line.as_bytes().iter().all(|&c| c == b'-') {
                    head = false;
                }
                continue;
            }

            let mut parts = line.split_whitespace();
            let Some(flags) = parts.next() else {
                continue;
            };
            let flags = flags.as_bytes();
            if flags.len() != 6 {
                continue;
            }

            let Some(name) = parts.next() else {
                continue;
            };
            if name.is_empty() {
                continue;
            }

            let sflags = [flags[0], flags[1], flags[2], flags[3], flags[4], flags[5]];
            let kind = match sflags[2] {
                b'V' => &mut codecs.video,
                b'A' => &mut codecs.audio,
                b'S' => &mut codecs.subtitle,
                b'D' => &mut codecs.data,
                b'T' => &mut codecs.attachment,
                _ => continue,
            };
            let mut flags = CodecFlags::empty();
            if sflags[0] == b'D' {
                flags |= CodecFlags::DECODING_SUPPORTED;
            }
            if sflags[1] == b'E' {
                flags |= CodecFlags::ENCODING_SUPPORTED;
            }
            if sflags[3] == b'I' {
                flags |= CodecFlags::INTRA_FRAME_ONLY_CODEC;
            }
            if sflags[4] == b'L' {
                flags |= CodecFlags::LOSSY_COMPRESSION;
            }
            if sflags[5] == b'S' {
                flags |= CodecFlags::LOSSLESS_COMPRESSION;
            }

            kind.insert(name.into(), flags);
        }
    }

    let status = cmd.wait()?;
    if !status.success() {
        return Err(io::Error::other(format!(
            "ffmpeg exited with {} status code",
            status
        )));
    }

    Ok(codecs)
}

static CODECS: OnceLock<Codecs> = OnceLock::new();

#[inline(always)]
#[allow(clippy::borrow_interior_mutable_const)]
pub fn codecs() -> io::Result<&'static Codecs> {
    CODECS.get_or_try_init(fetch_codecs)
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct EncoderFlags: u8 {
        const FRAME_LEVEL_MULTITHREADING = 1 << 0;
        const SLICE_LEVEL_MULTITHREADING = 1 << 1;
        const EXPERIMENTAL_CODEC = 1 << 2;
        const SUPPORTS_DRAW_HORIZ_BAND = 1 << 3;
        const SUPPORTS_DIRECT_RENDERING_METHOD_1 = 1 << 4;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Encoders {
    pub video: HashMap<Box<str>, EncoderFlags>,
    pub audio: HashMap<Box<str>, EncoderFlags>,
    pub subtitle: HashMap<Box<str>, EncoderFlags>,
}

fn fetch_encoders() -> io::Result<Encoders> {
    let mut cmd = Command::new("ffmpeg")
        .arg("-hide_banner")
        .arg("-encoders")
        .stdout(Stdio::piped())
        .stdin(Stdio::null())
        .spawn()?;
    let mut encoders = Encoders {
        video: HashMap::new(),
        audio: HashMap::new(),
        subtitle: HashMap::new(),
    };

    {
        let mut stdout = BufReader::new(cmd.stdout.as_mut().unwrap());
        let mut line = String::new();
        let mut head = true;

        while {
            line.clear();
            stdout.read_line(&mut line)?
        } != 0
        {
            if line.ends_with('\n') {
                line.remove(line.len() - 1);
                if line.ends_with('\r') {
                    line.remove(line.len() - 1);
                }
            } else if line.ends_with("\n\r") {
                line.remove(line.len() - 1);
                line.remove(line.len() - 1);
            }
            let line = line.trim();
            if head {
                if line.as_bytes().iter().all(|&c| c == b'-') {
                    head = false;
                }
                continue;
            }

            let mut parts = line.split_whitespace();
            let Some(flags) = parts.next() else {
                continue;
            };
            let flags = flags.as_bytes();
            if flags.len() != 6 {
                continue;
            }

            let Some(name) = parts.next() else {
                continue;
            };
            if name.is_empty() {
                continue;
            }

            let sflags = [flags[0], flags[1], flags[2], flags[3], flags[4], flags[5]];
            let kind = match sflags[0] {
                b'V' => &mut encoders.video,
                b'A' => &mut encoders.audio,
                b'S' => &mut encoders.subtitle,
                _ => continue,
            };
            let mut flags = EncoderFlags::empty();
            if sflags[1] == b'F' {
                flags |= EncoderFlags::FRAME_LEVEL_MULTITHREADING;
            }
            if sflags[2] == b'S' {
                flags |= EncoderFlags::SLICE_LEVEL_MULTITHREADING;
            }
            if sflags[3] == b'X' {
                flags |= EncoderFlags::EXPERIMENTAL_CODEC;
            }
            if sflags[4] == b'B' {
                flags |= EncoderFlags::SUPPORTS_DRAW_HORIZ_BAND;
            }
            if sflags[5] == b'D' {
                flags |= EncoderFlags::SUPPORTS_DIRECT_RENDERING_METHOD_1;
            }

            kind.insert(name.into(), flags);
        }
    }

    let status = cmd.wait()?;
    if !status.success() {
        return Err(io::Error::other(format!(
            "ffmpeg exited with {} status code",
            status
        )));
    }

    Ok(encoders)
}

static ENCODERS: OnceLock<Encoders> = OnceLock::new();

#[inline(always)]
#[allow(clippy::borrow_interior_mutable_const)]
pub fn encoders() -> io::Result<&'static Encoders> {
    ENCODERS.get_or_try_init(fetch_encoders)
}

#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum FilterKind {
    S_A,    // |->A
    A_S,    // A->|
    A_A,    // A->A
    AA_A,   // AA->A
    A_N,    // A->N
    S_AV,   // |->AV
    A_V,    // A->V
    S_N,    // |->N
    N_A,    // N->A
    N_N,    // N->N
    N_V,    // N->V
    S_V,    // |->V
    V_S,    // V->|
    V_N,    // V->N
    V_V,    // V->V
    VV_A,   // VV->A
    VV_V,   // VV->V
    VV_VV,  // VV->VV
    VVV_V,  // VVV->V
    VVVV_V, // VVVV->V
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct FilterFlags: u8 {
        const TIMELINE_SUPPORT = 1 << 0;
        const SLICE_THREADING = 1 << 1;
        const COMMAND_SUPPORT = 1 << 2;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Filter {
    pub kind: FilterKind,
    pub flags: FilterFlags,
}

pub type Filters = HashMap<Box<str>, Filter>;

fn fetch_filters() -> io::Result<Filters> {
    let mut cmd = Command::new("ffmpeg")
        .arg("-hide_banner")
        .arg("-filters")
        .stdout(Stdio::piped())
        .stdin(Stdio::null())
        .spawn()?;
    let mut filters = Filters::new();
    {
        let mut stdout = BufReader::new(cmd.stdout.as_mut().unwrap());
        let mut line = String::new();
        let mut head = true;

        while {
            line.clear();
            stdout.read_line(&mut line)?
        } != 0
        {
            if line.ends_with('\n') {
                line.remove(line.len() - 1);
                if line.ends_with('\r') {
                    line.remove(line.len() - 1);
                }
            } else if line.ends_with("\n\r") {
                line.remove(line.len() - 1);
                line.remove(line.len() - 1);
            }
            if head {
                if line.len() > 2 && line.as_bytes()[0] == b' ' && line.as_bytes()[1] != b' ' {
                    head = false;
                } else {
                    continue;
                }
            }
            let line = line.trim();

            let mut parts = line.split_whitespace();
            let Some(flags) = parts.next() else {
                continue;
            };
            let flags = flags.as_bytes();
            if flags.len() != 3 {
                continue;
            }

            let Some(name) = parts.next() else {
                continue;
            };
            if name.is_empty() {
                continue;
            }

            let Some(kind) = parts.next() else {
                continue;
            };
            if kind.is_empty() {
                continue;
            }

            let kind = match kind {
                "|->A" => FilterKind::S_A,
                "A->|" => FilterKind::A_S,
                "A->A" => FilterKind::A_A,
                "AA->A" => FilterKind::AA_A,
                "A->N" => FilterKind::A_N,
                "|->AV" => FilterKind::S_AV,
                "A->V" => FilterKind::A_V,
                "|->N" => FilterKind::S_N,
                "N->A" => FilterKind::N_A,
                "N->N" => FilterKind::N_N,
                "N->V" => FilterKind::N_V,
                "|->V" => FilterKind::S_V,
                "V->|" => FilterKind::V_S,
                "V->N" => FilterKind::V_N,
                "V->V" => FilterKind::V_V,
                "VV->A" => FilterKind::VV_A,
                "VV->V" => FilterKind::VV_V,
                "VV->VV" => FilterKind::VV_VV,
                "VVV->V" => FilterKind::VVV_V,
                "VVVV->V" => FilterKind::VVVV_V,
                _ => continue,
            };

            let sflags = [flags[0], flags[1], flags[2]];
            let mut flags = FilterFlags::empty();
            if sflags[0] == b'T' {
                flags |= FilterFlags::TIMELINE_SUPPORT;
            }
            if sflags[1] == b'S' {
                flags |= FilterFlags::SLICE_THREADING;
            }
            if sflags[2] == b'C' {
                flags |= FilterFlags::COMMAND_SUPPORT;
            }

            filters.insert(name.into(), Filter { kind, flags });
        }
    }

    let status = cmd.wait()?;
    if !status.success() {
        return Err(io::Error::other(format!(
            "ffmpeg exited with {} status code",
            status
        )));
    }

    Ok(filters)
}

static FILTERS: OnceLock<Filters> = OnceLock::new();

#[inline(always)]
#[allow(clippy::borrow_interior_mutable_const)]
pub fn filters() -> io::Result<&'static Filters> {
    FILTERS.get_or_try_init(fetch_filters)
}

fn fetch_hwaccels() -> io::Result<HashSet<Box<str>>> {
    let mut cmd = Command::new("ffmpeg")
        .arg("-hide_banner")
        .arg("-hwaccels")
        .stdout(Stdio::piped())
        .stdin(Stdio::null())
        .spawn()?;
    let mut hwaccels = HashSet::new();
    {
        let mut stdout = BufReader::new(cmd.stdout.as_mut().unwrap());
        let mut line = String::new();
        let mut head = true;

        while {
            line.clear();
            stdout.read_line(&mut line)?
        } != 0
        {
            if head {
                head = false;
                continue;
            }

            if line.ends_with('\n') {
                line.remove(line.len() - 1);
                if line.ends_with('\r') {
                    line.remove(line.len() - 1);
                }
            } else if line.ends_with("\n\r") {
                line.remove(line.len() - 1);
                line.remove(line.len() - 1);
            }

            hwaccels.insert(line.trim().into());
        }
    }

    let status = cmd.wait()?;
    if !status.success() {
        return Err(io::Error::other(format!(
            "ffmpeg exited with {} status code",
            status
        )));
    }

    Ok(hwaccels)
}

static HWACCELS: OnceLock<HashSet<Box<str>>> = OnceLock::new();

#[inline(always)]
#[allow(clippy::borrow_interior_mutable_const)]
pub fn hwaccels() -> io::Result<&'static HashSet<Box<str>>> {
    HWACCELS.get_or_try_init(fetch_hwaccels)
}

static HAS_HWACCELS: OnceLock<HashMap<Box<str>, bool>> = OnceLock::new();

fn check_hwaccel(accel: &str) -> io::Result<bool> {
    let mut child = Command::new("ffmpeg")
        .arg("-y")
        .arg("-hide_banner")
        .arg("-hwaccel")
        .arg(accel)
        .arg("-f")
        .arg("lavfi")
        .arg("-i")
        .arg("testsrc=duration=0:size=1280x720:rate=30")
        .arg("-f")
        .arg("null")
        .arg("-")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()?;
    let status = child.wait()?;
    Ok(status.success())
}

#[inline(always)]
pub fn has_hwaccel(accel: &str) -> io::Result<bool> {
    HAS_HWACCELS.init_and_mutate(HashMap::new, |accels| {
        if let Some(has) = accels.get(accel).copied() {
            return Ok(has);
        }
        if !hwaccels()?.contains(accel) {
            accels.insert(accel.into(), false);
            return Ok(false);
        }
        let res = check_hwaccel(accel)?;
        accels.insert(accel.into(), res);
        Ok(res)
    })
}

static HAS_VIDEO_ENCODER: OnceLock<HashMap<Box<str>, bool>> = OnceLock::new();

fn check_video_encoder(encoder: &str) -> io::Result<bool> {
    let mut child = Command::new("ffmpeg")
        .arg("-y")
        .arg("-hide_banner")
        .arg("-f")
        .arg("lavfi")
        .arg("-i")
        .arg("testsrc=duration=0:size=1280x720:rate=30")
        .arg("-c:v")
        .arg(encoder)
        .arg("-f")
        .arg("null")
        .arg("-")
        .stdin(Stdio::null())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()?;
    let status = child.wait()?;
    Ok(status.success())
}

#[inline(always)]
pub fn has_video_encoder(encoder: &str) -> io::Result<bool> {
    HAS_VIDEO_ENCODER.init_and_mutate(HashMap::new, |encs| {
        if let Some(has) = encs.get(encoder).copied() {
            return Ok(has);
        }
        if !encoders()?.video.contains_key(encoder) {
            encs.insert(encoder.into(), false);
            return Ok(false);
        }
        let res = check_video_encoder(encoder)?;
        encs.insert(encoder.into(), res);
        Ok(res)
    })
}

#[derive(Debug, Serialize, Deserialize, Copy, Clone)]
pub struct Dimension {
    pub height: u32,
    pub width: u32,
}

impl Dimension {
    pub const D720P: Dimension = Dimension {
        height: 720,
        width: 1280,
    };

    pub const D1080P: Dimension = Dimension {
        height: 1920,
        width: 1080,
    };
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "codec_type", rename_all = "lowercase")]
pub enum Stream {
    Unknown {
        index: u64,
    },
    Video {
        index: u64,
        #[serde(rename = "codec_name")]
        codec: Box<str>,
        #[serde(flatten)]
        dimension: Dimension,
    },
    Audio {
        index: u64,
        #[serde(rename = "codec_name")]
        codec: Box<str>,
    },
    Data {
        index: u64,
    },
    Subtitle {
        index: u64,
        #[serde(rename = "codec_name")]
        codec: Box<str>,
    },
    Attachment {
        index: u64,
    },
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct FormatTags {
    pub major_brand: Option<Box<str>>,
    pub minor_version: Option<Box<str>>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct Format {
    pub format_name: Box<str>,
    pub tags: Option<FormatTags>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct MediaInfo {
    pub streams: Vec<Stream>,
    pub format: Format,
}

const VALID_MP4_BRANDS: &[&str] = ["isom", "iso2", "mp41", "mp42", "avc1"].as_slice();

impl FormatTags {
    pub fn has_mp4_major_brand(&self) -> bool {
        self.major_brand
            .as_ref()
            .map(|b| VALID_MP4_BRANDS.iter().any(|vb| b.as_ref() == *vb))
            .unwrap_or(false)
    }
}

impl Format {
    pub fn is_mp4(&self) -> bool {
        self.format_name.split(',').any(|f| f.trim() == "mp4")
            && self
                .tags
                .as_ref()
                .is_some_and(FormatTags::has_mp4_major_brand)
    }
}

impl MediaInfo {
    pub fn is_mp4(&self) -> bool {
        self.format.is_mp4()
    }
}

pub fn get_video_info<P: AsRef<Path>>(path: P) -> io::Result<MediaInfo> {
    let out = Command::new("ffprobe")
        .arg("-v")
        .arg("error")
        .arg("-show_format")
        .arg("-show_entries")
        .arg("stream=codec_type,index,width,height,codec_name")
        .arg("-of")
        .arg("json")
        .arg(path.as_ref())
        .output()?;

    if !out.status.success() {
        return Err(io::Error::other(String::from_utf8_lossy(&out.stderr)));
    }

    serde_json::from_slice::<MediaInfo>(&out.stdout).map_err(io::Error::other)
}
