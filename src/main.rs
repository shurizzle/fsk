#[cfg(unix)]
use std::os::unix::process::CommandExt;
use std::{
    cell::UnsafeCell,
    ffi::OsStr,
    mem::MaybeUninit,
    ops::Deref,
    path::Path,
    process::{Command, Stdio},
    str::FromStr,
};

use ffmpeg::{Dimension, get_video_info};
use fraction::Fraction;
use num_integer::Integer;
use tmp::TempFile;

mod ffmpeg;
mod fraction;
mod tmp;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NvencScale {
    Npp,
    Cuda,
}

impl NvencScale {
    pub const fn as_str(&self) -> &'static str {
        match *self {
            NvencScale::Npp => "scale_npp",
            NvencScale::Cuda => "scale_cuda",
        }
    }
}

impl std::fmt::Display for NvencScale {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_ref())
    }
}

impl AsRef<str> for NvencScale {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<OsStr> for NvencScale {
    #[inline]
    fn as_ref(&self) -> &OsStr {
        <Self as AsRef<str>>::as_ref(self).as_ref()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VideoRemuxPlan {
    Libx264,
    Nvenc,
}

fn check_libx264() -> std::io::Result<Option<VideoRemuxPlan>> {
    if !ffmpeg::encoders()?.video.contains_key("libx264") {
        return Ok(None);
    }
    if ffmpeg::filters()?.contains_key("scale") {
        Ok(Some(VideoRemuxPlan::Libx264))
    } else {
        Ok(None)
    }
}

fn check_nvenc() -> std::io::Result<Option<VideoRemuxPlan>> {
    if !ffmpeg::has_video_encoder("h264_nvenc")? {
        return Ok(None);
    }
    if ffmpeg::filters()?.contains_key("scale") {
        Ok(Some(VideoRemuxPlan::Nvenc))
    } else {
        Ok(None)
    }
}

macro_rules! plan_fallback {
    ($plan:ident, $($rest:ident),+ $(,)?) => {
        match $plan() {
            res @ ::std::result::Result::Ok(::std::option::Option::Some(_)) => res,
            ::std::result::Result::Ok(::std::option::Option::None) => plan_fallback!($($rest),+),
            other => other,
        }
    };
    ($plan:ident $(,)?) => {
        $plan()
    };
    () => {
        ::std::result::Result::Ok(::std::option::Option::None)
    };
}

macro_rules! hwaccels_fallback {
    ($accel:expr, $($rest:expr),+ $(,)?) => {{
        let accel: &str = $accel;
        match $crate::ffmpeg::has_hwaccel(accel) {
            ::std::result::Result::Ok(true) => ::std::result::Result::Ok(::std::option::Option::Some(accel)),
            ::std::result::Result::Ok(false) => hwaccels_fallback!($($rest),+),
            ::std::result::Result::Err(err) => ::std::result::Result::Err(err),
        }
    }};
    ($accel:expr $(,)?) => {{
        let accel: &str = $accel;
        $crate::ffmpeg::has_hwaccel(accel).map(|r| {
            if r {
                ::std::option::Option::Some(accel)
            } else {
                ::std::option::Option::None
            }
        })
    }};
    () => {
        ::std::result::Result::Ok(::std::option::Option::None)
    };
}

pub fn video_remux_plan() -> std::io::Result<VideoRemuxPlan> {
    {
        let Some(h264) = ffmpeg::codecs()?.video.get("h264") else {
            return Err(std::io::Error::other("h264 codec not found"));
        };
        if !h264.contains(ffmpeg::CodecFlags::ENCODING_SUPPORTED) {
            return Err(std::io::Error::other("h264 codec isn't supported"));
        }
    }

    let Some(plan) = plan_fallback!(check_nvenc, check_libx264)? else {
        return Err(std::io::Error::other("no known h264 encoders found"));
    };
    Ok(plan)
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum SubsPlan {
    MovText,
}

impl SubsPlan {
    pub const fn as_str(&self) -> &'static str {
        match self {
            SubsPlan::MovText => "mov_text",
        }
    }
}

impl AsRef<str> for SubsPlan {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<OsStr> for SubsPlan {
    #[inline]
    fn as_ref(&self) -> &OsStr {
        <Self as AsRef<str>>::as_ref(self).as_ref()
    }
}

pub fn subs_plan() -> std::io::Result<SubsPlan> {
    {
        let Some(mov_text) = ffmpeg::codecs()?.subtitle.get("mov_text") else {
            return Err(std::io::Error::other("mov_text codec not found"));
        };
        if !mov_text.contains(ffmpeg::CodecFlags::ENCODING_SUPPORTED) {
            return Err(std::io::Error::other("mov_text codec isn't supported"));
        }
    }
    if ffmpeg::encoders()?.subtitle.contains_key("mov_text") {
        Ok(SubsPlan::MovText)
    } else {
        Err(std::io::Error::other("no known mov_text encoders found"))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ScalePlan {
    Width(u32),
    Height(u32),
}

impl std::fmt::Display for ScalePlan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            ScalePlan::Width(w) => write!(f, "w={}:h=-1", w),
            ScalePlan::Height(h) => write!(f, "w=-1:h={}", h),
        }
    }
}

pub fn scale_into(vd: Dimension, d: Dimension) -> Option<ScalePlan> {
    if vd.height > d.height {
        let mut width: Fraction = d.height.into();
        width = width / vd.height * vd.width;
        if width <= d.width {
            return Some(ScalePlan::Height(d.height));
        }
    }
    if vd.width > d.width {
        Some(ScalePlan::Width(d.width))
    } else {
        None
    }
}

struct CachedResultInner<T, F> {
    f: Option<F>,
    value: MaybeUninit<T>,
}

impl<T, E, F> CachedResultInner<T, F>
where
    F: Fn() -> Result<T, E>,
{
    #[inline(always)]
    pub fn new(f: F) -> Self {
        Self {
            f: Some(f),
            value: MaybeUninit::uninit(),
        }
    }

    #[inline(always)]
    pub fn get(&mut self) -> Result<&T, E> {
        if let Some(f) = self.f.as_ref() {
            return match f() {
                Ok(v) => unsafe {
                    self.f = None;
                    self.value.write(v);
                    Ok(self.value.assume_init_ref())
                },
                Err(e) => Err(e),
            };
        }
        unsafe { Ok(self.value.assume_init_ref()) }
    }
}

impl<T, F> Drop for CachedResultInner<T, F> {
    fn drop(&mut self) {
        if let Some(f) = self.f.take() {
            drop(f);
            unsafe { std::ptr::drop_in_place(self.value.assume_init_mut()) };
        }
    }
}

struct CachedResult<T, F>(UnsafeCell<CachedResultInner<T, F>>);

impl<T, E, F> CachedResult<T, F>
where
    F: Fn() -> Result<T, E>,
{
    pub fn new(f: F) -> Self {
        Self(UnsafeCell::new(CachedResultInner::new(f)))
    }

    pub fn get(&self) -> Result<&T, E> {
        unsafe { (*self.0.get()).get() }
    }
}

impl<T, E, F> std::fmt::Debug for CachedResult<T, F>
where
    F: Fn() -> Result<T, E>,
    T: std::fmt::Debug,
    E: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("CachedResult").field(&self.get()).finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Percent(u8);

impl Percent {
    #[inline(always)]
    pub fn new(v: u8) -> Option<Self> {
        if v <= 100 { Some(Self(v)) } else { None }
    }

    #[inline(always)]
    pub fn of<V: Integer + From<u8>>(&self, v: V) -> V {
        v * V::from(self.0) / V::from(100u8)
    }
}

pub struct ParsePercentError;

impl FromStr for Percent {
    type Err = ParsePercentError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.parse::<u8>() {
            Ok(x) if x <= 100 => Ok(Percent(x)),
            _ => Err(ParsePercentError),
        }
    }
}

impl Deref for Percent {
    type Target = u8;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

pub struct Cli {
    pub verbose: u32,
    pub overwrite: bool,
    pub hide_banner: bool,
    pub crf: Percent,
    pub input: Box<Path>,
    pub output: Box<Path>,
}

fn usage<S: AsRef<str>>(progname: S, code: i32) -> ! {
    eprintln!(
        "Usage: {} [-y] [-hide_banner] [-crf n] <-i infile> <outfile>",
        progname.as_ref()
    );
    std::process::exit(code);
}

fn parse_options() -> Cli {
    let mut args = std::env::args();
    let Some(progname) = args.next() else {
        usage("fsk", 1);
    };

    let mut verbose = 0;
    let mut overwrite = false;
    let mut hide_banner = false;
    let mut crf = None;
    let mut input: Option<String> = None;
    let mut output: Option<String> = None;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "-v" => verbose += 1,
            "-y" => overwrite = true,
            "-hide_banner" => hide_banner = true,
            "-crf" => {
                if crf.is_some() {
                    eprintln!("crf defined multiple times");
                    usage(progname, 1);
                }
                let Some(i) = args.next() else {
                    eprintln!("-crf flag needs an argument");
                    usage(progname, 1);
                };
                let Ok(i) = i.parse() else {
                    eprintln!("crf must be a percentage");
                    usage(progname, 1);
                };
                crf = Some(i);
            }
            "-i" => {
                if input.is_some() {
                    eprintln!("multiple input files not supported");
                    usage(progname, 1);
                }
                let Some(i) = args.next() else {
                    eprintln!("-i flag needs an argument");
                    usage(progname, 1);
                };
                input = Some(i);
            }
            out => {
                if output.is_some() {
                    eprintln!("multiple output files not supported");
                    usage(progname, 1);
                }
                if args.next().is_some() {
                    eprintln!("outfile must be the last argument");
                    usage(progname, 1);
                }

                output = Some(out.into());
            }
        }
    }

    let Some(input) = input else {
        eprintln!("input file not specified");
        usage(progname, 1);
    };
    let input = match std::fs::canonicalize(input) {
        Ok(p) => p.into_boxed_path(),
        Err(err) => {
            eprintln!("{err}");
            usage(progname, 1);
        }
    };
    let Some(output) = output else {
        eprintln!("output file not specified");
        usage(progname, 1);
    };
    let delete = std::fs::File::create_new(&output).is_ok();
    let res = std::fs::canonicalize(&output);
    if delete {
        _ = std::fs::remove_file(output);
    }
    let output = match res {
        Ok(p) => p.into_boxed_path(),
        Err(err) => {
            eprintln!("{err}");
            usage(progname, 1);
        }
    };

    Cli {
        verbose,
        overwrite,
        hide_banner,
        input,
        output,
        crf: crf.unwrap_or(Percent(45)),
    }
}

#[inline]
fn crfperc_to_cuda_cq(x: u8) -> u32 {
    const A: u64 = 376;
    const B: u64 = 88650;
    const S: u64 = 100000;

    let x2 = x as u64 * x as u64;
    let num = B * x as u64 - A * x2;
    (num / S) as u32
}

fn main() -> std::io::Result<()> {
    let cli = parse_options();
    let video_plan = CachedResult::new(video_remux_plan);
    let subs_plan = CachedResult::new(subs_plan);
    let info = get_video_info(cli.input.as_ref())?;

    let is_mp4 = info.is_mp4();
    let needs_remux = info.streams.iter().any(|s| {
        if let ffmpeg::Stream::Video {
            codec, dimension, ..
        } = s
        {
            codec.as_ref() != "h264" || scale_into(*dimension, Dimension::D720P).is_some()
        } else {
            false
        }
    });

    if !is_mp4 || needs_remux {
        let tmpfile = if cli.input == cli.output {
            let tmpfile = TempFile::with_prefix_in(".fsk-", cli.output.parent().unwrap())?;
            Some(tmpfile)
        } else {
            None
        };
        let mut cmd = Command::new("ffmpeg");
        for _ in 0..cli.verbose.saturating_sub(1) {
            cmd.arg("-v");
        }
        if cli.hide_banner {
            cmd.arg("-hide_banner");
        }
        if cli.overwrite || tmpfile.is_some() {
            cmd.arg("-y");
        }
        cmd.arg("-threads").arg(num_cpus::get().to_string());
        if let Some(hwaccel) = hwaccels_fallback!("cuda", "videotoolbox", "vulkan")? {
            cmd.arg("-hwaccel").arg(hwaccel);
        }
        cmd.arg("-i")
            .arg(cli.input.as_ref())
            .arg("-fps_mode")
            .arg("passthrough")
            .arg("-map")
            .arg("0")
            .arg("-movflags")
            .arg("+use_metadata_tags")
            .arg("-map_metadata")
            .arg("0");

        for stream in &info.streams {
            match stream {
                ffmpeg::Stream::Unknown { index } => {
                    cmd.arg(format!("-c:{}", index)).arg("copy");
                }
                ffmpeg::Stream::Video {
                    index,
                    codec,
                    dimension,
                } => {
                    let scale = scale_into(*dimension, Dimension::D720P);
                    if codec.as_ref() != "h264" || scale.is_some() {
                        let video_plan = *video_plan.get()?;

                        match video_plan {
                            VideoRemuxPlan::Libx264 => {
                                cmd.arg(format!("-c:{}", index))
                                    .arg("libx264")
                                    .arg(format!("-preset:{}", index))
                                    .arg("veryslow")
                                    .arg(format!("-tune:{}", index))
                                    .arg("film")
                                    .arg(format!("-crf:{}", index)) // 0-63
                                    .arg(cli.crf.of(63u32).to_string());
                            }
                            VideoRemuxPlan::Nvenc => {
                                cmd.arg(format!("-c:{}", index))
                                    .arg("h264_nvenc")
                                    .arg(format!("-preset:{}", index))
                                    .arg("p7")
                                    .arg(format!("-tune:{}", index))
                                    .arg("hq")
                                    .arg(format!("-rc:{}", index))
                                    .arg("vbr")
                                    .arg(format!("-cq:{}", index)) // 0-51
                                    .arg(crfperc_to_cuda_cq(*cli.crf).to_string())
                                    .arg(format!("-profile:{}", index))
                                    .arg("high")
                                    .arg(format!("-bufsize:{}", index))
                                    .arg("8M")
                                    .arg(format!("-rc-lookahead:{}", index))
                                    .arg("32")
                                    .arg(format!("-level:{}", index))
                                    .arg("4.1");
                            }
                        }

                        if let Some(scale) = scale {
                            cmd.arg(format!("-vf:{}", index)).arg(format!(
                                "scale={}:flags=lanczos+accurate_rnd+full_chroma_int",
                                scale
                            ));
                        }

                        cmd.arg(format!("-b:{}", index)).arg("0");
                    } else {
                        cmd.arg(format!("-c:{}", index)).arg("copy");
                    }
                }
                ffmpeg::Stream::Audio { index, codec } => {
                    if codec.as_ref() == "mp3" || codec.as_ref() == "aac" {
                        cmd.arg(format!("-c:{}", index)).arg("copy");
                    } else {
                        cmd.arg(format!("-c:{}", index)).arg("aac");
                    }
                }
                ffmpeg::Stream::Data { index } => {
                    cmd.arg(format!("-c:{}", index)).arg("copy");
                }
                ffmpeg::Stream::Subtitle { index, codec } => {
                    if codec.as_ref() == "mov_text" {
                        cmd.arg(format!("-c:{}", index)).arg("copy");
                    } else {
                        cmd.arg(format!("-c:{}", index)).arg(*subs_plan.get()?);
                    }
                }
                ffmpeg::Stream::Attachment { index } => {
                    cmd.arg(format!("-c:{}", index)).arg("copy");
                }
            }
        }

        cmd.arg("-f").arg("mp4");
        if let Some(path) = tmpfile.as_ref() {
            cmd.arg(&**path);
        } else {
            cmd.arg(cli.output.as_ref());
        }

        if cli.verbose > 0 {
            eprintln!("{:?}", cmd);
        }

        cmd.stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit());

        #[cfg(not(unix))]
        {
            if !cmd.spawn()?.wait()?.success() {
                drop(tmpfile);
                std::process::exit(1);
            }
            if let Some(tmpfile) = tmpfile {
                std::fs::rename(&*tmpfile, cli.output.as_ref())?;
                drop(tmpfile);
            }
        }
        #[cfg(unix)]
        {
            if let Some(tmpfile) = tmpfile {
                if !cmd.spawn()?.wait()?.success() {
                    drop(tmpfile);
                    std::process::exit(1);
                }
                std::fs::rename(&*tmpfile, cli.output.as_ref())?;
                drop(tmpfile);
            } else {
                return Err(cmd.exec());
            }
        }
    } else if cli.input != cli.output {
        std::fs::rename(cli.input.as_ref(), cli.output.as_ref())?;
    }

    Ok(())
}
