use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::process::ExitCode;

fn parse_args(args: &[String]) -> Result<(usize, PathBuf), String> {
    let mut count = 100usize;
    let mut outdir = PathBuf::from("dummy-cobol");
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--count" => {
                i += 1;
                let raw = args
                    .get(i)
                    .ok_or_else(|| "missing value for --count".to_string())?;
                count = raw.parse().map_err(|_| format!("invalid --count: {raw}"))?;
            }
            "--outdir" => {
                i += 1;
                let raw = args
                    .get(i)
                    .ok_or_else(|| "missing value for --outdir".to_string())?;
                outdir = PathBuf::from(raw);
            }
            other => return Err(format!("unknown argument: {other}")),
        }
        i += 1;
    }
    Ok((count, outdir))
}

fn main() -> ExitCode {
    let args: Vec<String> = env::args().skip(1).collect();
    let (count, outdir) = match parse_args(&args) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("{e}");
            return ExitCode::from(1);
        }
    };
    if let Err(e) = fs::create_dir_all(&outdir) {
        eprintln!("failed to create {}: {e}", outdir.display());
        return ExitCode::from(1);
    }
    for i in 1..=count {
        let fname = outdir.join(format!("dummy{i:05}.cob"));
        match fs::File::create(&fname) {
            Ok(mut f) => {
                if let Err(e) = write!(
                    f,
                    "IDENTIFICATION DIVISION.\nPROGRAM-ID. DUMMY{i}.\nPROCEDURE DIVISION.\nSTOP RUN.\n"
                ) {
                    eprintln!("failed to write {}: {e}", fname.display());
                }
            }
            Err(e) => eprintln!("failed to create {}: {e}", fname.display()),
        }
    }
    println!(
        "Generated {count} dummy COBOL files in {}",
        outdir.display()
    );
    ExitCode::SUCCESS
}
