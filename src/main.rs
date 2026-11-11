use std::collections::HashSet;
use std::io::Write;
use std::process::ExitCode;

use clap::Parser;

use ltext::Error;
use ltext::document::{fetch_document, print_document, raw_document, to_document};
use ltext::eval::{evaluate, free_vars, substitute};
use ltext::expr::{Expr, parse_expr};
use ltext::typecheck::{pp_type, type_of_top_level};

/// Matches the Haskell build, which also reports this outdated number
/// (docs/rust-port.md, bug #2).
const VERSION_STRING: &str = "0.1.3";

/// The Haskell build's optparse-applicative help text, reproduced exactly.
const HELP: &str = "\
λtext - parametrized file evaluator

Usage: ltext EXPRESSION [--version] [-t|--type] [-v|--verbose] [-r|--raw FILE]\u{20}
             [--left LEFT] [--right RIGHT]

  Evaluate EXPRESSION and send the substitution to stdout. See
  http://ltext.github.io/ for more details.

Available options:
  -h,--help                Show this help text
  --version                Print the version number
  -t,--type                Perform type inference on an expression
  -v,--verbose             Be verbose, sending info through stderr
  -r,--raw FILE            Treat these files as plaintext without an arity
                           header
  --left LEFT              The left delimiter to use for rendering partially
                           applied files
  --right RIGHT            The right delimiter to use for rendering partially
                           applied files";

#[derive(Parser, Debug)]
#[command(name = "ltext", override_help = HELP, disable_version_flag = true)]
struct Opts {
    #[arg(value_name = "EXPRESSION")]
    expression: String,

    /// Print the version number
    #[arg(long)]
    version: bool,

    /// Perform type inference on an expression
    #[arg(short = 't', long = "type")]
    type_query: bool,

    /// Be verbose, sending info through stderr
    #[arg(short, long)]
    verbose: bool,

    /// Treat these files as plaintext without an arity header
    #[arg(short, long, value_name = "FILE")]
    raw: Vec<String>,

    /// The left delimiter to use for rendering partially applied files
    #[arg(long, value_name = "LEFT")]
    left: Option<String>,

    /// The right delimiter to use for rendering partially applied files
    #[arg(long, value_name = "RIGHT")]
    right: Option<String>,
}

fn main() -> ExitCode {
    let opts = match Opts::try_parse() {
        Ok(o) => o,
        Err(e) => {
            let _ = e.print();
            // Help goes to stdout with exit 0; every usage error exits 1, as in
            // the Haskell build (clap's default would be 2).
            return if e.use_stderr() {
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            };
        }
    };

    if opts.version {
        eprintln!("Version: {VERSION_STRING}");
        return ExitCode::SUCCESS;
    }

    match run(&opts) {
        Ok(out) => {
            let mut stdout = std::io::stdout().lock();
            let _ = stdout
                .write_all(out.as_bytes())
                .and_then(|()| stdout.flush());
            ExitCode::SUCCESS
        }
        Err(e) => {
            eprintln!("{e}");
            ExitCode::FAILURE
        }
    }
}

fn run(opts: &Opts) -> Result<String, Error> {
    let expr = parse_expr(&opts.expression)?;
    let raw: HashSet<String> = opts.raw.iter().cloned().collect();

    let ty = type_of_top_level(&expr, &raw)?;
    if opts.type_query {
        return Ok(format!("{}\n", pp_type(&ty)));
    }

    let resolved = resolve_top_level(expr, &raw)?;
    let doc = to_document(&evaluate(&resolved))?;
    let delims = match (&opts.left, &opts.right) {
        (Some(l), Some(r)) => Some((l.as_str(), r.as_str())),
        _ => None,
    };
    Ok(print_document(delims, &doc)?)
}

/// Replace each free variable with the contents of the file it names.
fn resolve_top_level(e: Expr, raw: &HashSet<String>) -> Result<Expr, Error> {
    let mut out = e.clone();
    for f in free_vars(&e) {
        let d = if raw.contains(&f) {
            raw_document(&f)?
        } else {
            fetch_document(&f)?
        };
        out = substitute(&f, &d, &out);
    }
    Ok(out)
}
