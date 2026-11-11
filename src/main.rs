use std::io::Write;
use std::process::ExitCode;

use clap::Parser;

use ltext::Error;
use ltext::document::{print_document, to_document};
use ltext::eval::{evaluate, free_vars, substitute};
use ltext::expr::{Expr, parse_expr, pp_expr};
use ltext::files::Loader;
use ltext::typecheck::{pp_type, type_of_top_level};

/// λtext - parameterized file evaluator
///
/// Evaluate EXPRESSION and send the result to stdout. Free variables in
/// EXPRESSION name files; a file whose first line is a header like
/// `{{ x y }}` is a function of its parameters. See https://ltext.github.io/
/// for more details.
#[derive(Parser, Debug)]
#[command(name = "ltext", version)]
struct Opts {
    /// Expression to evaluate, e.g. `greet name` or `\x -> wrap x`
    #[arg(value_name = "EXPRESSION")]
    expression: String,

    /// Print the type of EXPRESSION instead of evaluating it
    #[arg(short = 't', long = "type")]
    type_query: bool,

    /// Report what ltext is doing on stderr
    #[arg(short, long)]
    verbose: bool,

    /// Treat FILE as plain text, even if its first line looks like a header
    #[arg(short, long, value_name = "FILE")]
    raw: Vec<String>,

    /// Left delimiter for rendering a partially applied result
    #[arg(long, value_name = "LEFT", requires = "right")]
    left: Option<String>,

    /// Right delimiter for rendering a partially applied result
    #[arg(long, value_name = "RIGHT", requires = "left")]
    right: Option<String>,
}

fn main() -> ExitCode {
    let opts = match Opts::try_parse() {
        Ok(o) => o,
        Err(e) => {
            let _ = e.print();
            // --help and --version exit 0; usage errors exit 1, like every
            // other error.
            return if e.use_stderr() {
                ExitCode::FAILURE
            } else {
                ExitCode::SUCCESS
            };
        }
    };

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
    let mut files = Loader::new(&opts.raw, opts.verbose);
    if opts.verbose {
        eprintln!("[ltext] expression: {}", pp_expr(&expr));
    }

    let ty = type_of_top_level(&expr, &mut files)?;
    if opts.verbose {
        eprintln!("[ltext] type: {}", pp_type(&ty));
    }
    if opts.type_query {
        return Ok(format!("{}\n", pp_type(&ty)));
    }

    let resolved = resolve_top_level(expr, &mut files)?;
    let doc = to_document(&evaluate(&resolved))?;
    let delims = opts.left.as_deref().zip(opts.right.as_deref());
    Ok(print_document(delims, &doc)?)
}

/// Replace each free variable with the contents of the file it names.
fn resolve_top_level(e: Expr, files: &mut Loader) -> Result<Expr, Error> {
    let mut out = e.clone();
    for f in free_vars(&e) {
        out = substitute(&f, &files.load(&f)?, &out);
    }
    Ok(out)
}
