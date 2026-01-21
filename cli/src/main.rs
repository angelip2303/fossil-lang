use std::env;
use std::fs;
use std::io::Write;
use std::path::PathBuf;

use clap::{Parser, Subcommand};
use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::error::CompileError;
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::ThirExecutor;

#[derive(Parser)]
#[command(name = "fossil")]
#[command(about = "Fossil - functional data transformation language")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a Fossil script
    Run {
        /// Path to the .fossil script
        script: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { script } => {
            if let Err(e) = run_script(&script) {
                eprintln!("Error: {e:?}");
                std::process::exit(1);
            }
        }
    }
}

#[derive(Debug)]
enum RunError {
    Io(std::io::Error),
    Compile(CompileError),
}

impl From<std::io::Error> for RunError {
    fn from(e: std::io::Error) -> Self {
        RunError::Io(e)
    }
}

impl From<CompileError> for RunError {
    fn from(e: CompileError) -> Self {
        RunError::Compile(e)
    }
}

fn run_script(script: &PathBuf) -> Result<(), RunError> {
    // 1. Read source file
    let source = fs::read_to_string(script)?;

    // 2. Expand environment variables
    let expanded = expand_env_vars(&source);

    // 3. Write to temp file (to use File mode which handles types correctly)
    let temp_dir = std::env::temp_dir();
    let temp_path = temp_dir.join("fossil_script.fossil");
    {
        let mut file = fs::File::create(&temp_path)?;
        file.write_all(expanded.as_bytes())?;
    }

    // 4. Setup compiler context
    let mut gcx = GlobalContext::new();
    gcx.register_provider("csv", fossil_providers::csv::CsvProvider);
    gcx.register_provider("sql", fossil_providers::sql::SqlProvider);
    gcx.register_provider("shex", fossil_providers::shex::ShexProvider);
    fossil_stdlib::init(&mut gcx);

    // 5. Compile using File mode
    let compiler = Compiler::with_context(gcx);
    let thir = compiler.compile(CompilerInput::File(temp_path))?;

    // 6. Execute
    ThirExecutor::execute(thir)?;

    Ok(())
}

/// Expand ${VAR} patterns with environment variables
fn expand_env_vars(source: &str) -> String {
    let re = regex::Regex::new(r"\$\{([^}]+)\}").unwrap();
    let mut result = source.to_string();

    for cap in re.captures_iter(source) {
        let full_match = &cap[0];
        let var_name = &cap[1];
        if let Ok(value) = env::var(var_name) {
            result = result.replace(full_match, &value);
        }
    }

    result
}
