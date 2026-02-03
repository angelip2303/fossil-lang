use std::collections::HashMap;
use std::fs::read_to_string;
use std::path::PathBuf;

use ariadne::{Cache, Source};
use clap::{Parser, Subcommand};

use fossil_lang::ast::Loc;
use fossil_lang::compiler::{CompileResult, Compiler, CompilerInput};
use fossil_lang::error::{CompileError, CompileErrors};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::IrExecutor;

struct SourceCache {
    sources: HashMap<usize, Source<String>>,
}

impl SourceCache {
    fn new(source_id: usize, content: String) -> Self {
        let mut sources = HashMap::new();
        sources.insert(source_id, Source::from(content));
        Self { sources }
    }
}

impl Cache<usize> for SourceCache {
    type Storage = String;

    #[allow(refining_impl_trait)]
    fn fetch(
        &mut self,
        id: &usize,
    ) -> Result<&Source<Self::Storage>, Box<dyn std::fmt::Debug + '_>> {
        self.sources
            .get(id)
            .ok_or_else(|| Box::new(format!("Source {} not found", id)) as Box<dyn std::fmt::Debug>)
    }

    #[allow(refining_impl_trait)]
    fn display<'a>(&self, id: &'a usize) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(*id))
    }
}

#[derive(Parser)]
#[command(name = "fossil")]
#[command(about = "Fossil - A small language for living data")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a Fossil script
    Compile {
        /// Path to the Fossil script to compile
        #[arg(short, long)]
        path: PathBuf,
    },
    Run {
        /// Path to the Fossil script to run
        #[arg(short, long)]
        path: PathBuf,
    },
}

fn main() -> Result<(), CompileErrors> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Compile { path } => {
            let _ = compile(path)?;
        }
        Commands::Run { path } => {
            let _ = run(path)?;
        }
    };

    Ok(())
}

fn compile(path: PathBuf) -> Result<CompileResult, CompileErrors> {
    let mut gcx = GlobalContext::default();
    gcx.register_provider("csv", fossil_providers::csv::CsvProvider);
    gcx.register_provider("shex", fossil_providers::shapes::shex::ShexProvider);
    fossil_stdlib::init(&mut gcx);

    let source_content =
        read_to_string(&path).map_err(|e| CompileError::from_io(e, Loc::generated()))?;

    let mut cache = SourceCache::new(0, source_content);

    let compiler = Compiler::with_context(gcx);
    let result = compiler.compile(CompilerInput::File(path))?;

    if !result.warnings.is_empty() {
        for report in result.warnings.reports() {
            report.eprint(&mut cache).expect("failed to print warning");
        }
    }

    Ok(result)
}

fn run(path: PathBuf) -> Result<(), CompileErrors> {
    let result = compile(path)?;
    IrExecutor::execute(result.program)?;
    Ok(())
}
