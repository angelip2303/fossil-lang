use std::fs::File;
use std::path::PathBuf;

use fossil_lang::compiler::{Compiler, CompilerInput};
use fossil_lang::passes::GlobalContext;
use fossil_lang::runtime::executor::IrExecutor;
use oxrdfio::RdfParser;

use crate::TestSuiteError;

fn test(test_case: &str) -> Result<bool, TestSuiteError> {
    let mut gcx = GlobalContext::new();
    gcx.register_provider("csv", fossil_providers::csv::CsvProvider);
    fossil_stdlib::init(&mut gcx);

    let compiler = Compiler::with_context(gcx);
    let mapping = PathBuf::from(format!("tests/{}/mapping.fossil", test_case));
    let program = compiler.compile(CompilerInput::File(mapping))?;
    let _ = IrExecutor::execute(program)?;

    let actual = RdfParser::from_format(oxrdfio::RdfFormat::NQuads)
        .for_reader(File::open(format!("tests/{}/actual.nq", test_case))?)
        .collect::<Result<Vec<_>, _>>()?;

    let expected = RdfParser::from_format(oxrdfio::RdfFormat::NQuads)
        .for_reader(File::open(format!("tests/{}/output.nq", test_case))?)
        .collect::<Result<Vec<_>, _>>()?;

    Ok(actual == expected)
}

pub fn test_positive(test_case: &str) -> Result<(), TestSuiteError> {
    match test(test_case)? {
        true => Ok(()),
        false => Err(TestSuiteError::NotEquals),
    }
}

pub fn test_negative(test_case: &str) -> Result<(), TestSuiteError> {
    match test(test_case)? {
        true => Err(TestSuiteError::NotEquals),
        false => Ok(()),
    }
}
