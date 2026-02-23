use fossil_lang::context::extract_type_metadata;
use fossil_lang::error::FossilErrors;
use fossil_lang::passes::IrProgram;
use fossil_lang::passes::expand::ProviderExpander;
use fossil_lang::passes::lower;
use fossil_lang::passes::parse::Parser;
use fossil_lang::passes::typecheck::TypeChecker;

pub fn compile(src: &str) -> Result<IrProgram, FossilErrors> {
    let parsed = Parser::parse(src, 0)?;
    let expand = ProviderExpander::new((parsed.ast, parsed.gcx)).expand()?;
    let ty = extract_type_metadata(&expand.ast);
    let (ir, gcx, resolutions) =
        lower::lower_with_metadata(expand.ast, expand.gcx, ty)?;
    let program = TypeChecker::new(ir, gcx, resolutions).check()?;
    Ok(program)
}
