use fossil_lang::context::extract_type_metadata;
use fossil_lang::error::FossilErrors;
use fossil_lang::passes::IrProgram;
use fossil_lang::passes::convert::ast_to_ir;
use fossil_lang::passes::expand::ProviderExpander;
use fossil_lang::passes::parse::Parser;
use fossil_lang::passes::resolve::IrResolver;
use fossil_lang::passes::typecheck::TypeChecker;

pub fn compile(src: &str) -> Result<IrProgram, FossilErrors> {
    let parsed = Parser::parse(src, 0)?;
    let expand = ProviderExpander::new((parsed.ast, parsed.gcx)).expand()?;
    let ty = extract_type_metadata(&expand.ast);
    let ir = ast_to_ir(expand.ast);
    let (ir, gcx) = IrResolver::new(ir, expand.gcx)
        .with_type_metadata(ty)
        .resolve()?;
    let program = TypeChecker::new(ir, gcx).check()?;
    Ok(program)
}
