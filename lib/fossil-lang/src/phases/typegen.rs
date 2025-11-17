use crate::ast::{Ast, Type, TypeId};
use crate::context::Interner;
use crate::error::TypeGenError;
use crate::module::{Binding, ModuleRegistry};
use crate::phases::BindingRef;
use crate::phases::{ResolutionTable, ResolvedProgram};

pub struct TypeGenerator<'a> {
    registry: &'a ModuleRegistry,
}

impl<'a> TypeGenerator<'a> {
    pub fn new(registry: &'a ModuleRegistry) -> Self {
        Self { registry }
    }

    pub fn generate(self, program: ResolvedProgram) -> Result<ResolvedProgram, TypeGenError> {
        let ResolvedProgram {
            mut ast,
            mut symbols,
            resolution,
        } = program;

        let providers: Vec<_> = ast
            .types
            .iter()
            .filter_map(|(ty_id, ty)| match ty {
                Type::Provider { .. } => Some(ty_id),
                _ => None,
            })
            .collect();

        for id in providers {
            self.generate_provider(id, &mut ast, &mut symbols, &resolution)?;
        }

        Ok(ResolvedProgram {
            ast,
            symbols,
            resolution,
        })
    }

    fn generate_provider(
        &self,
        ty: TypeId,
        ast: &mut Ast,
        symbols: &mut Interner,
        resolution: &ResolutionTable,
    ) -> Result<(), TypeGenError> {
        let args = match ast.types.get(ty) {
            Type::Provider { args, .. } => args.clone(),
            _ => unreachable!(),
        };

        let binding = match resolution.providers.get(&ty) {
            Some(BindingRef::Module(id)) => *id,
            _ => unreachable!(),
        };

        let provider = match self.registry.get(binding) {
            Binding::Provider(p) => p,
            _ => unreachable!("Resolver validated this"),
        };

        let generated_type = provider.generate(&args, ast, symbols)?;

        *ast.types.get_mut(ty) = generated_type;

        Ok(())
    }
}
