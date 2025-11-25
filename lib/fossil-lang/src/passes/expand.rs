use crate::ast::ast::*;
use crate::context::DefKind;
use crate::error::{CompileError, CompileErrorKind};
use crate::passes::resolve::ResolvedAst;

pub struct ProviderExpander {
    resolved: ResolvedAst,
}

impl ProviderExpander {
    pub fn new(resolved: ResolvedAst) -> Self {
        Self { resolved }
    }

    pub fn expand(mut self) -> Result<ResolvedAst, CompileError> {
        let provider_types: Vec<TypeId> = self
            .resolved
            .ast
            .types
            .iter()
            .filter_map(|(id, ty)| match &ty.kind {
                TypeKind::Provider { .. } => Some(id),
                _ => None,
            })
            .collect();

        for type_id in provider_types {
            self.expand_provider(type_id)?;
        }

        Ok(self.resolved)
    }

    fn expand_provider(&mut self, type_id: TypeId) -> Result<(), CompileError> {
        let (path, args, loc) = {
            let ty = self.resolved.ast.types.get(type_id);
            match &ty.kind {
                TypeKind::Provider { provider, args } => {
                    (provider.clone(), args.clone(), ty.loc.clone())
                }

                _ => return Ok(()),
            }
        };

        let def_id = self.resolved.gcx.definitions.resolve(path).ok_or_else(|| {
            CompileError::new(CompileErrorKind::UndefinedProvider(path), loc.clone())
        })?;

        let provider_impl = match self.resolved.gcx.definitions.get(def_id).kind {
            DefKind::Provider(provider) => provider.clone(),

            _ => {
                return Err(CompileError::new(CompileErrorKind::NotAProvider(path), loc));
            }
        };

        let generated_kind = provider_impl.generate(&args, &mut self.resolved.gcx.interner)?;

        let ty = self.resolved.ast.types.get_mut(type_id);
        ty.kind = generated_kind;

        Ok(())
    }
}
