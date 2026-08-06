use formality_core::{
    parse::{CoreParse, ParseResult, Parser, Scope},
    Upcast,
};

use crate::rust::FormalityLang as Rust;

use super::{Parameter, TraitId, TraitRef, Ty};

impl CoreParse<Rust> for TraitRef {
    fn parse<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::single_variant(scope, text, "TraitRef", |p| {
            p.each_nonterminal(|self_ty: Ty, p| {
                p.expect_char(':')?;
                p.each_nonterminal(|trait_id: TraitId, p| {
                    p.each_delimited_nonterminal(
                        '<',
                        true,
                        '>',
                        |trait_parameters: Vec<Parameter>, p| {
                            let parameters = std::iter::once(self_ty.clone().upcast())
                                .chain(trait_parameters)
                                .collect();
                            p.ok(TraitRef {
                                trait_id: trait_id.clone(),
                                parameters,
                            })
                        },
                    )
                })
            })
        })
    }
}
