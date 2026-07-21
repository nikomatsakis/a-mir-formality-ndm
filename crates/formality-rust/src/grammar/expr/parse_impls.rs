//! Handwritten parsing for function values.

use formality_core::parse::{
    ActiveVariant, CoreParse, ParseResult, ParseSuccessType, Parser, Precedence, Scope,
};
use formality_core::Upcast;

use crate::grammar::{Parameter, TraitId, Ty, ValueId};
use crate::rust::FormalityLang as Rust;

use super::{FnName, FnValue, Turbofish};

impl CoreParse<Rust> for FnValue {
    fn parse<'t>(scope: &Scope<Rust>, text: &'t str) -> ParseResult<'t, Self> {
        Parser::multi_variant(scope, text, "FnValue", |parser| {
            parser.parse_variant("qualified", Precedence::default(), |p| {
                p.expect_char('<')?;
                p.each_nonterminal(|self_ty: Ty, p| {
                    p.expect_keyword("as")?;
                    p.each_nonterminal(|trait_id: TraitId, p| {
                        each_parse_parameters(p, |trait_arguments, p| {
                            p.expect_char('>')?;
                            p.expect_char(':')?;
                            p.expect_char(':')?;
                            p.each_nonterminal(|method_id: ValueId, p| {
                                p.each_opt_nonterminal(|method_arguments: Option<Turbofish>, p| {
                                    let mut substitution = Vec::with_capacity(
                                        1 + trait_arguments.len()
                                            + method_arguments
                                                .as_ref()
                                                .map_or(0, |a| a.parameters.len()),
                                    );
                                    substitution.push(self_ty.clone().upcast());
                                    substitution.extend(trait_arguments.clone());
                                    if let Some(method_arguments) = method_arguments {
                                        substitution.extend(method_arguments.parameters);
                                    }

                                    p.ok(FnValue::new(
                                        FnName::qualified_id(&trait_id, &method_id),
                                        substitution,
                                    ))
                                })
                            })
                        })
                    })
                })
            });

            parser.parse_variant("free", Precedence::default(), |p| {
                p.each_nonterminal(|id: ValueId, p| {
                    p.each_nonterminal(|arguments: Turbofish, p| {
                        p.ok(FnValue::new(FnName::free_id(&id), arguments.parameters))
                    })
                })
            });
        })
    }
}

fn each_parse_parameters<'s, 't, R: ParseSuccessType>(
    p: &mut ActiveVariant<'s, 't, Rust>,
    op: impl Fn(Vec<Parameter>, &mut ActiveVariant<'s, 't, Rust>) -> ParseResult<'t, R>,
) -> ParseResult<'t, R> {
    if p.expect_char('<').is_err() {
        return op(vec![], p);
    }

    p.each_comma_nonterminal(|parameters: Vec<Parameter>, p| {
        p.expect_char('>')?;
        op(parameters, p)
    })
}
