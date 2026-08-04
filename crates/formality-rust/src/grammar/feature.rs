use formality_core::term;

#[term(#![feature($name)])]
pub struct FeatureGate {
    pub name: FeatureGateName,
}

#[term(#![formality(max_size = $max_size)])]
pub struct FormalityConfig {
    pub max_size: usize,
}

#[term]
#[derive(Copy)]
pub enum FeatureGateName {
    #[grammar(polonius_alpha)]
    PoloniusAlpha,
    #[grammar(non_lifetime_binders)]
    NonLifetimeBinders,
}
