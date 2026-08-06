use std::fmt::{self, Debug, Formatter};

use super::{Parameter, TraitRef};

impl Debug for TraitRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let TraitRef {
            trait_id,
            parameters,
        } = self;
        let (self_parameter, trait_parameters) = parameters
            .split_first()
            .expect("trait references always have a `Self` parameter");

        write!(f, "{self_parameter:?}: {trait_id:?}")?;
        write!(f, "{:?}", PrettyParameters(trait_parameters))
    }
}

struct PrettyParameters<'a>(&'a [Parameter]);

impl Debug for PrettyParameters<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let Some((first, rest)) = self.0.split_first() else {
            return Ok(());
        };

        write!(f, "<{first:?}")?;
        for parameter in rest {
            write!(f, ", {parameter:?}")?;
        }
        write!(f, ">")
    }
}
