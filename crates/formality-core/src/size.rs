use std::{cell::Cell, marker::PhantomData, rc::Rc, sync::Arc};

use crate::Set;

/// Measures how much a value contributes to the size of a judgment input.
///
/// Terms derive this trait structurally, but other judgment inputs can implement
/// it directly. Context-like inputs that should not contribute to overflow
/// detection can return zero. Implementations that combine child contributions
/// should use saturating arithmetic.
pub trait Size {
    fn size(&self) -> usize;
}

/// The maximum judgment-input size used when no explicit cutoff is installed.
pub const DEFAULT_CUTOFF: usize = 1_000;

thread_local! {
    /// `None` outside judgment evaluation; `Some` throughout one outermost
    /// judgment evaluation or an explicit `with_cutoff` scope.
    static CUTOFF: Cell<Option<usize>> = const { Cell::new(None) };
}

/// Returns the cutoff active for this thread.
///
/// Outside a judgment evaluation or [`with_cutoff`] scope, this returns
/// [`DEFAULT_CUTOFF`].
#[doc(hidden)]
pub fn cutoff() -> usize {
    CUTOFF.with(|cutoff| cutoff.get().unwrap_or(DEFAULT_CUTOFF))
}

/// Executes `op` with an explicit judgment-input cutoff.
///
/// Cutoff scopes must be established outside judgment evaluation and cannot be
/// nested. The setting is thread-local and is not inherited by threads spawned
/// from `op`. The previous thread-local state is restored if `op` unwinds.
///
/// # Panics
///
/// Panics if `cutoff` is zero or another cutoff/judgment context is already
/// active on this thread.
#[track_caller]
pub fn with_cutoff<T>(cutoff: usize, op: impl FnOnce() -> T) -> T {
    assert!(cutoff > 0, "the judgment cutoff must be greater than zero");
    let _guard = CutoffGuard::install(cutoff);
    op()
}

/// Establishes the default cutoff for an outermost judgment evaluation.
///
/// Generated judgment functions call this on entry. Nested judgments inherit
/// the already-active cutoff and receive a no-op guard. This function is public
/// only because exported macro expansions must be able to call it.
#[doc(hidden)]
pub fn enter_judgment() -> CutoffGuard {
    CUTOFF.with(|cutoff| {
        if cutoff.get().is_some() {
            CutoffGuard {
                owns_context: false,
                not_send_or_sync: PhantomData,
            }
        } else {
            cutoff.set(Some(DEFAULT_CUTOFF));
            CutoffGuard {
                owns_context: true,
                not_send_or_sync: PhantomData,
            }
        }
    })
}

/// Restores the thread-local cutoff when an owning scope exits.
///
/// This type is public only because exported macro expansions hold the value
/// returned by [`enter_judgment`].
#[doc(hidden)]
#[must_use = "the cutoff guard must remain alive for the judgment evaluation"]
pub struct CutoffGuard {
    owns_context: bool,
    not_send_or_sync: PhantomData<Rc<()>>,
}

impl CutoffGuard {
    #[track_caller]
    fn install(cutoff: usize) -> Self {
        CUTOFF.with(|current| {
            assert!(
                current.get().is_none(),
                "cannot adjust the judgment cutoff within an active cutoff context"
            );
            current.set(Some(cutoff));
        });
        Self {
            owns_context: true,
            not_send_or_sync: PhantomData,
        }
    }
}

impl Drop for CutoffGuard {
    fn drop(&mut self) {
        if self.owns_context {
            CUTOFF.with(|cutoff| {
                assert!(
                    cutoff.get().is_some(),
                    "judgment cutoff context ended without an active cutoff"
                );
                cutoff.set(None);
            });
        }
    }
}

impl<T: Size> Size for Vec<T> {
    fn size(&self) -> usize {
        self.iter()
            .fold(0, |size, item| size.saturating_add(item.size()))
    }
}

impl<T: Size + Ord> Size for Set<T> {
    fn size(&self) -> usize {
        self.iter()
            .fold(0, |size, item| size.saturating_add(item.size()))
    }
}

impl<T: Size> Size for Option<T> {
    fn size(&self) -> usize {
        self.as_ref().map(Size::size).unwrap_or(0)
    }
}

impl<T: Size + ?Sized> Size for Arc<T> {
    fn size(&self) -> usize {
        T::size(self)
    }
}

macro_rules! size_one {
    ($($ty:ty),* $(,)?) => {
        $(
            impl Size for $ty {
                fn size(&self) -> usize {
                    1
                }
            }
        )*
    };
}

size_one!(bool, usize, u8, u16, u32, u64, i8, i16, i32, i64, isize);

impl Size for u128 {
    fn size(&self) -> usize {
        std::mem::size_of::<Self>()
    }
}

impl Size for () {
    fn size(&self) -> usize {
        0
    }
}

impl<A: Size, B: Size> Size for (A, B) {
    fn size(&self) -> usize {
        let (a, b) = self;
        a.size().saturating_add(b.size())
    }
}

impl<A: Size, B: Size, C: Size> Size for (A, B, C) {
    fn size(&self) -> usize {
        let (a, b, c) = self;
        a.size().saturating_add(b.size()).saturating_add(c.size())
    }
}

impl<A: Size + ?Sized> Size for &A {
    fn size(&self) -> usize {
        A::size(self)
    }
}

impl<A: Size> Size for [A] {
    fn size(&self) -> usize {
        self.iter()
            .fold(0, |size, item| size.saturating_add(item.size()))
    }
}

#[cfg(test)]
mod tests {
    use std::panic::{catch_unwind, AssertUnwindSafe};

    use super::*;

    struct Maximum;

    impl Size for Maximum {
        fn size(&self) -> usize {
            usize::MAX
        }
    }

    #[test]
    fn default_cutoff_is_used_outside_a_context() {
        assert_eq!(cutoff(), DEFAULT_CUTOFF);
    }

    #[test]
    fn custom_cutoff_is_scoped() {
        with_cutoff(22, || assert_eq!(cutoff(), 22));
        assert_eq!(cutoff(), DEFAULT_CUTOFF);
    }

    #[test]
    #[should_panic(expected = "the judgment cutoff must be greater than zero")]
    fn zero_cutoff_is_rejected() {
        with_cutoff(0, || {});
    }

    #[test]
    fn nested_cutoff_is_rejected_without_changing_the_outer_cutoff() {
        with_cutoff(22, || {
            let result = catch_unwind(AssertUnwindSafe(|| with_cutoff(44, || {})));
            assert!(result.is_err());
            assert_eq!(cutoff(), 22);
        });
        assert_eq!(cutoff(), DEFAULT_CUTOFF);
    }

    #[test]
    fn active_judgment_context_rejects_cutoff_adjustment() {
        let _guard = enter_judgment();
        let result = catch_unwind(AssertUnwindSafe(|| with_cutoff(22, || {})));
        assert!(result.is_err());
        assert_eq!(cutoff(), DEFAULT_CUTOFF);
    }

    #[test]
    fn unwinding_restores_the_default_cutoff() {
        let result = catch_unwind(AssertUnwindSafe(|| {
            with_cutoff(22, || panic!("stop"));
        }));
        assert!(result.is_err());
        assert_eq!(cutoff(), DEFAULT_CUTOFF);
    }

    #[test]
    fn unwinding_restores_the_outermost_judgment_context() {
        let result = catch_unwind(AssertUnwindSafe(|| {
            let _guard = enter_judgment();
            panic!("stop");
        }));
        assert!(result.is_err());

        with_cutoff(22, || assert_eq!(cutoff(), 22));
        assert_eq!(cutoff(), DEFAULT_CUTOFF);
    }

    #[test]
    fn structural_sizes_saturate() {
        assert_eq!(vec![Maximum, Maximum].size(), usize::MAX);
        assert_eq!((Maximum, 0_u32).size(), usize::MAX);
    }
}
