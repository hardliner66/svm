use core::fmt;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;
use core::num::NonZeroU64;

use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

pub type HandleMap<T> = IndexMap<Handle<T>, T>;

#[repr(transparent)]
#[derive(Copy, Clone, Serialize, Deserialize)]
pub struct Handle<T> {
    id: NonZeroU64,
    _marker: PhantomData<fn() -> T>,
}

impl<T> Handle<T> {
    #[inline]
    pub fn new(id: NonZeroU64) -> Self {
        Self {
            id,
            _marker: PhantomData,
        }
    }

    #[inline]
    pub fn id(self) -> NonZeroU64 {
        self.id
    }
}

impl<T> PartialEq for Handle<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}
impl<T> Eq for Handle<T> {}

impl<T> PartialOrd for Handle<T> {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl<T> Ord for Handle<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.id.cmp(&other.id)
    }
}

impl<T> Hash for Handle<T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.id.hash(state)
    }
}

impl<T> fmt::Debug for Handle<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple(&format!("Handle<{}>", std::any::type_name::<T>()))
            .field(&self.id)
            .finish()
    }
}

impl<T> fmt::Display for Handle<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.id.fmt(f)
    }
}

impl<T> From<NonZeroU64> for Handle<T> {
    fn from(id: NonZeroU64) -> Self {
        Self::new(id)
    }
}
impl<T> TryFrom<u64> for Handle<T> {
    type Error = core::num::TryFromIntError;
    fn try_from(raw: u64) -> Result<Self, Self::Error> {
        NonZeroU64::try_from(raw).map(Self::new)
    }
}
