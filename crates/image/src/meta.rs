use indexmap::IndexMap;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
pub enum Value {
    String(String),
    U128(u128),
    U64(u64),
    U32(u32),
    U16(u16),
    U8(u8),
    I128(i128),
    I64(i64),
    I32(i32),
    I16(i16),
    I8(i8),
    F64(f64),
    F32(f32),
    Bool(bool),
    Char(char),
    Map(MetaData),
    List(Vec<Value>),
}

pub type MetaData = IndexMap<String, Value>;

#[derive(Serialize, Deserialize)]
pub struct TypedMetaData<T> {
    pub data: T,
    pub other: MetaData,
}

impl<T: Default> Default for TypedMetaData<T> {
    fn default() -> Self {
        Self {
            data: Default::default(),
            other: Default::default(),
        }
    }
}
