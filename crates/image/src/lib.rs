pub mod error;
pub mod handle;
pub mod meta;

use std::{collections::HashMap, fs::File, path::Path};

use bincode::config::Configuration;
use serde::{Deserialize, Serialize};
use stringlit::s;

use crate::{
    error::{ImageReadError, ImageWriteError},
    handle::{Handle, HandleMap},
    meta::{TypedMetaData, Value},
};

#[derive(Serialize, Deserialize)]
pub struct SchedulerMetaData {}

#[derive(Serialize, Deserialize)]
pub struct Scheduler {
    pub meta: TypedMetaData<SchedulerMetaData>,
    pub mailboxes: HandleMap<Mailbox>,
    pub actors: HandleMap<Actor>,
}

#[derive(Serialize, Deserialize)]
pub struct ImageMetaData {
    pub identifier: String,
}

impl Default for ImageMetaData {
    fn default() -> Self {
        Self {
            identifier: s!("SVM"),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Image {
    pub meta: TypedMetaData<ImageMetaData>,
    pub schedulers: HandleMap<Scheduler>,
    pub behaviors: HandleMap<Behavior>,
}

impl Default for Image {
    fn default() -> Self {
        Self {
            meta: Default::default(),
            schedulers: Default::default(),
            behaviors: Default::default(),
        }
    }
}

#[derive(Serialize, Deserialize)]
pub struct Behavior {}

#[derive(Serialize, Deserialize)]
pub struct MailboxMetaData {}

#[derive(Serialize, Deserialize)]
pub struct Mailbox {
    pub meta: TypedMetaData<MailboxMetaData>,
    pub actor: Handle<Actor>,
}

#[derive(Serialize, Deserialize)]
pub struct ActorMetaData {}

pub type ActorState = HashMap<String, Value>;

#[derive(Serialize, Deserialize)]
pub struct Actor {
    pub meta: TypedMetaData<ActorMetaData>,
    pub state: ActorState,
    pub current: Behavior,
    pub beh: Handle<Behavior>,
    pub mailbox: Handle<Mailbox>,
}

impl Image {
    pub fn load<P: AsRef<Path>>(p: P) -> Result<Self, ImageReadError> {
        let mut f = File::create(p)?;
        let reader = std::io::BufReader::new(&mut f);
        bincode::serde::decode_from_reader(reader, Self::get_bincode_config())
            .map_err(ImageReadError::DecodeError)
    }

    pub fn store<P: AsRef<Path>>(&self, p: P) -> Result<(), ImageWriteError> {
        let mut f = File::create(p)?;
        let mut writer = std::io::BufWriter::new(&mut f);
        bincode::serde::encode_into_std_write(self, &mut writer, Self::get_bincode_config())
            .map_err(ImageWriteError::EncodeError)?;
        Ok(())
    }

    fn get_bincode_config() -> Configuration {
        bincode::config::standard()
    }
}
