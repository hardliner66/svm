#![allow(unused)]

use std::{
    collections::HashMap,
    ffi::OsStr,
    path::PathBuf,
    time::{Duration, SystemTime},
};

use clap::{Parser, Subcommand};
use fuser::{
    FileAttr, FileType, Filesystem, MountOption, ReplyAttr, ReplyData, ReplyDirectory, ReplyEmpty,
    ReplyEntry, ReplyWrite, Request,
};
use libc::{EIO, ENOENT};
use svm_image::Image;

/// TTL for inode attributes in the kernel cache
const TTL: Duration = Duration::from_secs(1);

/// Top-level FS object
struct ImageFs {
    pub image: Image,

    /// Minimal inode -> identity cache so we can answer getattr/read/etc.
    inode_map: HashMap<u64, NodeIdent>,

    /// Buffered writes per (ino, fh) for atomic "compile on flush"
    write_bufs: HashMap<(u64, u64), Vec<u8>>,
}

impl ImageFs {
    fn new(image: Image) -> Self {
        let mut s = Self {
            image,
            inode_map: HashMap::new(),
            write_bufs: HashMap::new(),
        };

        // Seed well-known inodes
        s.inode_map.insert(ino_fixed(Kind::Root), NodeIdent::Root);
        s.inode_map
            .insert(ino_fixed(Kind::NsRoot), NodeIdent::NamespacesRoot);

        s
    }

    // ===== Helpers to build attributes =====

    fn attr_dir(&self, ino: u64) -> FileAttr {
        FileAttr {
            ino,
            size: 0,
            blocks: 0,
            atime: now(),
            mtime: now(),
            ctime: now(),
            crtime: now(),
            kind: FileType::Directory,
            perm: 0o755,
            nlink: 2,
            uid: unsafe { libc::geteuid() },
            gid: unsafe { libc::getegid() },
            rdev: 0,
            flags: 0,
            blksize: 0,
        }
    }

    fn attr_file(&self, ino: u64, size: u64, perm: u16) -> FileAttr {
        FileAttr {
            ino,
            size,
            blocks: size.div_ceil(512),
            atime: now(),
            mtime: now(),
            ctime: now(),
            crtime: now(),
            kind: FileType::RegularFile,
            perm,
            nlink: 1,
            uid: unsafe { libc::geteuid() },
            gid: unsafe { libc::getegid() },
            rdev: 0,
            flags: 0,
            blksize: 0,
        }
    }

    // ===== Directory population =====

    /// Add a directory entry, handling the running offset.
    fn add_dirent(
        &mut self,
        reply: &mut ReplyDirectory,
        offset: &mut i64,
        ino: u64,
        ftype: FileType,
        name: &str,
    ) {
        *offset += 1;
        _ = reply.add(ino, *offset, ftype, name);
    }

    // ===== Identity/ino constructors =====

    fn ns_ino(&self, ns_id: &[u8]) -> u64 {
        ino_bytes(Kind::Ns, &[b"ns", ns_id])
    }
    fn ns_meta_ino(&self, ns_id: &[u8]) -> u64 {
        ino_bytes(Kind::NsMeta, &[b"ns", ns_id, b"meta"])
    }
    fn actor_dir_ino(&self, ns_id: &[u8], act_id: &[u8]) -> u64 {
        ino_bytes(Kind::Actor, &[b"ns", ns_id, b"act", act_id])
    }
    fn actor_code_ino(&self, ns_id: &[u8], act_id: &[u8]) -> u64 {
        ino_bytes(Kind::Code, &[b"ns", ns_id, b"act", act_id, b"code"])
    }
    fn actor_state_ino(&self, ns_id: &[u8], act_id: &[u8]) -> u64 {
        ino_bytes(Kind::State, &[b"ns", ns_id, b"act", act_id, b"state"])
    }
    fn actor_mailbox_dir_ino(&self, ns_id: &[u8], act_id: &[u8]) -> u64 {
        ino_bytes(Kind::Mailbox, &[b"ns", ns_id, b"act", act_id, b"mbox"])
    }
    fn actor_children_dir_ino(&self, ns_id: &[u8], act_id: &[u8]) -> u64 {
        ino_bytes(Kind::Children, &[b"ns", ns_id, b"act", act_id, b"children"])
    }
    fn msg_file_ino(&self, ns_id: &[u8], act_id: &[u8], msg_id: u64) -> u64 {
        // mix actor identity with msg id so files are unique
        let id56 = hash_parts(&[fold_bytes_u64(ns_id), fold_bytes_u64(act_id), msg_id]) & MASK56;
        (id56 << 8) | Kind::Msg as u64
    }

    // ===== Utility: ensure cached identity =====

    fn remember(&mut self, ino: u64, ident: NodeIdent) {
        self.inode_map.entry(ino).or_insert(ident);
    }
}

/// The different “kinds” (low 8 bits of inode)
#[repr(u8)]
#[derive(Copy, Clone)]
enum Kind {
    Root = 1,
    NsRoot = 2,
    Ns = 3,

    Actor = 10,
    Code = 11,
    State = 12,
    Mailbox = 13,
    Msg = 14,
    Children = 15,
    // If you later want symlinks, you can re-enable a ChildLink kind here.
    NsMeta = 17,

    // Reserved for a future /actors/by-id view:
    ActorsById = 20,
    ActorById = 21,
}

/// Node identity we cache for getattr/read/write/etc.
#[derive(Clone)]
enum NodeIdent {
    Root,
    NamespacesRoot,
    Namespace {
        ns_id: Vec<u8>,
        name: String,
        parent: Option<Vec<u8>>,
    },
    NsMeta {
        ns_id: Vec<u8>,
    },
    ActorDir {
        ns_id: Vec<u8>,
        act_id: Vec<u8>,
        name: String,
    },
    ActorCode {
        ns_id: Vec<u8>,
        act_id: Vec<u8>,
    },
    ActorState {
        ns_id: Vec<u8>,
        act_id: Vec<u8>,
    },
    MailboxDir {
        ns_id: Vec<u8>,
        act_id: Vec<u8>,
    },
    MsgFile {
        ns_id: Vec<u8>,
        act_id: Vec<u8>,
        msg_id: u64,
        name: String,
    },
    ChildrenDir {
        ns_id: Vec<u8>,
        act_id: Vec<u8>,
    },
    ChildEntry {
        ns_id: Vec<u8>,
        act_id: Vec<u8>,
        child_name: String,
        child_id: Vec<u8>,
    },
}

// ======== hashing / inode derivation (no external deps) ========

const MASK56: u64 = 0x00FF_FFFF_FFFF_FFFF;

/// SplitMix64 mixer: tiny, fast, good avalanche (not crypto).
fn mix64(mut z: u64) -> u64 {
    z = z.wrapping_add(0x9E37_79B9_7F4A_7C15);
    z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
    z ^ (z >> 31)
}

/// Hash a list of u64 parts into a 64-bit number.
fn hash_parts(parts: &[u64]) -> u64 {
    let mut s = 0u64;
    for &p in parts {
        s = mix64(s ^ p);
    }
    s
}

/// Fold arbitrary bytes into a u64 with mixing.
fn fold_bytes_u64(bytes: &[u8]) -> u64 {
    let mut acc = 0u64;
    let mut chunk = [0u8; 8];
    for (i, b) in bytes.iter().enumerate() {
        let idx = i % 8;
        chunk[idx] = chunk[idx].wrapping_add(*b);
        if idx == 7 {
            acc = mix64(acc ^ u64::from_le_bytes(chunk));
        }
    }
    if bytes.len() % 8 != 0 {
        acc = mix64(acc ^ u64::from_le_bytes(chunk));
    }
    acc
}

/// Deterministic 56-bit hash over byte parts, used as high bits of inode.
fn h56_bytes(parts: &[&[u8]]) -> u64 {
    let mut s = 0u64;
    for p in parts {
        s = mix64(s ^ fold_bytes_u64(p));
    }
    s & MASK56
}

fn ino_bytes(kind: Kind, parts: &[&[u8]]) -> u64 {
    (h56_bytes(parts) << 8) | kind as u64
}

/// fixed inode helpers for well-known roots.
fn ino_fixed(kind: Kind) -> u64 {
    ((hash_parts(&[kind as u64, 0xdada_beef]) & MASK56) << 8) | kind as u64
}

#[inline]
fn now() -> SystemTime {
    SystemTime::now()
}

// ================== fuse impl ==================

impl Filesystem for ImageFs {
    fn lookup(&mut self, _req: &Request, parent: u64, name: &OsStr, reply: ReplyEntry) {
        let name_str = match name.to_str() {
            Some(s) => s,
            None => {
                reply.error(ENOENT);
                return;
            }
        };

        // resolve based on parent identity
        let Some(parent_ident) = self.inode_map.get(&parent).cloned() else {
            reply.error(ENOENT);
            return;
        };

        match match parent_ident {
            NodeIdent::Root => {
                if name_str == "namespaces" {
                    let ino = ino_fixed(Kind::NsRoot);
                    let attr = self.attr_dir(ino);
                    Some((ino, attr, NodeIdent::NamespacesRoot))
                } else {
                    None
                }
            }
            NodeIdent::NamespacesRoot => {
                // top-level namespace by name
                let (ns_id, disp_name) = match image_resolve_namespace(&self.image, None, name_str)
                {
                    Some(x) => x,
                    None => {
                        reply.error(ENOENT);
                        return;
                    }
                };
                let ino = self.ns_ino(&ns_id);
                let ident = NodeIdent::Namespace {
                    ns_id: ns_id.clone(),
                    name: disp_name,
                    parent: None,
                };
                let attr = self.attr_dir(ino);
                Some((ino, attr, ident))
            }
            NodeIdent::Namespace { ns_id, .. } => {
                if name_str == ".ns.json" {
                    let ino = self.ns_meta_ino(&ns_id);
                    let size = image_ns_meta_size(&self.image, &ns_id);
                    let attr = self.attr_file(ino, size, 0o644);
                    let ident = NodeIdent::NsMeta { ns_id };
                    Some((ino, attr, ident))
                } else if let Some((child_ns_id, disp_name)) =
                    image_resolve_namespace(&self.image, Some(&ns_id), name_str)
                {
                    let ino = self.ns_ino(&child_ns_id);
                    let ident = NodeIdent::Namespace {
                        ns_id: child_ns_id.clone(),
                        name: disp_name,
                        parent: Some(ns_id.clone()),
                    };
                    let attr = self.attr_dir(ino);
                    Some((ino, attr, ident))
                } else if let Some((act_id, act_name)) =
                    image_resolve_actor(&self.image, &ns_id, trim_actor_suffix(name_str))
                {
                    // actor dir is displayed as "<name>.act"
                    if !name_str.ends_with(".act") {
                        reply.error(ENOENT);
                        return;
                    }
                    let ino = self.actor_dir_ino(&ns_id, &act_id);
                    let ident = NodeIdent::ActorDir {
                        ns_id,
                        act_id: act_id.clone(),
                        name: format!("{}.act", act_name),
                    };
                    let attr = self.attr_dir(ino);
                    Some((ino, attr, ident))
                } else {
                    None
                }
            }
            NodeIdent::ActorDir { ns_id, act_id, .. } => match name_str {
                "code.st" => {
                    let ino = self.actor_code_ino(&ns_id, &act_id);
                    let size = image_actor_code_size(&self.image, &act_id);
                    let attr = self.attr_file(ino, size, 0o644);
                    let ident = NodeIdent::ActorCode { ns_id, act_id };
                    Some((ino, attr, ident))
                }
                "state.json" => {
                    let ino = self.actor_state_ino(&ns_id, &act_id);
                    let size = image_actor_state_size(&self.image, &act_id);
                    let attr = self.attr_file(ino, size, 0o644);
                    let ident = NodeIdent::ActorState { ns_id, act_id };
                    Some((ino, attr, ident))
                }
                "mailbox" => {
                    let ino = self.actor_mailbox_dir_ino(&ns_id, &act_id);
                    let attr = self.attr_dir(ino);
                    let ident = NodeIdent::MailboxDir { ns_id, act_id };
                    Some((ino, attr, ident))
                }
                "children" => {
                    let ino = self.actor_children_dir_ino(&ns_id, &act_id);
                    let attr = self.attr_dir(ino);
                    let ident = NodeIdent::ChildrenDir { ns_id, act_id };
                    Some((ino, attr, ident))
                }
                _ => None,
            },
            NodeIdent::MailboxDir { ns_id, act_id } => {
                if let Some((msg_id, disp_name)) =
                    image_resolve_message(&self.image, &act_id, name_str)
                {
                    let ino = self.msg_file_ino(&ns_id, &act_id, msg_id);
                    let size = image_message_size(&self.image, &act_id, msg_id);
                    let attr = self.attr_file(ino, size, 0o444);
                    let ident = NodeIdent::MsgFile {
                        ns_id,
                        act_id,
                        msg_id,
                        name: disp_name,
                    };
                    Some((ino, attr, ident))
                } else {
                    None
                }
            }
            NodeIdent::ChildrenDir { ns_id, act_id } => {
                if let Some((child_id, child_name)) =
                    image_resolve_child(&self.image, &act_id, name_str)
                {
                    // represent child entries as regular files (you can switch to symlinks later)
                    let id56 = h56_bytes(&[&ns_id, &act_id, &child_id]);
                    let ino = (id56 << 8) | Kind::Children as u64; // reuse kind for entries
                    let size = child_name.len() as u64;
                    let attr = self.attr_file(ino, size, 0o444);
                    let ident = NodeIdent::ChildEntry {
                        ns_id,
                        act_id,
                        child_name,
                        child_id,
                    };
                    Some((ino, attr, ident))
                } else {
                    None
                }
            }
            _ => None,
        } {
            Some((ino, attr_opt, ident_opt)) => {
                if ino != 0 {
                    self.remember(ino, ident_opt);
                    reply.entry(&TTL, &attr_opt, 0);
                }
            }
            None => {
                reply.error(ENOENT);
            }
        }
    }

    fn getattr(&mut self, _req: &Request<'_>, ino: u64, _fh: Option<u64>, reply: ReplyAttr) {
        let Some(ident) = self.inode_map.get(&ino).cloned() else {
            // Provide defaults for well-known roots even if cache was not primed
            if ino == ino_fixed(Kind::Root) {
                reply.attr(&TTL, &self.attr_dir(ino));
                return;
            }
            if ino == ino_fixed(Kind::NsRoot) {
                reply.attr(&TTL, &self.attr_dir(ino));
                return;
            }
            reply.error(ENOENT);
            return;
        };

        let attr = match ident {
            NodeIdent::Root | NodeIdent::NamespacesRoot => self.attr_dir(ino),
            NodeIdent::Namespace { .. } => self.attr_dir(ino),
            NodeIdent::ActorDir { .. } => self.attr_dir(ino),
            NodeIdent::MailboxDir { .. } | NodeIdent::ChildrenDir { .. } => self.attr_dir(ino),
            NodeIdent::NsMeta { ns_id } => {
                let size = image_ns_meta_size(&self.image, &ns_id);
                self.attr_file(ino, size, 0o644)
            }
            NodeIdent::ActorCode { act_id, .. } => {
                let size = image_actor_code_size(&self.image, &act_id);
                self.attr_file(ino, size, 0o644)
            }
            NodeIdent::ActorState { act_id, .. } => {
                let size = image_actor_state_size(&self.image, &act_id);
                self.attr_file(ino, size, 0o644)
            }
            NodeIdent::MsgFile { act_id, msg_id, .. } => {
                let size = image_message_size(&self.image, &act_id, msg_id);
                self.attr_file(ino, size, 0o444)
            }
            NodeIdent::ChildEntry { child_name, .. } => {
                self.attr_file(ino, child_name.len() as u64, 0o444)
            }
        };

        reply.attr(&TTL, &attr);
    }

    fn readdir(
        &mut self,
        _req: &Request,
        ino: u64,
        _fh: u64,
        mut offset: i64,
        mut reply: ReplyDirectory,
    ) {
        // Helper: add "." and ".." when offset < 1/2
        if offset == 0 {
            // "." (self)
            _ = reply.add(ino, 1, FileType::Directory, ".");
            offset = 1;
        }
        if offset == 1 {
            // ".." (we don't track parent ino here; kernel doesn't require accurate value)
            _ = reply.add(ino, 2, FileType::Directory, "..");
            offset = 2;
        }

        let Some(ident) = self.inode_map.get(&ino).cloned() else {
            reply.ok();
            return;
        };

        let mut cookie = offset;

        match ident {
            NodeIdent::Root => {
                let dino = ino_fixed(Kind::NsRoot);
                self.remember(dino, NodeIdent::NamespacesRoot);
                self.add_dirent(
                    &mut reply,
                    &mut cookie,
                    dino,
                    FileType::Directory,
                    "namespaces",
                );
            }
            NodeIdent::NamespacesRoot => {
                for (name, ns_id) in image_list_namespaces(&self.image, None)
                    .into_iter()
                    .skip((cookie - 2) as usize)
                {
                    let n_ino = self.ns_ino(&ns_id);
                    self.remember(
                        n_ino,
                        NodeIdent::Namespace {
                            ns_id: ns_id.clone(),
                            name: name.clone(),
                            parent: None,
                        },
                    );
                    self.add_dirent(&mut reply, &mut cookie, n_ino, FileType::Directory, &name);
                }
            }
            NodeIdent::Namespace { ns_id, .. } => {
                // 1) namespaces
                let subs = image_list_namespaces(&self.image, Some(&ns_id));
                for (name, child_ns_id) in subs {
                    let n_ino = self.ns_ino(&child_ns_id);
                    self.remember(
                        n_ino,
                        NodeIdent::Namespace {
                            ns_id: child_ns_id.clone(),
                            name: name.clone(),
                            parent: Some(ns_id.clone()),
                        },
                    );
                    self.add_dirent(&mut reply, &mut cookie, n_ino, FileType::Directory, &name);
                }
                // 2) actors
                for (act_name, act_id) in image_list_actors(&self.image, &ns_id) {
                    let dir_name = format!("{}.act", act_name);
                    let a_ino = self.actor_dir_ino(&ns_id, &act_id);
                    self.remember(
                        a_ino,
                        NodeIdent::ActorDir {
                            ns_id: ns_id.clone(),
                            act_id: act_id.clone(),
                            name: dir_name.clone(),
                        },
                    );
                    self.add_dirent(
                        &mut reply,
                        &mut cookie,
                        a_ino,
                        FileType::Directory,
                        &dir_name,
                    );
                }
                // 3) optional namespace meta
                let meta_ino = self.ns_meta_ino(&ns_id);
                self.remember(
                    meta_ino,
                    NodeIdent::NsMeta {
                        ns_id: ns_id.clone(),
                    },
                );
                self.add_dirent(
                    &mut reply,
                    &mut cookie,
                    meta_ino,
                    FileType::RegularFile,
                    ".ns.json",
                );
            }
            NodeIdent::ActorDir { ns_id, act_id, .. } => {
                // code.st
                let code_ino = self.actor_code_ino(&ns_id, &act_id);
                self.remember(
                    code_ino,
                    NodeIdent::ActorCode {
                        ns_id: ns_id.clone(),
                        act_id: act_id.clone(),
                    },
                );
                self.add_dirent(
                    &mut reply,
                    &mut cookie,
                    code_ino,
                    FileType::RegularFile,
                    "code.st",
                );

                // state.json
                let state_ino = self.actor_state_ino(&ns_id, &act_id);
                self.remember(
                    state_ino,
                    NodeIdent::ActorState {
                        ns_id: ns_id.clone(),
                        act_id: act_id.clone(),
                    },
                );
                self.add_dirent(
                    &mut reply,
                    &mut cookie,
                    state_ino,
                    FileType::RegularFile,
                    "state.json",
                );

                // mailbox/
                let mdir = self.actor_mailbox_dir_ino(&ns_id, &act_id);
                self.remember(
                    mdir,
                    NodeIdent::MailboxDir {
                        ns_id: ns_id.clone(),
                        act_id: act_id.clone(),
                    },
                );
                self.add_dirent(
                    &mut reply,
                    &mut cookie,
                    mdir,
                    FileType::Directory,
                    "mailbox",
                );

                // children/
                let cdir = self.actor_children_dir_ino(&ns_id, &act_id);
                self.remember(
                    cdir,
                    NodeIdent::ChildrenDir {
                        ns_id: ns_id.clone(),
                        act_id: act_id.clone(),
                    },
                );
                self.add_dirent(
                    &mut reply,
                    &mut cookie,
                    cdir,
                    FileType::Directory,
                    "children",
                );
            }
            NodeIdent::MailboxDir { ns_id, act_id } => {
                for (msg_id, fname) in image_list_messages(&self.image, &act_id) {
                    let m_ino = self.msg_file_ino(&ns_id, &act_id, msg_id);
                    self.remember(
                        m_ino,
                        NodeIdent::MsgFile {
                            ns_id: ns_id.clone(),
                            act_id: act_id.clone(),
                            msg_id,
                            name: fname.clone(),
                        },
                    );
                    self.add_dirent(
                        &mut reply,
                        &mut cookie,
                        m_ino,
                        FileType::RegularFile,
                        &fname,
                    );
                }
            }
            NodeIdent::ChildrenDir { ns_id, act_id } => {
                for (child_name, child_id) in image_list_children(&self.image, &act_id) {
                    // Represent as regular file; contents can be path/ID (see read())
                    let id56 = h56_bytes(&[&ns_id, &act_id, &child_id]);
                    let ch_ino = (id56 << 8) | Kind::Children as u64;
                    self.remember(
                        ch_ino,
                        NodeIdent::ChildEntry {
                            ns_id: ns_id.clone(),
                            act_id: act_id.clone(),
                            child_name: child_name.clone(),
                            child_id,
                        },
                    );
                    self.add_dirent(
                        &mut reply,
                        &mut cookie,
                        ch_ino,
                        FileType::RegularFile,
                        &child_name,
                    );
                }
            }
            _ => {}
        }

        reply.ok();
    }

    fn read(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        _fh: u64,
        offset: i64,
        size: u32,
        _flags: i32,
        _lock_owner: Option<u64>,
        reply: ReplyData,
    ) {
        let Some(ident) = self.inode_map.get(&ino).cloned() else {
            reply.error(ENOENT);
            return;
        };

        let bytes = match ident {
            NodeIdent::ActorCode { act_id, .. } => image_read_actor_code(&self.image, &act_id),
            NodeIdent::ActorState { act_id, .. } => image_read_actor_state(&self.image, &act_id),
            NodeIdent::MsgFile { act_id, msg_id, .. } => {
                image_read_message(&self.image, &act_id, msg_id)
            }
            NodeIdent::NsMeta { ns_id } => image_read_ns_meta(&self.image, &ns_id),
            NodeIdent::ChildEntry {
                child_id,
                child_name,
                ..
            } => {
                // Simple payload: "<child_name>\n<display_path_or_id>\n"
                let target = image_child_display_path(&self.image, &child_id);
                format!("{}\n{}\n", child_name, target).into_bytes()
            }
            _ => Vec::new(),
        };

        let start = offset.max(0) as usize;
        let end = (start + size as usize).min(bytes.len());
        if start > bytes.len() {
            reply.data(&[]);
        } else {
            reply.data(&bytes[start..end]);
        }
    }

    fn write(
        &mut self,
        _req: &Request<'_>,
        ino: u64,
        fh: u64,
        offset: i64,
        data: &[u8],
        _write_flags: u32,
        _flags: i32,
        _lock_owner: Option<u64>,
        reply: ReplyWrite,
    ) {
        let Some(NodeIdent::ActorCode { .. }) = self.inode_map.get(&ino) else {
            // Only actor code is writable
            reply.error(EIO);
            return;
        };

        let buf = self.write_bufs.entry((ino, fh)).or_default();

        // Ensure capacity & place chunk at offset
        let off = offset.max(0) as usize;
        if buf.len() < off + data.len() {
            buf.resize(off + data.len(), 0);
        }
        buf[off..off + data.len()].copy_from_slice(data);

        reply.written(data.len() as u32);
    }

    fn flush(&mut self, _req: &Request, ino: u64, fh: u64, _lock_owner: u64, reply: ReplyEmpty) {
        // Strict mode: compile/install on flush; error if compile fails
        let Some(NodeIdent::ActorCode { act_id, .. }) = self.inode_map.get(&ino).cloned() else {
            reply.ok();
            return;
        };

        let key = (ino, fh);
        let buf = match self.write_bufs.remove(&key) {
            Some(b) => b,
            None => {
                reply.ok();
                return;
            }
        };

        match image_install_actor_code(&mut self.image, &act_id, &buf) {
            Ok(()) => reply.ok(),
            Err(_) => reply.error(EIO),
        }
    }
}

// ============ CLI ============

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    cmd: Command,
}

#[derive(Subcommand)]
enum Command {
    Mount { image: PathBuf, target: PathBuf },
    New { target: PathBuf },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    match cli.cmd {
        Command::Mount { image, target } => {
            let fs = ImageFs::new(Image::load(image)?);
            fuser::mount2(fs, target, &[MountOption::FSName("stimg".into())])?;
        }
        Command::New { target } => Image::default().store(target)?,
    }
    Ok(())
}

// ============ image adapter layer ============
//
// Replace the stubs below with calls into your `svm_image::Image` API.
// They currently compile and panic at runtime if hit, making it easy
// to fill them one by one.

fn image_list_namespaces(_image: &Image, _parent: Option<&[u8]>) -> Vec<(String, Vec<u8>)> {
    // TODO: wire to your image model
    // Return Vec of (display_name, ns_id_bytes)
    unimplemented!("image_list_namespaces");
}

fn image_resolve_namespace(
    _image: &Image,
    _parent: Option<&[u8]>,
    _name: &str,
) -> Option<(Vec<u8>, String)> {
    // TODO: given a parent namespace (or None for root), resolve a child by name
    // Return (ns_id_bytes, display_name)
    unimplemented!("image_resolve_namespace");
}

fn image_list_actors(_image: &Image, _ns_id: &[u8]) -> Vec<(String, Vec<u8>)> {
    // TODO: list (actor_display_name, actor_id_bytes) under a namespace
    unimplemented!("image_list_actors");
}

fn image_resolve_actor(_image: &Image, _ns_id: &[u8], _name: &str) -> Option<(Vec<u8>, String)> {
    // TODO: resolve actor by display name (without ".act" suffix)
    unimplemented!("image_resolve_actor");
}

fn image_actor_code_size(image: &Image, act_id: &[u8]) -> u64 {
    image_read_actor_code(image, act_id).len() as u64
}
fn image_read_actor_code(_image: &Image, _act_id: &[u8]) -> Vec<u8> {
    // TODO: return the actor's source as bytes (UTF-8 text)
    unimplemented!("image_read_actor_code");
}
fn image_install_actor_code(_image: &mut Image, _act_id: &[u8], _src: &[u8]) -> Result<(), ()> {
    // TODO: compile/install method(s) from src into the runtime
    // Return Err(()) to signal compile failure (write will surface EIO)
    unimplemented!("image_install_actor_code");
}

fn image_actor_state_size(image: &Image, act_id: &[u8]) -> u64 {
    image_read_actor_state(image, act_id).len() as u64
}
fn image_read_actor_state(_image: &Image, _act_id: &[u8]) -> Vec<u8> {
    // TODO: pretty JSON snapshot of public/debuggable state
    unimplemented!("image_read_actor_state");
}

fn image_list_messages(_image: &Image, _act_id: &[u8]) -> Vec<(u64, String)> {
    // TODO: list mailbox messages as (msg_id, filename like "000001.msg")
    unimplemented!("image_list_messages");
}
fn image_resolve_message(_image: &Image, _act_id: &[u8], _name: &str) -> Option<(u64, String)> {
    // TODO: parse/display mapping for a single message file name
    unimplemented!("image_resolve_message");
}
fn image_message_size(image: &Image, act_id: &[u8], msg_id: u64) -> u64 {
    image_read_message(image, act_id, msg_id).len() as u64
}
fn image_read_message(_image: &Image, _act_id: &[u8], _msg_id: u64) -> Vec<u8> {
    // TODO: read message payload/headers (text or JSON)
    unimplemented!("image_read_message");
}

fn image_read_ns_meta(_image: &Image, _ns_id: &[u8]) -> Vec<u8> {
    // TODO: namespace metadata (JSON)
    unimplemented!("image_read_ns_meta");
}
fn image_ns_meta_size(image: &Image, ns_id: &[u8]) -> u64 {
    image_read_ns_meta(image, ns_id).len() as u64
}

fn image_list_children(_image: &Image, _act_id: &[u8]) -> Vec<(String, Vec<u8>)> {
    // TODO: list (child_display_name, child_actor_id_bytes)
    unimplemented!("image_list_children");
}
fn image_resolve_child(_image: &Image, _act_id: &[u8], _name: &str) -> Option<(Vec<u8>, String)> {
    // TODO: resolve child by display name
    unimplemented!("image_resolve_child");
}
fn image_child_display_path(_image: &Image, _child_id: &[u8]) -> String {
    // TODO: return a canonical path or ID for display (used in Children entries)
    unimplemented!("image_child_display_path");
}

// ============ small helpers ============

fn trim_actor_suffix(name: &str) -> &str {
    name.strip_suffix(".act").unwrap_or(name)
}
