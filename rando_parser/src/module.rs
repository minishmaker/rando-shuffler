use std::{collections::HashMap, path::Path, fs::File};

use serde::{Deserialize, Serialize};
use zip::{ZipArchive, result::ZipResult};

use rando_core::descriptor::DescriptorType;

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub version: String,
    pub provides: ModuleContents
}


#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct ModuleContents {
    pub descriptors: Option<HashMap<String, DescriptorDecl>>,
    pub descriptor_definitions: Option<Vec<String>>,
    pub logic: Option<Vec<String>>,
    pub data: Option<Vec<String>>,
    pub shuffles: Option<Vec<String>>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct DescriptorDecl {
    //#[serde(rename = "type")]
    //pub result_type: DescriptorType,
    pub params: Vec<DescriptorParam>,
    #[serde(default)]
    pub stateful: bool,
    #[serde(default)]
    pub export: DescriptorExport,
    pub consumes: Option<ConsumesDecl>
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DescriptorExport {
    Default,
    Node,
    Access,
}

impl Default for DescriptorExport {
    fn default() -> Self {
        Self::Default
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum DescriptorParam {
    Scoped,
    Unscoped,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct ConsumesDecl {
    pub key: String,
    pub lock: String,
}

/*
fn load_module(path: &Path) -> ZipResult<Vec<String>> {
    let file = File::open(path)?;
    let zip = ZipArchive::new(file)?;
    let module_file = zip.by_name("module.yaml")?;

    let module = serde_yaml::from_reader::<_, Module>(module_file)
        .unwrap(); // Fixme

    let files = {
        let mut files = zip.file_names()
            .collect::<Vec<_>>();
        files.sort();
        files
    };
    
    let ModuleContents {
        descriptors,
        descriptor_definitions,
        logic,
        data,
        shuffles
    } = &module.provides;

    let files = descriptor_definitions.iter()
        .chain(logic.iter())
        .chain(data.iter())
        .chain(shuffles.iter())
        .map(|v| v.iter());

    todo!()
}
*/