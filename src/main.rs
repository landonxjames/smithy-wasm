use std::collections::HashMap;

use serde::Deserialize;

fn main() {
    let s3_model_bytes = include_bytes!("../models/s3.json");

    let s3_model: SmithyAST = serde_json::from_slice(s3_model_bytes).unwrap();
    println!("MODEL: {s3_model:#?}");
}

/// A basic copy of the Smithy JSON AST
#[derive(Deserialize, Debug)]
struct SmithyAST {
    smithy: String,
    metadata: Option<serde_json::Value>,
    shapes: HashMap<String, SmithyShapes>,
}

struct SmithyModel {
    ast: SmithyAST,
    operations: HashMap<String, SmithyOperation>,
}

impl SmithyModel {
    fn from_ast(ast: SmithyAST) -> Self {
        let operations = ast
            .shapes
            .iter()
            .filter_map(|(k, v)| match v {
                SmithyShapes::Operation(inner) => Some((
                    k.clone(),
                    SmithyOperation {
                        name: k.clone(),
                        shape: inner.clone(),
                    },
                )),
                _ => None,
            })
            .collect();

        Self { ast, operations }
    }

    fn get_operations(&self) -> &HashMap<String, SmithyOperation> {
        &self.operations
    }
}

struct SmithyOperation {
    name: String,
    shape: OperationShape,
}

impl SmithyOperation {
    fn get_input(&self) -> &SmithyMember {
        &self.shape.input
    }

    fn get_output(&self) -> &SmithyMember {
        &self.shape.output
    }
}

type SmithyTraits = Option<HashMap<String, serde_json::Value>>;

#[derive(Deserialize, Debug, Clone)]
struct SmithyMember {
    target: String,
    traits: SmithyTraits,
}

#[derive(Deserialize, Debug, Clone)]
#[serde(tag = "type")]
#[serde(rename_all = "lowercase")]
enum SmithyShapes {
    Boolean,
    String,
    Timestamp,
    Long,
    Integer,
    Blob(BlobShape),
    Structure(StructureShape),
    List(ListShape),
    Service(ServiceShape),
    Operation(OperationShape),
    Union(UnionShape),
    Enum(EnumShape),
    Map(MapShape),
}

#[derive(Deserialize, Debug, Clone)]
struct StructureShape {
    members: HashMap<String, SmithyMember>,
    traits: SmithyTraits,
}

#[derive(Deserialize, Debug, Clone)]
struct UnionShape {
    members: HashMap<String, SmithyMember>,
    traits: SmithyTraits,
}

#[derive(Deserialize, Debug, Clone)]
struct EnumShape {
    members: HashMap<String, SmithyMember>,
    traits: SmithyTraits,
}

#[derive(Deserialize, Debug, Clone)]
struct MapShape {
    key: SmithyMember,
    value: SmithyMember,
}

#[derive(Deserialize, Debug, Clone)]
struct BlobShape {
    traits: SmithyTraits,
}

#[derive(Deserialize, Debug, Clone)]
struct ListShape {
    member: SmithyMember,
}

#[derive(Deserialize, Debug, Clone)]
struct ServiceShape {
    version: String,
    operations: Vec<SmithyMember>,
    traits: SmithyTraits,
}

#[derive(Deserialize, Debug, Clone)]
struct OperationShape {
    input: SmithyMember,
    output: SmithyMember,
}

#[derive(Deserialize, Debug, Clone)]
struct SmithyMetadata {
    suppressions: Vec<SmithySuppression>,
}

#[derive(Deserialize, Debug, Clone)]
struct SmithySuppression {
    id: String,
    namespace: String,
}
