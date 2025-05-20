use std::collections::HashMap;

use convert_case::{Boundary, Case, Casing};
use serde::Deserialize;
use serde_json::Map;

fn main() {
    let s3_model_bytes = include_bytes!("../../models/s3.json");

    let s3_ast: SmithyAST = serde_json::from_slice(s3_model_bytes).unwrap();
    let s3_model = SmithyModel::from_ast(s3_ast);
    let s3_service = s3_model
        .services
        .get("com.amazonaws.s3#AmazonS3")
        .expect("S3 service exists");
    // let s3_shapes = s3_service.get_all_shapes_in_closure(&s3_model);
    // println!("SERVICES: {s3_shapes:#?}")

    // let wit_service = format_wit_service(&s3_model, &s3_service.shape, &"s3".to_string());
    let wit_model = format_wit_model(&s3_model, &"s3".to_string());
    // TODO: terrible hack to deal with SnakeCased enum members in S3
    let wit_model = wit_model.replace("v-1", "v1");
    let wit_model = wit_model.replace("all-storage-classes-128k", "all-storage-classes128k");

    println!("{wit_model}")
}

fn format_wit_model(model: &SmithyModel, name: &String) -> String {
    let kebab_model_name = name
        .without_boundaries(&Boundary::digits())
        .to_case(Case::Kebab)
        .modify_reserved_words();
    let mut interfaces = Vec::new();
    let mut world = format!("package component:{kebab_model_name};\n\n");

    for (service_name, service) in model.services.iter() {
        let kebab_service_name = get_short_name(service_name)
            .without_boundaries(&Boundary::digits())
            .to_case(Case::Kebab)
            .modify_reserved_words();
        world.push_str(&format_wit_service(
            model,
            &service.shape,
            &kebab_service_name,
        ));
        interfaces.push(kebab_service_name);
    }

    world.push_str(&format!("world {kebab_model_name} {{\n"));

    for interface in interfaces.iter() {
        world.push_str(&format!("\texport {interface};\n"));
    }

    world.push_str("}");
    world
}

// TODO: for simple types this is fine, but record, variant, enum, and flags types must all have names associated with them,
// so probably better to generate those separately and only reference the name
/// Takes in a name and a [SmithyShapes] and outputs a wit record field of the format
/// `name: type;`
fn format_wit_field(model: &SmithyModel, name: &String, smithy_type: &SmithyShapes) -> String {
    let kebab_case_name = name
        .without_boundaries(&Boundary::digits())
        .to_case(Case::Kebab)
        .modify_reserved_words();
    let type_str = format_wit_type(model, smithy_type, &kebab_case_name);
    format!("{}: {};", kebab_case_name, type_str)
}

/// Maps Smithy types to WIT types
fn format_wit_type(model: &SmithyModel, smithy_type: &SmithyShapes, name: &String) -> String {
    match smithy_type {
        // Simple types
        SmithyShapes::Blob(_) => "list<u8>".into(),
        SmithyShapes::Boolean => "bool".into(),
        SmithyShapes::String => "string".into(),
        SmithyShapes::Byte => "s8".into(),
        SmithyShapes::Short => "s16".into(),
        SmithyShapes::Integer => "s32".into(),
        SmithyShapes::Long => "s64".into(),
        SmithyShapes::Float => "f32".into(),
        SmithyShapes::Double => "f64".into(),
        // TODO: how to model unit types, in both WIT and Smithy they only really exist in enums (all unit types) and unions/variants (entries can be unit types)
        // and are identified only by their name.
        SmithyShapes::Unit => "".into(),
        // Serialize the Big* number, Document, and Timestamp types to string since WIT doesn't really have an equivalent
        // will need to either define a shared shape for these or just do some serde work in the generated code
        SmithyShapes::BigInteger => "string".into(),
        SmithyShapes::BigDecimal => "string".into(),
        SmithyShapes::Timestamp => "string".into(),
        SmithyShapes::Document => "string".into(),
        // Aggregate types
        SmithyShapes::Enum(enum_shape) | SmithyShapes::IntEnum(enum_shape) => {
            format_wit_enum(&enum_shape, name)
        }
        SmithyShapes::List(list_shape) => format_wit_list(model, &list_shape, name),
        SmithyShapes::Map(map_shape) => format_wit_map(model, map_shape, name).into(),
        SmithyShapes::Structure(structure_shape) => {
            format_wit_struct(model, structure_shape, name).into()
        }
        SmithyShapes::Union(union_shape) => format_wit_union(model, union_shape, name).into(),
        // Servicey types
        SmithyShapes::Operation(op_shape) => format_wit_operation(model, op_shape, name),
        SmithyShapes::Service(_) | SmithyShapes::Resource => {
            panic!("Unuexpected Servicey shape")
        }
    }
}

// Creates a type that will live inside an interface
fn define_wit_type(model: &SmithyModel, smithy_type: &SmithyShapes, name: &String) -> String {
    match smithy_type {
        // Simple types
        SmithyShapes::Blob(_)
        | SmithyShapes::Boolean
        | SmithyShapes::String
        | SmithyShapes::Byte
        | SmithyShapes::Short
        | SmithyShapes::Integer
        | SmithyShapes::Long
        | SmithyShapes::Float
        | SmithyShapes::Double
        | SmithyShapes::BigInteger
        | SmithyShapes::BigDecimal
        | SmithyShapes::Timestamp
        | SmithyShapes::Document => format!(
            "type {} = {}; \n",
            name.without_boundaries(&Boundary::digits())
                .to_case(Case::Kebab)
                .modify_reserved_words(),
            format_wit_type(model, smithy_type, name)
        ),
        SmithyShapes::List(list_shape) => format!(
            "type {} = {}; \n",
            name.without_boundaries(&Boundary::digits())
                .to_case(Case::Kebab)
                .modify_reserved_words(),
            format_wit_list(model, &list_shape, name),
        ),
        SmithyShapes::Map(map_shape) => format!(
            "type {} = {}; \n",
            name.without_boundaries(&Boundary::digits())
                .to_case(Case::Kebab)
                .modify_reserved_words(),
            format_wit_map(model, &map_shape, name),
        ),
        // TODO: how to model unit types, in both WIT and Smithy they only really exist in enums (all unit types) and unions/variants (entries can be unit types)
        // and are identified only by their name.
        SmithyShapes::Unit => "".into(),
        // Serialize the Big* number, Document, and Timestamp types to string since WIT doesn't really have an equivalent
        // will need to either define a shared shape for these or just do some serde work in the generated code

        // Aggregate types
        SmithyShapes::Enum(enum_shape) | SmithyShapes::IntEnum(enum_shape) => {
            format_wit_enum(&enum_shape, name)
        }
        SmithyShapes::Structure(structure_shape) => {
            format_wit_struct(model, structure_shape, name).into()
        }
        SmithyShapes::Union(union_shape) => format_wit_union(model, union_shape, name).into(),
        // Servicey types
        SmithyShapes::Operation(op_shape) => format_wit_operation(model, op_shape, name),
        SmithyShapes::Service(_) | SmithyShapes::Resource => {
            panic!("Unuexpected Servicey shape")
        }
    }
}

fn format_wit_service(model: &SmithyModel, service_shape: &ServiceShape, name: &String) -> String {
    let service = SmithyService {
        name: name.clone(),
        shape: service_shape.clone(),
    };
    let shapes = service.get_all_shapes_in_closure(model);
    let ops = service.get_operations(model);

    let mut s = format!(
        "interface {} {{\n",
        name.without_boundaries(&Boundary::digits())
            .to_case(Case::Kebab)
            .modify_reserved_words()
    );
    s.push_str("// Shapes\n");

    for (shape_name, shape) in shapes.iter() {
        s.push_str(&define_wit_type(
            model,
            shape,
            &shape_name
                .without_boundaries(&Boundary::digits())
                .to_case(Case::Kebab)
                .modify_reserved_words(),
        ));
    }

    s.push_str("\n");
    s.push_str("// Operations\n");
    s.push_str("resource client {\n");
    for (op_name, op) in ops.iter() {
        s.push_str(&format!(
            "\t{}",
            &define_wit_type(
                model,
                &SmithyShapes::Operation(op.shape.clone()),
                &op_name
                    .without_boundaries(&Boundary::digits())
                    .to_case(Case::Kebab)
                    .modify_reserved_words(),
            )
        ));
    }
    s.push_str("}\n");

    s.push_str("}\n");
    s
}

fn format_wit_operation(model: &SmithyModel, op_shape: &OperationShape, name: &String) -> String {
    let operation = SmithyOperation {
        name: name.clone(),
        shape: op_shape.clone(),
    };
    let input_name = operation
        .get_input_name()
        .without_boundaries(&Boundary::digits())
        .to_case(Case::Kebab)
        .modify_reserved_words();
    let input_shape = operation.get_input_shape(model);
    let output_name = operation
        .get_output_name()
        .without_boundaries(&Boundary::digits())
        .to_case(Case::Kebab)
        .modify_reserved_words();
    let output_shape = operation.get_output_shape(model);

    // We trust that the containing service has already generated the input and output types
    match (input_shape, output_shape) {
        (SmithyShapes::Unit, SmithyShapes::Unit) => format!("{name}: func();\n"),
        (_, SmithyShapes::Unit) => format!("{name}: func(input: {input_name});\n"),
        (SmithyShapes::Unit, _) => format!("{name}: func() -> {output_name};\n"),
        (_, _) => format!("{name}: func(input: {input_name}) -> {output_name};\n"),
    }
}

/// WIT doesn't really have string or number enums, so we aren't differentiating between them for now
fn format_wit_enum(enum_shape: &EnumShape, name: &String) -> String {
    let mut e: String = format!("enum {name} {{\n");
    // All shapes should be Unit types, so we ignore
    for (name, _) in enum_shape.members.iter() {
        let kebab_name = name
            .from_case(Case::Snake)
            .without_boundaries(&Boundary::digits())
            .to_case(Case::Kebab)
            .modify_reserved_words();
        e.push_str(format!("\t{kebab_name},\n").as_str());
    }

    e.push_str("}\n");
    e
}

fn format_wit_list(model: &SmithyModel, list_shape: &ListShape, name: &String) -> String {
    format!(
        "list<{}>",
        list_shape
            .member
            .get_short_name()
            .without_boundaries(&Boundary::digits())
            .to_case(Case::Kebab)
            .modify_reserved_words()
    )
}

// TODO: Maybe this should used named types like format_wit_list above?
/// This unrolls the map to a list<(key-type, value-type)> since WIT doesn't support arbitrary maps
fn format_wit_map(model: &SmithyModel, map_shape: &MapShape, name: &String) -> String {
    let key_type_name = map_shape
        .key
        .get_short_name()
        .without_boundaries(&Boundary::digits())
        .to_case(Case::Kebab)
        .modify_reserved_words();
    let value_type_name = map_shape
        .value
        .get_short_name()
        .without_boundaries(&Boundary::digits())
        .to_case(Case::Kebab)
        .modify_reserved_words();
    format!("list<({key_type_name}, {value_type_name})>")
}

// Turns a Smithy structure type into a WIT record type
fn format_wit_struct(model: &SmithyModel, struct_shape: &StructureShape, name: &String) -> String {
    let mut s: String = format!("record {name} {{\n");

    for (k, v) in struct_shape.members.iter() {
        let kebab_name = k
            .without_boundaries(&Boundary::digits())
            .to_case(Case::Kebab)
            .modify_reserved_words();
        let value_type_name = v
            .get_short_name()
            .without_boundaries(&Boundary::digits())
            .to_case(Case::Kebab)
            .modify_reserved_words();
        s.push_str(format!("\t{kebab_name}: {value_type_name},\n").as_str());
    }
    s.push_str("}\n");
    s
}

/// Formats a Smithy Union type as a WIT Variant type
fn format_wit_union(model: &SmithyModel, union_shape: &UnionShape, name: &String) -> String {
    let mut u: String = format!("variant {name} {{\n");

    for (k, v) in union_shape.members.iter() {
        let kebab_name = k
            .without_boundaries(&Boundary::digits())
            .to_case(Case::Kebab)
            .modify_reserved_words();
        let value_shape = v.get_shape(model).1;
        let value_type_name = v
            .get_short_name()
            .without_boundaries(&Boundary::digits())
            .to_case(Case::Kebab)
            .modify_reserved_words();

        // unit types are identified by name only
        if value_shape == &SmithyShapes::Unit {
            u.push_str(format!("\t{kebab_name},\n").as_str());
        } else {
            u.push_str(format!("\t{kebab_name}({value_type_name}),\n").as_str());
        }
    }
    u.push_str("}\n");
    u
}

/// A basic copy of the Smithy JSON AST
#[derive(Deserialize, Debug)]
struct SmithyAST {
    smithy: String,
    metadata: Option<serde_json::Value>,
    shapes: HashMap<String, SmithyShapes>,
}

#[derive(Debug)]
struct SmithyModel {
    ast: SmithyAST,
    operations: HashMap<String, SmithyOperation>,
    services: HashMap<String, SmithyService>,
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

        let services = ast
            .shapes
            .iter()
            .filter_map(|(k, v)| match v {
                SmithyShapes::Service(inner) => Some((
                    k.clone(),
                    SmithyService {
                        name: k.clone(),
                        shape: inner.clone(),
                    },
                )),
                _ => None,
            })
            .collect();

        Self {
            ast,
            operations,
            services,
        }
    }

    fn get_operations(&self) -> &HashMap<String, SmithyOperation> {
        &self.operations
    }

    fn get_all_shapes(&self) -> &HashMap<String, SmithyShapes> {
        &self.ast.shapes
    }

    fn get_services(&self) -> &HashMap<String, SmithyService> {
        &self.services
    }

    fn get_shape<'a>(&'a self, name: &'a String) -> (&'a String, &'a SmithyShapes) {
        if name != "smithy.api#Unit" {
            (name, self.ast.shapes.get(name).expect("Shape exists"))
        } else {
            (name, &SmithyShapes::Unit)
        }
    }
}

#[derive(Debug, Clone)]
struct SmithyOperation {
    name: String,
    shape: OperationShape,
}

impl SmithyOperation {
    fn get_input_member(&self) -> &SmithyMember {
        &self.shape.input
    }

    fn get_input_shape<'a>(&'a self, model: &'a SmithyModel) -> &'a SmithyShapes {
        let (shape_name, shape) = model.get_shape(&self.get_input_member().target);
        match shape {
            SmithyShapes::Structure(_) | SmithyShapes::Unit => {}
            _ => panic!("Operation input not a Structure"),
        };
        shape
    }

    fn get_input_name(&self) -> &str {
        self.get_input_member().get_short_name()
    }

    fn get_output_member(&self) -> &SmithyMember {
        &self.shape.output
    }

    fn get_output_shape<'a>(&'a self, model: &'a SmithyModel) -> &'a SmithyShapes {
        let (shape_name, shape) = model.get_shape(&self.get_output_member().target);
        match shape {
            SmithyShapes::Structure(_) | SmithyShapes::Unit => {}
            _ => panic!("Operation output not a Structure"),
        };
        shape
    }

    fn get_output_name(&self) -> &str {
        self.get_output_member().get_short_name()
    }

    fn get_short_name(&self) -> &str {
        let parts: Vec<&str> = self.name.split('#').collect();
        parts[1]
    }
}

#[derive(Debug)]
struct SmithyService {
    name: String,
    shape: ServiceShape,
}

impl SmithyService {
    fn get_operations(&self, model: &SmithyModel) -> HashMap<String, SmithyOperation> {
        let map = &self
            .shape
            .operations
            .iter()
            .map(|op| {
                let shape = match op.get_shape(model).1 {
                    SmithyShapes::Operation(operation_shape) => operation_shape,
                    _ => panic!("Service should only contain operation shapes"),
                }
                .clone();
                let name = op.get_short_name().to_string();
                (name.clone(), SmithyOperation { name: name, shape })
            })
            .collect::<HashMap<String, SmithyOperation>>();

        map.clone()
    }

    fn get_all_shapes_in_closure(&self, model: &SmithyModel) -> HashMap<String, SmithyShapes> {
        let mut all_shapes: HashMap<String, SmithyShapes> = HashMap::new();
        let op_shapes: HashMap<String, SmithyOperation> = self.get_operations(model);

        for (op_name, op) in op_shapes.iter() {
            let input_name = op.get_input_name();
            let input_shape = op.get_input_shape(model);
            if let SmithyShapes::Structure(input_struct) = input_shape {
                all_shapes.insert(input_name.to_string(), input_shape.clone());
                get_all_shapes_in_aggregate_type(model, &mut all_shapes, input_struct);
            }

            let output_name = op.get_output_name();
            let output_shape = op.get_output_shape(model);
            if let SmithyShapes::Structure(output_struct) = output_shape {
                all_shapes.insert(output_name.to_string(), output_shape.clone());
                get_all_shapes_in_aggregate_type(model, &mut all_shapes, output_struct);
            }
        }

        all_shapes
    }
}

// Gets all shapes that are children of an aggregate type including recursively through child types
// that are also aggregates
fn get_all_shapes_in_aggregate_type(
    model: &SmithyModel,
    shape_map: &mut HashMap<String, SmithyShapes>,
    agg_shape: &impl AggregateType,
) {
    for member in agg_shape.get_members().iter() {
        let (shape_name, shape) = member.get_shape(model).clone();
        // println!("COLLECT {} : {:#?}", member.get_short_name(), shape);
        // Simple types just get inserted
        if shape.is_simple_type() {
            shape_map.insert(get_short_name(shape_name).to_string(), shape.clone());
        } else if shape.is_aggregate_type() {
            shape_map.insert(get_short_name(shape_name).to_string(), shape.clone());
            match shape {
                SmithyShapes::List(list_shape) => {
                    get_all_shapes_in_aggregate_type(model, shape_map, list_shape)
                }
                SmithyShapes::Map(map_shape) => {
                    get_all_shapes_in_aggregate_type(model, shape_map, map_shape)
                }
                SmithyShapes::Structure(structure_shape) => {
                    get_all_shapes_in_aggregate_type(model, shape_map, structure_shape)
                }
                SmithyShapes::Union(union_shape) => {
                    get_all_shapes_in_aggregate_type(model, shape_map, union_shape)
                }
                _ => panic!("Expected aggregate shape, got {:#?}", shape),
            }
        } else {
            panic!("Unsupported type found in struct: {:#?}", shape);
        }
    }
}

trait AggregateType {
    fn get_members(&self) -> Vec<SmithyMember>;
}

type SmithyTraits = Option<HashMap<String, serde_json::Value>>;

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct SmithyMember {
    target: String,
    traits: SmithyTraits,
}

impl SmithyMember {
    fn get_shape<'a>(&'a self, model: &'a SmithyModel) -> (&'a String, &'a SmithyShapes) {
        model.get_shape(&self.target)
    }

    fn get_short_name(&self) -> &str {
        let parts: Vec<&str> = self.target.split('#').collect();
        parts[1]
    }
}

fn get_short_name(target: &String) -> &str {
    let parts: Vec<&str> = target.split('#').collect();
    parts[1]
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(tag = "type")]
#[serde(rename_all = "lowercase")]
enum SmithyShapes {
    // Simple types
    Unit,
    Blob(BlobShape),
    Boolean,
    String,
    Byte,
    Short,
    Integer,
    Long,
    Float,
    Double,
    BigInteger,
    BigDecimal,
    Timestamp,
    Document,
    Enum(EnumShape),
    IntEnum(EnumShape),

    // Aggregate Types
    List(ListShape),
    Map(MapShape),
    Structure(StructureShape),
    Union(UnionShape),

    // Service Types
    Service(ServiceShape),
    Operation(OperationShape),
    Resource,
}

impl SmithyShapes {
    /// Is it a type that won't have any internal members?
    fn is_simple_type(&self) -> bool {
        match self {
            SmithyShapes::Blob(_)
            | SmithyShapes::Boolean
            | SmithyShapes::String
            | SmithyShapes::Byte
            | SmithyShapes::Short
            | SmithyShapes::Integer
            | SmithyShapes::Long
            | SmithyShapes::Float
            | SmithyShapes::Double
            | SmithyShapes::BigInteger
            | SmithyShapes::BigDecimal
            | SmithyShapes::Timestamp
            | SmithyShapes::Document
            | SmithyShapes::Enum(_)
            | SmithyShapes::IntEnum(_) => true,
            _ => false,
        }
    }

    /// Is it a type that might have internal members?
    fn is_aggregate_type(&self) -> bool {
        match self {
            SmithyShapes::List(_)
            | SmithyShapes::Map(_)
            | SmithyShapes::Structure(_)
            | SmithyShapes::Union(_) => true,
            _ => false,
        }
    }

    // fn is_wit_named_type(&self) -> bool {
    //     match self {
    //         SmithyShapes::Enum(enum_shape) => todo!(),
    //         SmithyShapes::IntEnum(enum_shape) => todo!(),
    //         SmithyShapes::List(list_shape) => todo!(),
    //         SmithyShapes::Map(map_shape) => todo!(),
    //         SmithyShapes::Structure(structure_shape) => todo!(),
    //         SmithyShapes::Union(union_shape) => todo!(),
    //         SmithyShapes::Service(service_shape) => todo!(),
    //         SmithyShapes::Operation(operation_shape) => todo!(),
    //         SmithyShapes::Resource => todo!(),
    //     }
    // }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct StructureShape {
    members: HashMap<String, SmithyMember>,
    traits: SmithyTraits,
}

impl AggregateType for StructureShape {
    fn get_members(&self) -> Vec<SmithyMember> {
        self.members
            .iter()
            .map(|(k, v)| v.clone())
            .collect::<Vec<SmithyMember>>()
            .clone()
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct UnionShape {
    members: HashMap<String, SmithyMember>,
    traits: SmithyTraits,
}

impl AggregateType for UnionShape {
    fn get_members(&self) -> Vec<SmithyMember> {
        self.members
            .iter()
            .map(|(k, v)| v.clone())
            .collect::<Vec<SmithyMember>>()
            .clone()
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct EnumShape {
    members: HashMap<String, SmithyMember>,
    traits: SmithyTraits,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct MapShape {
    key: SmithyMember,
    value: SmithyMember,
}

impl AggregateType for MapShape {
    fn get_members(&self) -> Vec<SmithyMember> {
        vec![self.key.clone(), self.value.clone()]
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct BlobShape {
    traits: SmithyTraits,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct ListShape {
    member: SmithyMember,
}

impl AggregateType for ListShape {
    fn get_members(&self) -> Vec<SmithyMember> {
        vec![self.member.clone()]
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct ServiceShape {
    version: String,
    operations: Vec<SmithyMember>,
    traits: SmithyTraits,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct OperationShape {
    input: SmithyMember,
    output: SmithyMember,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct SmithyMetadata {
    suppressions: Vec<SmithySuppression>,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct SmithySuppression {
    id: String,
    namespace: String,
}

pub trait WitReservedWords<T: AsRef<str>> {
    fn modify_reserved_words(&self) -> String;
}

impl<T: AsRef<str>> WitReservedWords<T> for T
where
    T: ToString,
{
    fn modify_reserved_words(&self) -> String {
        match self.to_string().as_str() {
            "as" | "async" | "bool" | "borrow" | "char" | "constructor" | "enum" | "export"
            | "f32" | "f64" | "flags" | "from" | "func" | "future" | "import" | "include"
            | "interface" | "list" | "option" | "own" | "package" | "record" | "resource"
            | "result" | "s16" | "s32" | "s64" | "s8" | "static" | "stream" | "string"
            | "tuple" | "type" | "u16" | "u32" | "u64" | "u8" | "use" | "variant" | "with"
            | "world" => "%".to_string() + &self.to_string(),
            _ => self.to_string(),
        }
    }
}
