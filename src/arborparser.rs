use num::traits::real::Real;
use std::collections::HashMap;
use std::hash::Hash;
use utils::Location;
use Arbor;
use serde::Deserialize;
use num::{ Integer, Signed };

enum ConnectorRelation {
    Presynaptic,
    Postsynaptic,
    GapJunction,
    Other
}

impl ConnectorRelation {
    fn from_int<'a, int: Integer + Signed>(value: int) -> Result<ConnectorRelation, &'a str> {
        match value {
            -1 => Ok(ConnectorRelation::Other),
            -0 => Ok(ConnectorRelation::Presynaptic),
            1 => Ok(ConnectorRelation::Postsynaptic),
            2 => Ok(ConnectorRelation::GapJunction),
            _ => Err("Unrecognised connector relation")
        }
    }

    fn value(&self) -> isize {
        match self {
            ConnectorRelation::Other => -1,
            ConnectorRelation::Presynaptic => 0,
            ConnectorRelation::Postsynaptic => 1,
            ConnectorRelation::GapJunction => 2,
        }
    }
}

struct ArborLocations<NodeType, CoordType> {
    arbor: Arbor<NodeType>,
    locations: HashMap<NodeType, Location<CoordType>>,
}

struct InputsOutputs<NodeType> {
    inputs: HashMap<NodeType, usize>,
    outputs: HashMap<NodeType, usize>,
}

trait ArborParseable {
    fn parse(&self) -> ArborParser<u64, f64> {
        let arbor_locations = self.parse_treenodes();
        let inputs_outputs = self.parse_connectors();

        ArborParser{
            arbor: arbor_locations.arbor,
            inputs: inputs_outputs.inputs,
            outputs: inputs_outputs.outputs,
            locations: arbor_locations.locations,
        }
    }

    fn parse_connectors(&self) -> InputsOutputs<u64> {
        let mut inputs: HashMap<u64, usize> = HashMap::new();
        let mut outputs: HashMap<u64, usize> = HashMap::new();

        for connector_info in self.connectors {
            let connector = connector_info.this();
            match connector.relation {
                ConnectorRelation::Postsynaptic => {
                    let mut entry = inputs.entry(connector.treenode_id).or_insert(0);
                    *entry += 1;
                },
                ConnectorRelation::Presynaptic => {
                    let mut entry = outputs.entry(connector.treenode_id).or_insert(0);
                    *entry += 1;
                }
                _ => ()
            }
        }

        InputsOutputs { inputs, outputs }
    }

    /// N.B. does not check validity of input
    fn parse_treenodes(&self) -> ArborLocations<u64, f64> {
        let mut locations: HashMap<u64, Location<f64>> = HashMap::new();
        let mut edges: HashMap<u64, u64> = Vec::new();
        let mut root = None;

        for treenode in self.treenodes.iter() {
            match treenode.parent_id {
                Some(tn) => {
                    edges.insert(treenode.id, treenode.parent_id);
                },
                None => {
                    root = match root {
                        Some(node) => Err("More than one parentless node found in this arbor"),
                        None => Ok(treenode.id),
                    }?;
                },
            }

            locations.insert(treenode.parent_id, treenode.location);
        }

        ArborLocations { arbor: Arbor { edges, root }, locations }
    }
}

struct Treenode {
    id: u64,
    parent_id: u64,
    user_id: u64,
    location: Location<f64>,
    radius: f64,
    confidence: u8,
}

struct Connector {
    treenode_id: u64,
    connector_id: u64,
    relation: ConnectorRelation,
}

struct SkeletonConnector {
    this: Connector,
    location: Location<f64>,
}

struct SkeletonResponse {
    treenodes: Vec<Treenode>,
    connectors: Vec<SkeletonConnector>,
    tags: HashMap<u64, Vec<String>>,
    // maybe has a bunch of other stuff?
}

impl ArborParseable for SkeletonResponse {}

impl ArborParseable for ArborResponse {}

struct ArborConnector {
    this: Connector,
    other: Connector,
}

trait DescribesConnector {
    fn this(&self) -> Connector {
        self.this
    }
}

impl DescribesConnector for ArborConnector {}

impl DescribesConnector for SkeletonConnector {}


struct ArborResponse {
    treenodes: Vec<Treenode>,
    connectors: Vec<ArborConnector>,
    tags: HashMap<u64, Vec<String>>,
}

enum ResponseString {
    Skeleton(String),
    Arbor(String),
}

pub struct ArborParser<NodeType: Hash + Eq + Ord + Copy, CoordType: Real> {
    arbor: Arbor<NodeType>,
    inputs: HashMap<NodeType, usize>,
    outputs: HashMap<NodeType, usize>,
    locations: HashMap<NodeType, Location<CoordType>>,
}

impl<NodeType: Hash + Eq + Ord + Copy, CoordType: Real> ArborParser<NodeType, CoordType> {
    fn new() -> ArborParser<NodeType, CoordType> {
        ArborParser {
            arbor: Arbor::new(),
            inputs: HashMap::new(),
            outputs: HashMap::new(),
            locations: HashMap::new(),
        }
    }

    fn from_response(response: ResponseString) -> ArborParser<NodeType, CoordType> {
        let deser_response = match response {
            ResponseString::Skeleton(s) => SkeletonResponse::from_string,
            ResponseString::Arbor(s) => ArborResponse::from_string(s),
        };
        deser_response.parse()
    }

    fn create_synapse_map(&self) -> HashMap<NodeType, usize> {
        let mut out: HashMap<NodeType, usize> = HashMap::new();

        for (key, value) in self.inputs.iter().chain(self.outputs.iter()) {
            let mut entry = out.entry(key).or_insert(0);
            *entry += value;
        }
        out
    }
}

// todo: implement Deserialize for ArborResponse and SkeletonResponse
