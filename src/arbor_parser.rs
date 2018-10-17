use num::traits::float::Float;
use num::FromPrimitive;
use serde::Deserialize;
use serde::Deserializer;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;
use utils::Location;
use Arbor;

enum_from_primitive! {
#[derive(Deserialize, Debug, PartialEq, Clone)]
enum ConnectorRelation {
    Presynaptic = 0,
    Postsynaptic = 1,
    GapJunction = 2,
    Other = -1
}
}

struct ArborLocations<NodeType: Hash + Clone, F: Float> {
    arbor: Arbor<NodeType>,
    locations: HashMap<NodeType, Location<F>>,
}

struct InputsOutputs<NodeType> {
    inputs: HashMap<NodeType, usize>,
    outputs: HashMap<NodeType, usize>,
}

trait ArborParseable<C: DescribesConnector> {
    fn connectors(&self) -> &Vec<C>;

    fn treenodes(&self) -> &Vec<Treenode>;

    fn to_arborparser(&self) -> Result<ArborParser<u64, f64>, &'static str> {
        let arbor_locations = self.treenodes_to_arbor_locations()?;
        let inputs_outputs = self.connectors_to_inputs_outputs();

        Ok(ArborParser {
            arbor: arbor_locations.arbor,
            inputs: inputs_outputs.inputs,
            outputs: inputs_outputs.outputs,
            locations: arbor_locations.locations,
        })
    }

    fn connectors_to_inputs_outputs(&self) -> InputsOutputs<u64> {
        let mut inputs: HashMap<u64, usize> = HashMap::new();
        let mut outputs: HashMap<u64, usize> = HashMap::new();

        for connector_info in self.connectors().iter() {
            let connector = connector_info.this();
            match connector.relation {
                ConnectorRelation::Postsynaptic => {
                    let mut entry = inputs.entry(connector.treenode_id).or_insert(0);
                    *entry += 1;
                }
                ConnectorRelation::Presynaptic => {
                    let mut entry = outputs.entry(connector.treenode_id).or_insert(0);
                    *entry += 1;
                }
                _ => (),
            }
        }

        InputsOutputs { inputs, outputs }
    }

    /// N.B. does not check validity of input
    fn treenodes_to_arbor_locations(&self) -> Result<ArborLocations<u64, f64>, &'static str> {
        let mut locations: HashMap<u64, Location<f64>> = HashMap::new();
        let mut edges: HashMap<u64, u64> = HashMap::new();
        let mut root = None;

        for treenode in self.treenodes().iter() {
            match treenode.parent_id {
                Some(tn) => {
                    edges.insert(treenode.id, tn);
                }
                None => {
                    root = match root {
                        Some(node) => Err("More than one parentless node found in this arbor"),
                        None => Ok(Some(treenode.id)),
                    }?;
                }
            }

            locations.insert(treenode.id, treenode.location.clone());
        }

        Ok(ArborLocations {
            arbor: Arbor::from(edges, root)?,
            locations,
        })
    }
}

#[derive(Deserialize, Debug)]
struct Treenode {
    id: u64,
    parent_id: Option<u64>,
    user_id: u64,
    location: Location<f64>,
    radius: f64,
    confidence: u8,
}

#[derive(Deserialize, Debug, Clone)]
struct Connector {
    treenode_id: u64,
    connector_id: u64,
    relation: ConnectorRelation,
}

#[derive(Deserialize, Debug)]
struct SkeletonConnector {
    this: Connector,
    location: Location<f64>,
}

#[derive(Deserialize, Debug)]
struct SkeletonResponse {
    treenodes: Vec<Treenode>,
    connectors: Vec<SkeletonConnector>,
    tags: HashMap<u64, Vec<String>>, // not sure what's in this dict
                                     // conditionally has a bunch of other stuff?
}

impl ArborParseable<SkeletonConnector> for SkeletonResponse {
    fn connectors(&self) -> &Vec<SkeletonConnector> {
        &self.connectors
    }
    fn treenodes(&self) -> &Vec<Treenode> {
        &self.treenodes
    }
}

impl ArborParseable<ArborConnector> for ArborResponse {
    fn connectors(&self) -> &Vec<ArborConnector> {
        &self.connectors
    }
    fn treenodes(&self) -> &Vec<Treenode> {
        &self.treenodes
    }
}

#[derive(Deserialize, Debug)]
struct ArborConnector {
    this: Connector,
    other: Connector,
}

trait DescribesConnector {
    fn this(&self) -> Connector;
}

impl DescribesConnector for ArborConnector {
    fn this(&self) -> Connector {
        self.this.clone()
    }
}

impl DescribesConnector for SkeletonConnector {
    fn this(&self) -> Connector {
        self.this.clone()
    }
}

#[derive(Deserialize, Debug)]
struct ArborResponse {
    treenodes: Vec<Treenode>,
    connectors: Vec<ArborConnector>,
    tags: HashMap<u64, Vec<String>>,
}

enum Response {
    Skeleton(SkeletonResponse),
    Arbor(ArborResponse),
}

pub struct ArborParser<NodeType: Hash + Eq + Ord + Copy, F: Float> {
    arbor: Arbor<NodeType>,
    inputs: HashMap<NodeType, usize>,
    outputs: HashMap<NodeType, usize>,
    locations: HashMap<NodeType, Location<F>>,
}

impl<NodeType: Hash + Debug + Eq + Ord + Copy, F: Float> ArborParser<NodeType, F> {
    fn new() -> ArborParser<NodeType, F> {
        ArborParser {
            arbor: Arbor::new(),
            inputs: HashMap::new(),
            outputs: HashMap::new(),
            locations: HashMap::new(),
        }
    }

    fn create_synapse_map(&self) -> HashMap<NodeType, usize> {
        let mut out: HashMap<NodeType, usize> = HashMap::new();

        for (key, value) in self.inputs.iter().chain(self.outputs.iter()) {
            let mut entry = out.entry(*key).or_insert(0);
            *entry += value;
        }
        out
    }

    fn collapse_artifactual_branches(
        &mut self,
        tags: HashMap<String, Vec<NodeType>>,
    ) -> ArborParser<NodeType, F> {
        unimplemented!();
    }

    // todo: collapse short branches too?
}

impl ArborParser<u64, f64> {
    fn from_response(response: Response) -> Result<ArborParser<u64, f64>, &'static str> {
        match response {
            Response::Skeleton(r) => r.to_arborparser(),
            Response::Arbor(r) => r.to_arborparser(),
        }
    }
}

// todo: implement Deserialize for ArborResponse and SkeletonResponse
