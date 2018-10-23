use fnv::FnvHashMap;
use num::traits::float::Float;
use serde::Deserialize;
use serde::Deserializer;
use std::fmt::Debug;
use std::hash::Hash;
use utils::Location;
use Arbor;
use serde::de::Visitor;
use std::fmt;
use serde::de;
use serde::de::SeqAccess;
use serde_json::Value;
use std::result::Result::Ok;
use std::result::Result::Err;
use num::Integer;

// todo: figure out strategy for access control

#[derive(Debug, PartialEq, Clone)]
pub enum ConnectorRelation {
    // 0 = presynaptic, 1 = postsynaptic, 2 = gap junction, -1 = other:
    Presynaptic = 0,  // 0
    Postsynaptic = 1,  // 1
    GapJunction = 2,  // 2
    Other = -1,  // -1
}

impl ConnectorRelation {
    fn from_i64(number: i64) -> Result<ConnectorRelation, &'static str> {
        match number {
            0 => Ok(ConnectorRelation::Presynaptic),
            1 => Ok(ConnectorRelation::Postsynaptic),
            2 => Ok(ConnectorRelation::GapJunction),
            -1 => Ok(ConnectorRelation::Other),
            _ => Err("Number is not a known connector relation"),
        }
    }

    fn from_json<E: de::Error>(value: Value) -> Result<ConnectorRelation, E> {
        match value {
            Value::Number(n) => ConnectorRelation::from_i64(n.as_i64().unwrap_or(-2)).map_err(de::Error::custom),
            _ => Err(de::Error::custom("Value is not a number"))
        }
    }
}

pub struct ArborLocations<NodeType: Hash + Clone + Eq, F: Float> {
    arbor: Arbor<NodeType>,
    locations: FnvHashMap<NodeType, Location<F>>,
}

pub struct InputsOutputs<NodeType> {
    inputs: FnvHashMap<NodeType, usize>,
    outputs: FnvHashMap<NodeType, usize>,
}

pub trait ArborParseable<C: DescribesConnector> {
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
        let mut inputs: FnvHashMap<u64, usize> = FnvHashMap::default();
        let mut outputs: FnvHashMap<u64, usize> = FnvHashMap::default();

        for connector_info in self.connectors().iter() {
            let connector = connector_info.this();
            // todo: must be a way to factor these very similar arms
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
        let mut locations: FnvHashMap<u64, Location<f64>> = FnvHashMap::default();
        let mut edges: FnvHashMap<u64, u64> = FnvHashMap::default();
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
            arbor: Arbor::new(edges, root)?,
            locations,
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct Treenode {
    // todo: cannot derive Deserialize because Location needs to be collected
    // [id, parent_id, user_id, location_x, location_y, location_z, radius, confidence]
    id: u64,
    parent_id: Option<u64>,
    user_id: u64,
    location: Location<f64>,
    radius: f64,
    confidence: u8,
}

impl<'de> Deserialize<'de> for Treenode {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error> where
        D: Deserializer<'de> {
        struct TreenodeVisitor;

        impl<'de> Visitor<'de> for TreenodeVisitor {
            type Value = Treenode;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Treenode")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<Treenode, V::Error> where V: SeqAccess<'de> {
                let id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let parent_id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;
                let user_id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(2, &self))?;
                let x = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(3, &self))?;
                let y = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(4, &self))?;
                let z = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(5, &self))?;
                let radius = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(6, &self))?;
                let confidence = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(7, &self))?;

                Ok(Treenode {
                    id,
                    parent_id: match parent_id {
                        Value::Null => None,
                        Value::Number(n) => n.as_u64(),
                        _ => panic!("not a number"),  // todo: use de::Error::invalid_type
                    },
                    user_id,
                    location: Location {x,y,z},
                    radius,
                    confidence,
                })
            }
        }

        deserializer.deserialize_any(TreenodeVisitor)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Connector {
    // todo: cannot derive Deserialize because Arbor and SkeletonConnectors use different ordering,
    // and need to serialise confidences
    treenode_id: u64,
    connector_id: u64,
    relation: ConnectorRelation,
}

#[derive(Debug)]
pub struct SkeletonConnector {
    // todo: cannot derive Deserialize because Location needs to be collected
    // 0 = presynaptic, 1 = postsynaptic, 2 = gap junction, -1 = other:
    //
    //    [treenode_id, connector_id, 0|1|2|-1, location_x, location_y, location_z]
    this: Connector,
    location: Location<f64>,
}

impl<'de> Deserialize<'de> for SkeletonConnector {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error> where
        D: Deserializer<'de> {
        struct SkeletonConnectorVisitor;

        impl<'de> Visitor<'de> for SkeletonConnectorVisitor {
            type Value = SkeletonConnector;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Treenode")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<SkeletonConnector, V::Error> where V: SeqAccess<'de> {
                let treenode_id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let connector_id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;
                let relation_id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(2, &self))?;
                let x = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let y = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let z = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;

                Ok(SkeletonConnector {
                    this: Connector {
                        treenode_id,
                        connector_id,
                        relation: ConnectorRelation::from_json(relation_id)?,
                    },
                    location: Location {x,y,z},
                })
            }
        }

        deserializer.deserialize_any(SkeletonConnectorVisitor)
    }
}

#[derive(Deserialize, Debug)]
pub struct SkeletonResponse {
    // Only deserializes for with_history=false, with_merge_history=false, with_reviews=false,
    // with_annotations=false, with_user_info=false
    //
    // [[nodes], [connectors], {nodeID: [tags]}]
    //                          ^should be {tag_name: [skeleton_ids]}?
    //
    // Each element in the [nodes] array has the following form:
    //
    //    [id, parent_id, user_id, location_x, location_y, location_z, radius, confidence].
    //
    //Each element in the [connectors] array has the following form, with the
    //    third element representing the connector link as 0 = presynaptic, 1 =
    //    postsynaptic, 2 = gap junction, -1 = other:
    //
    //    [treenode_id, connector_id, 0|1|2|-1, location_x, location_y, location_z]
    treenodes: Vec<Treenode>,
    connectors: Vec<SkeletonConnector>,
    tags: FnvHashMap<String, Vec<u64>>,
    reviews: Vec<Vec<Value>>,  // placeholders, ignored
    annotations: Vec<Vec<Value>>,  // placeholders, ignored
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

#[derive(Debug)]
pub struct ArborConnector {
    // todo: cannot derive Deserialize because fields are out of order and in the same array
    //[treenode_id, confidence,
    //     connector_id,
    //     confidence, treenode_id, skeleton_id,
    //     relation_id, relation_id]
    this: Connector,
    this_confidence: u8,
    other: Connector,
    other_confidence: u8,
    other_skeleton_id: u64,
}

impl<'de> Deserialize<'de> for ArborConnector {
    fn deserialize<D>(deserializer: D) -> Result<Self, <D as Deserializer<'de>>::Error> where
        D: Deserializer<'de> {
        struct ArborConnectorVisitor;

        impl<'de> Visitor<'de> for ArborConnectorVisitor {
            type Value = ArborConnector;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("struct Treenode")
            }

            fn visit_seq<V>(self, mut seq: V) -> Result<ArborConnector, V::Error> where V: SeqAccess<'de> {
                let this_treenode_id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(0, &self))?;
                let this_confidence = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(1, &self))?;
                let connector_id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(2, &self))?;
                let other_confidence = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(3, &self))?;
                let other_treenode_id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(4, &self))?;
                let other_skeleton_id = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(5, &self))?;
                let this_relation = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(6, &self))?;
                let other_relation = seq.next_element()?
                    .ok_or_else(|| de::Error::invalid_length(7, &self))?;

                Ok(ArborConnector {
                    this: Connector {
                        treenode_id: this_treenode_id,
                        connector_id,
                        relation: ConnectorRelation::from_json(this_relation)?,
                    },
                    this_confidence,
                    other: Connector {
                        treenode_id: other_treenode_id,
                        connector_id,
                        relation: ConnectorRelation::from_json(other_relation)?,
                    },
                    other_confidence,
                    other_skeleton_id,
                })
            }
        }

        deserializer.deserialize_any(ArborConnectorVisitor)
    }
}

pub trait DescribesConnector {
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
pub struct ArborResponse {
    // Only deserializes for with_time=false
    //
    // [[nodes], [connections], {nodeID: [tags]}]
    //                           ^should be {tag_name: [skeleton_ids]}?
    //
    // Each element in the [nodes] array has the following form:
    //
    //    [id, parent_id, user_id, location_x, location_y, location_z, radius, confidence].
    //
    // Each element in the [connections] array has the following form:
    //[treenode_id, confidence,
    //     connector_id,
    //     confidence, treenode_id, skeleton_id,
    //     relation_id, relation_id]
    treenodes: Vec<Treenode>,
    connectors: Vec<ArborConnector>,
    tags: FnvHashMap<String, Vec<u64>>,
}

enum Response {
    // todo: is this even necessary? They both impl ArborParseable and so are treated the same by callers
    Skeleton(SkeletonResponse),
    Arbor(ArborResponse),
}

#[derive(PartialEq, Debug)]
pub struct ArborParser<NodeType: Hash + Eq + Ord + Copy, F: Float> {
    pub arbor: Arbor<NodeType>,
    pub inputs: FnvHashMap<NodeType, usize>,
    pub outputs: FnvHashMap<NodeType, usize>,
    pub locations: FnvHashMap<NodeType, Location<F>>,
}

impl<NodeType: Hash + Debug + Eq + Ord + Copy, F: Float> Default for ArborParser<NodeType, F> {
    fn default() -> ArborParser<NodeType, F> {
        ArborParser {
            arbor: Arbor::default(),
            inputs: FnvHashMap::default(),
            outputs: FnvHashMap::default(),
            locations: FnvHashMap::default(),
        }
    }
}

impl<NodeType: Hash + Debug + Eq + Ord + Copy, F: Float> ArborParser<NodeType, F> {
    fn create_synapse_map(&self) -> FnvHashMap<NodeType, usize> {
        let mut out: FnvHashMap<NodeType, usize> = FnvHashMap::default();

        for (key, value) in self.inputs.iter().chain(self.outputs.iter()) {
            let mut entry = out.entry(*key).or_insert(0);
            *entry += value;
        }
        out
    }

    fn collapse_artifactual_branches(
        &mut self,
        tags: FnvHashMap<String, Vec<NodeType>>,
    ) -> ArborParser<NodeType, F> {
        unimplemented!();
    }

    // todo: collapse short branches too?
}

impl ArborParser<u64, f64> {
    fn new(response: Response) -> Result<ArborParser<u64, f64>, &'static str> {
        // todo: must be a way to do this with traits
        match response {
            Response::Arbor(r) => r.to_arborparser(),
            Response::Skeleton(r) => r.to_arborparser(),
        }
    }
}

// todo: something like this should be possible
//impl<C: DescribesConnector, P: ArborParseable<C>> ArborParser<u64, f64> {
//    fn new(response: P) -> Result<ArborParser<u64, f64>, &'static str> {
//        P.to_arborparser()
//    }
//}


#[cfg(test)]
mod tests {
    use super::*;
    use serde_json;
    use std::fs::File;
    use std::io::Read;
    use std::path::Path;
    use std::path::PathBuf;

    //            1
    //            |
    //            2
    //            |
    // 13<-(101)<-3
    //            | \
    //            4  6<-(103)<-16
    //            |   \
    // 15<-(102)<-5    7<-(104)<-17

    fn small_arbor() -> Arbor<u64> {
        let edges: Vec<u64> = vec![5, 4, 4, 3, 3, 2, 2, 1, 7, 6, 6, 3];
        let mut arbor = Arbor::default();
        arbor.add_edges(edges);
        arbor
    }

    fn small_arborparser() -> ArborParser<u64, f64> {
        let mut locations: FnvHashMap<u64, Location<f64>> = FnvHashMap::default();
        for idx in 1..8 {
            locations.insert(
                idx as u64,
                Location {
                    x: idx as f64,
                    y: 0.0,
                    z: 0.0,
                },
            );
        }
        let inputs: FnvHashMap<u64, usize> = vec![(6, 1), (7, 1)].into_iter().collect();
        let outputs: FnvHashMap<u64, usize> = vec![(3, 1), (5, 1)].into_iter().collect();

        ArborParser {
            arbor: small_arbor(),
            locations,
            inputs,
            outputs,
        }
    }

    fn read_file(fname: &str) -> String {
        let mut f = File::open(to_path(fname)).expect("file not found");

        let mut contents = String::new();
        f.read_to_string(&mut contents)
            .expect("something went wrong reading the file");
        contents.to_owned()
    }

    fn to_path(fname: &str) -> PathBuf {
        let mut d = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        d.push("resources/test");
        d.push(fname);
        d
    }

    fn get_arbor_str() -> String {
        read_file("small/compact-arbor.json")
    }

    fn get_skeleton_str() -> String {
        read_file("small/compact-skeleton.json")

    }

    #[test]
    fn deser_skeleton_response() {
        let s = get_skeleton_str();
        let skel_response: SkeletonResponse = serde_json::from_str(&s).expect("It didn't work :(");
    }

    #[test]
    fn parser_from_skeleton_response() {
        let s = get_skeleton_str();
        let skel_response: SkeletonResponse = serde_json::from_str(&s).expect("It didn't work :(");
        let ap = ArborParser::new(Response::Skeleton(skel_response)).expect("response was invalid");
        assert_eq!(ap, small_arborparser())
    }

    #[test]
    fn deser_arbor_response() {
        let s = get_arbor_str();
        let arbor_response: ArborResponse = serde_json::from_str(&s).expect("It didn't work :(");
    }

    #[test]
    fn parser_from_arbor_response() {
        let s = get_arbor_str();
        let arbor_response: ArborResponse = serde_json::from_str(&s).expect("It didn't work :(");
        let ap = ArborParser::new(Response::Arbor(arbor_response)).expect("response was invalid");
        assert_eq!(ap, small_arborparser())
    }

    #[test]
    fn parse_real_arbor() {
        let s = read_file("3034133/compact-arbor.json");
        let response: ArborResponse = serde_json::from_str(&s).expect("fail");
        let ap = ArborParser::new(Response::Arbor(response)).expect("fail");
    }

    #[test]
    fn parse_real_skeleton() {
        let s = read_file("3034133/compact-skeleton.json");
        let response: SkeletonResponse = serde_json::from_str(&s).expect("fail");
        let ap = ArborParser::new(Response::Skeleton(response)).expect("fail");
    }

    #[test]
    fn connector_relation_from_i64() {
        let rel = ConnectorRelation::from_i64(0).expect("didn't work");
        assert_eq!(rel, ConnectorRelation::Presynaptic);
    }
}
