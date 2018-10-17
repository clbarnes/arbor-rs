use fnv::FnvHashMap;
use num::traits::float::Float;
use num::FromPrimitive;
use serde::Deserialize;
use serde::Deserializer;
use std::fmt::Debug;
use std::hash::Hash;
use utils::Location;
use Arbor;

enum_from_primitive! {
#[derive(Deserialize, Debug, PartialEq, Clone)]
enum ConnectorRelation {
    // 0 = presynaptic, 1 = postsynaptic, 2 = gap junction, -1 = other:
    Presynaptic = 0,
    Postsynaptic = 1,
    GapJunction = 2,
    Other = -1
}
}

struct ArborLocations<NodeType: Hash + Clone + Eq, F: Float> {
    arbor: Arbor<NodeType>,
    locations: FnvHashMap<NodeType, Location<F>>,
}

struct InputsOutputs<NodeType> {
    inputs: FnvHashMap<NodeType, usize>,
    outputs: FnvHashMap<NodeType, usize>,
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
        let mut inputs: FnvHashMap<u64, usize> = FnvHashMap::default();
        let mut outputs: FnvHashMap<u64, usize> = FnvHashMap::default();

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
            arbor: Arbor::from(edges, root)?,
            locations,
        })
    }
}

#[derive(Deserialize, Debug, PartialEq)]
struct Treenode {
    // todo: cannot derive Deserialize because Location needs to be collected
    // [id, parent_id, user_id, location_x, location_y, location_z, radius, confidence]
    id: u64,
    parent_id: Option<u64>,
    user_id: u64,
    location: Location<f64>,
    radius: f64,
    confidence: u8,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
struct Connector {
    // todo: cannot derive Deserialize because Arbor and SkeletonConnectors use different ordering,
    // and need to serialise confidences
    treenode_id: u64,
    connector_id: u64,
    relation: ConnectorRelation,
}

#[derive(Deserialize, Debug)]
struct SkeletonConnector {
    // todo: cannot derive Deserialize because Location needs to be collected
    // 0 = presynaptic, 1 = postsynaptic, 2 = gap junction, -1 = other:
    //
    //    [treenode_id, connector_id, 0|1|2|-1, location_x, location_y, location_z]
    this: Connector,
    location: Location<f64>,
}

#[derive(Deserialize, Debug)]
struct SkeletonResponse {
    // Only deserializes for with_history=false, with_merge_history=false, with_reviews=false,
    // with_annotations=false, with_user_info=false
    //
    // [[nodes], [connectors], {nodeID: [tags]}]
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
    // todo: cannot derive Deserialize because fields are out of order and in the same array
    //[treenode_id, confidence,
    //     connector_id,
    //     confidence, treenode_id, skeleton_id,
    //     relation_id, relation_id]
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
    // Only deserializes for with_time=false
    //
    // [[nodes], [connections], {nodeID: [tags]}]
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

impl<NodeType: Hash + Debug + Eq + Ord + Copy, F: Float> ArborParser<NodeType, F> {
    fn new() -> ArborParser<NodeType, F> {
        ArborParser {
            arbor: Arbor::new(),
            inputs: FnvHashMap::default(),
            outputs: FnvHashMap::default(),
            locations: FnvHashMap::default(),
        }
    }

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
    fn from_response(response: Response) -> Result<ArborParser<u64, f64>, &'static str> {
        match response {
            Response::Skeleton(r) => r.to_arborparser(),
            Response::Arbor(r) => r.to_arborparser(),
        }
    }
}

// todo: implement Deserialize for ArborResponse
// SkeletonResponse should be fine?

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json;
    use std::fs::File;

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
        let mut arbor = Arbor::new();
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

    fn get_arbor_str() -> &'static str {
        // todo: read from file instead
        "[
          [
            [1, null, 1001, 1, 0, 0, -1, 5],
            [2, 1, 1001, 2, 0, 0, -1, 5],
            [3, 2, 1001, 3, 0, 0, -1, 5],
            [4, 3, 1001, 4, 0, 0, -1, 5],
            [5, 4, 1001, 5, 0, 0, -1, 5],
            [6, 3, 1001, 6, 0, 0, -1, 5],
            [7, 6, 1001, 7, 0, 0, -1, 5]
          ],
          [
            [3, 5, 101, 5, 13, 10001, 0, 1],
            [5, 5, 102, 5, 15, 10002, 0, 1],
            [6, 5, 103, 5, 16, 10003, 1, 0],
            [7, 5, 104, 5, 17, 10004, 1, 0]
          ],
          {
            \"ends\": [7, 5],
            \"soma\": [1],
            \"mitochondrion\": [2, 4]
          }
        ]"
    }

    fn get_skeleton_str() -> &'static str {
        // todo: read from file instead
        "[
          [
            [1, null, 1001, 1, 0, 0, -1, 5],
            [2, 1, 1001, 2, 0, 0, -1, 5],
            [3, 2, 1001, 3, 0, 0, -1, 5],
            [4, 3, 1001, 4, 0, 0, -1, 5],
            [5, 4, 1001, 5, 0, 0, -1, 5],
            [6, 3, 1001, 6, 0, 0, -1, 5],
            [7, 6, 1001, 7, 0, 0, -1, 5]
          ],
          [
            [3, 101, 0, 3, 1, 0],
            [5, 102, 0, 5, 1, 0],
            [6, 103, 1, 6, 1, 0],
            [7, 104, 1, 7, 1, 0]
          ],
          {
            \"ends\": [7, 5],
            \"soma\": [1],
            \"mitochondrion\": [2, 4]
          }
        ]"
    }

    #[test]
    fn deser_skeleton_response() {
        let s = get_skeleton_str();
        let skel_response: SkeletonResponse = serde_json::from_str(s).expect("It didn't work :(");
    }

    #[test]
    fn skeleton_response_to_parser() {
        let s = get_skeleton_str();
        let skel_response: SkeletonResponse = serde_json::from_str(s).expect("It didn't work :(");
        let ap = skel_response.to_arborparser();
    }

    #[test]
    fn parser_from_skeleton_response() {
        let s = get_skeleton_str();
        let skel_response: SkeletonResponse = serde_json::from_str(s).expect("It didn't work :(");
        let ap = ArborParser::from_response(Response::Skeleton(skel_response));
    }

    #[test]
    fn deser_arbor_response() {
        let s = get_arbor_str();
        let arbor_response: ArborResponse = serde_json::from_str(s).expect("It didn't work :(");
    }

    #[test]
    fn arbor_response_to_parser() {
        let s = get_arbor_str();
        let arbor_response: ArborResponse = serde_json::from_str(s).expect("It didn't work :(");
        let ap = arbor_response.to_arborparser();
    }

    #[test]
    fn parser_from_arbor_response() {
        let s = get_arbor_str();
        let arbor_response: ArborResponse = serde_json::from_str(s).expect("It didn't work :(");
        let ap = ArborParser::from_response(Response::Arbor(arbor_response));
    }

}
