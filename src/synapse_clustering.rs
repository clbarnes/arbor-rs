use std::hash::Hash;
use Arbor;

pub struct SynapseClustering<NodeType: Hash + Clone> {
    arbor: Arbor<NodeType>,
}
