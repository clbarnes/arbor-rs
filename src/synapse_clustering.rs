use std::hash::Hash;
use Arbor;

pub struct SynapseClustering<NodeType: Hash + Clone + Eq> {
    arbor: Arbor<NodeType>,
}
