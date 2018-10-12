use std::hash::Hash;
use Arbor;

pub struct SynapseClustering<NodeType: Hash> {
    arbor: Arbor<NodeType>,
}
