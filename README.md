WIP

# arbor-rs

A rust implementation of 
[Arbor.js](https://github.com/catmaid/CATMAID/blob/master/django/applications/catmaid/static/libs/catmaid/Arbor.js)
and associated tools.

## Testing

Integration tests depend on arbor-harness, a git submodule which downloads the reference implementation, 
runs and benchmarks it, and dumps the output to JSON.
This requires node.js (ideally v11+), and can be populated thus:

```bash
git submodules --init --recursive
cd resources/test/arbor-harness
npm install
npm start
```
