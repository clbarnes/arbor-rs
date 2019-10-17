WIP

# arbor-rs

A rust implementation of 
[Arbor.js](https://github.com/catmaid/CATMAID/blob/master/django/applications/catmaid/static/libs/catmaid/Arbor.js)
and associated tools.

## Testing

Integration tests depend on arbor-harness, an npm package which downloads the reference implementation, 
runs and benchmarks it, and dumps the output to JSON.
This requires node.js (ideally v11+), and can be populated thus:

```bash
npm install -g arbor-harness
# populate CATMAID's implementation
arbor-harness impl
# download test data
arbor-harness data -d ./resources/test/3034133
# run and benchmark reference implementation
arbor-harness results bench -d ./resources/test/3034133 -o ./resources/test/3034133/results

```

Run
