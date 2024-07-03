# MPSS on Kenzo

## Setup

First, you need an implementation of the Common Lisp language.
So far I have been using `ecl` (Embeddable Common Lisp), for more information on this see [here](https://ecl.common-lisp.dev/main.html).
`ecl` is available in the package managers of most linux distros, or on MacOS via `homebrew`

```
brew install ecl
```

Now clone this git repo, recursively with submodules so that you pull in the kenzo source that we require:
```
git clone --recurse-submodules git://github.com/tomchaplin/kenzo_magnitude.git
```

Next we need to compile kenzo so it is quick to load up and use
```
pushd kenzo
ecl < compile.lisp
popd
```

Now we are ready to start computing the MPSS.

## Usage

All of the functionality is implemented in `magnitude-paths.lisp` and is pretty poorly documented atm.
Example usage is shown in `magnitude-paths-example.lisp` where we compute the ranks of the MPSS up to page 4 for some small digraphs.
To run this example, simply run
```
ecl < magnitude-paths-example.lisp
```

## TODO

Usability:
- [ ] Document functions
- [ ] Python bindings
- [ ] Print rank tables with options for magnitude indexing vs spectral sequence indexing
- [ ] Read out generators
- [ ] Build binary that accepts distance matrix string from stdin

Performance:
- [ ] Cache basis computations
- [ ] Use previous basis to speed-up computation of new one (discard any infinite-length paths)
- [ ] Parallelise basis computation
- [ ] Switch over to integer representation of path
- [ ] Get working on `sbcl`
