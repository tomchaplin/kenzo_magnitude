run_example: compile_kenzo
	ecl < magnitude-paths-example.lisp

compile_kenzo:
	#!/bin/bash
	pushd kenzo
	ecl < compile.lisp
	popd
