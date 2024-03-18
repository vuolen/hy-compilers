# hy-compilers

This compiler is written in Haskell. I'm sure you who are reviewing my code
might be fed up with Haskell projects, but I hope you find reassurance in the
fact that this course taught me a lot about both compilers and real-life Haskell.

To make things easy, the compiler runs in Docker and is fed the source code
through stdin. The compiled product comes out of stdout and is fed to the
provided `assembler.py`.

## How to run

1. Build the compiler with `docker build . -t hy-compilers`
   - The docker build also runs the unit tests
2. Run the compiler with `./run.sh [infile] [outfile]`

To run the e2e tests, use `python e2e_tests.py` (build the Docker image first)
