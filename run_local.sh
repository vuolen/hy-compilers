# for development, doesnt use docker
cat $1 | cabal run hy-compilers -v0 | python assembler.py $2