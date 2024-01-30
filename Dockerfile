FROM haskell:9.6.4

WORKDIR /opt/hy-compilers

RUN cabal update

COPY hy-compilers.cabal /opt/hy-compilers/hy-compilers.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/hy-compilers/
RUN cabal install

CMD ["hy-compilers"]