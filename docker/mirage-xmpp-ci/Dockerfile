FROM ocaml/opam2

ARG opam_deps

RUN opam switch 4.05 \
    && opam update && opam upgrade -y \
    && opam depext --yes --update --install $opam_deps

WORKDIR /home/opam/app
