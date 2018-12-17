FROM ocaml/opam2

RUN opam switch 4.05 \
    && opam update \
    && opam upgrade -y \
    && opam depext --yes --update --install dune

WORKDIR /home/opam/app

COPY . .

RUN sudo chown -R opam:opam .
