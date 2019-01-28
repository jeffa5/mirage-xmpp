FROM ocaml/opam2

WORKDIR /home/opam/app

COPY . .
RUN sudo chown -R opam:opam .
RUN opam switch 4.05 && opam update && opam upgrade -y && eval $(opam env) && opam depext --yes --update --install dune && opam depext --yes --update --install $(./project-deps.sh)

EXPOSE 5222

RUN eval $(opam env) && make mirage

CMD mirage/xmpp --hostname="localhost" -l "info" 2>&1
