# Cameleer2Html

A lexer to translate Cameleer programs to Html files.

---

What is a Cameleer program? For simplicity, we denominate OCaml files with GOSPEL specifications as Cameleer files.

OCaml - https://ocaml.org/about \
GOSPEL - https://github.com/ocaml-gospel/gospel \
Cameleer - https://github.com/ocaml-gospel/cameleer

---

This project uses as a basis a lexer for (pure) OCaml programs by Jean-Christophe Filliâtre and the team behind the course Langages de programmation et compilation 2024-2025 at École Normale Supérieure.

Base lexer - https://usr.lmf.cnrs.fr/~jcf/ens/compil/caml2html.mll.html \
Course - https://usr.lmf.cnrs.fr/~jcf/ens/compil/ \
Jean-Christophe Filliâtre - https://usr.lmf.cnrs.fr/~jcf/index.fr.html \
École Normale Supérieure - https://www.ens.psl.eu/

## Usage

```dune exec bin/cameleer2html.exe -- examples/example.ml "Example" true```
```dune exec cameleer2html -- examples/example.ml "Example" true```

## Styling

The selected color code is personal preference and does not fully cover
every facet of the language, such as official module names.