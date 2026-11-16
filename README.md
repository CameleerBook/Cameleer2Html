# Cameleer2Html

A lexer to generate HTML files from Cameleer (OCaml + GOSPEL) programs

---

What is a Cameleer program? For simplicity, we denominate OCaml files with
GOSPEL specifications as Cameleer files.

OCaml - https://ocaml.org/about \
GOSPEL - https://github.com/ocaml-gospel/gospel \
Cameleer - https://github.com/ocaml-gospel/cameleer

---

This project uses as a basis a lexer for (pure) OCaml programs by 
Jean-Christophe Filliâtre and the team behind the course Langages de programmation et compilation 2024-2025 at École Normale Supérieure.

Base lexer - https://usr.lmf.cnrs.fr/~jcf/ens/compil/caml2html.mll.html \
Course - https://usr.lmf.cnrs.fr/~jcf/ens/compil/ \
Jean-Christophe Filliâtre - https://usr.lmf.cnrs.fr/~jcf/index.fr.html \
École Normale Supérieure - https://www.ens.psl.eu/

## Installation

The first step to install Cameleer2Html is to clone the repository
```
$ git clone https://github.com/CameleerBook/Cameleer2Html.git
```

Next, pin the `cameleer2html` package. Note: Requires `opam`
```
$ opam pin add path/to/cameleer2html
``` 

After installation succeeds, you can try `cameleer2html` by doing
```
$ cameleer2html --version
```

## Usage

Basis usage:
```
$ cameleer2html <file.ml>
```
Generates a file name `<file.ml>.html`. Does not display line count, and the
title use on the HTML header is `<file.ml>`.

Optional flags:
- `-l` - prints line numbers.
<!-- - `-o <file>` - chooses the output file name (and location). The `.html` extension is optional. -->
- `-t <title>` - sets the title used on the generated HTML header.

Advanced usage example:
<!--
```
$ cameleer2html <file.ml> -l -o <output_file> -t <title>
```
-->

```
$ cameleer2html <file.ml> -l -t <title>
```

## Usage (Developer)

```
dune exec bin/cameleer2html.exe -- examples/example.ml -l -t "Example"
dune exec cameleer2html -- examples/example.ml -l -t "Example"
```

## Styling

The selected color code is personal preference and does not fully cover
every facet of the language, such as official module names.