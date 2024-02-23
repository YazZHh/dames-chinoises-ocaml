# dames-chinoises-ocaml
Projet encadré porté sur le jeux des dames chinoises, développé en programmation fonctionnelle en OCaml

# Installation
Pour faire tourner ce programme, il vous faut installer OCaml
- Sur linux c'est très simple, voir [ce guide](https://v2.ocaml.org/docs/install.fr.html#Linux). Il ne reste plus qu'à clone le repo.
- Sur Windows, téléchargez puis installer OCaml depuis [cette page](https://fdopen.github.io/opam-repository-mingw/installation/). Puis une fois l'installation terminée, ouvrez le raccourci `Cygwin64 Terminal` sur votre bureau puis entrez ces commandes (en supposant que vous avez Visual Studio Code d'installé) :
```
opam install dune utop ocaml-lsp-server
git clone https://github.com/YazZHh/dames-chinoises-ocaml
cd dames-chinoises-ocaml
code .
```
L'interpréteur OCaml peut être lancé en entrant tout simplement la commande `ocaml` dans le terminal.
