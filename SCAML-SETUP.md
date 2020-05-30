### scaml compilation and installation

first of all, have `opam` installed and initialized

(at the project root directory)
$ opam update
$ opam switch create . ocaml-base-compiler.4.09.1
$ git submodule init
$ git submodule update
$ opam install -y vendors/*/*.opam

this should setup a local opam switch and install scaml as well.

### on merlin and emacs

it is highly recommended to install `merlin` for the purpose of
development.

for emacs, the `tuareg` package is recommended for the authoring of
OCaml files.

to make emacs happy with local opam switches, you may find the
following elisp helpful

```elisp
(defun opam-env ()
  (interactive nil)
  (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
    (setenv (car var) (cadr var))))
```

### on utop

it is recommended to launch utop with dune, which could be done as:

(under a project that refers scaml.inlined)
$ dune utop

(in emacs)
$ dune utop . -- -emacs

without doubt, there is a utop mode for emacs available
