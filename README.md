# SICP-in-OCaml

## tips
1. build bytes for ocamldebug, add config under root in `dune` file.
```
(executable
 (name NewtonMethod)
 (libraries base stdio)
 (modes byte exe)
```

2. If your IDE does not autocomplete, execute `dune build` after defining libs in the `dune` file
```
(executable
 (name NewtonMethod)
 (libraries base stdio core))
```
3. trace 
In ocaml, I did not find a command similar to `trace` in lisp. So I always paste it into utop.

## Nonsense
- As long as the prefix syntax is supported, it is not so important as to whether it is used or not. But the prefix suspension style looks better.

## Error
After a computer crash, vs code no longer derives the type. When I select the global sandbox, the error message is displayed. `Error starting server: Sandbox initialisation failed: ocaml-lsp-server is not installed`

The reason is that the environment variables are not loaded correctly after the crash.
```
source ~/.profile
```

## ref
- https://ocaml.org/
- https://realworldocaml.org/
- https://mitpress.mit.edu/sites/default/files/sicp/index.html
- https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book.html
- https://github-wiki-see.page/m/codewars/codewars.com/wiki/Language-OCaml
- https://www.cs.cornell.edu/courses/cs3110/2014sp/lecture_notes.php
- https://codology.net/
- https://sicp.readthedocs.io/en/latest/index.html
- https://www.angelfire.com/tx4/cus/pl/pltranslations.html