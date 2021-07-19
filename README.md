# SICP-in-OCaml

## tips
1. build bytes for ocamldebug, add config under root in `dune` file.
```
(executable
 (name NewtonMethod)
 (libraries base stdio)
 (modes byte exe)
```

2. If your IDE does not provide suggestions, execute `dune build` after defining libs in the `dune` file
```
(executable
 (name NewtonMethod)
 (libraries base stdio core))
```
3. trace 