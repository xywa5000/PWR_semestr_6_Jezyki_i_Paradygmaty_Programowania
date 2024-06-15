# Instrukcja uruchamiania

Programy napisane dla systemu Linux (Ubuntu 22.04.2 LTS)

## Haskell

Wymagania:

```
The Glorious Glasgow Haskell Compilation System, version 9.4.8
```

Kompilacja i uruchamianie:

```
$ ghc lib_haskell.hs -o lib_haskell
$ ./lib_haskell
```

## Lisp

Wymagania:

```
SBCL 2.1.11.debian
```

Uruchamianie:

```
sbcl --load lib_lisp.lisp
```

## Sml

Wymagania:

```
Standard ML of New Jersey v110.79
```

Uruchamianie:

```
sml lib_sml.sml
```

## Prolog

Wymagania:

```
SWI-Prolog version 8.4.2 for x86_64-linux
```

Uruchamianie:

```
swipl -s lib_prolog.pl -t test.
```

lub

```
swipl
consult('lib_prolog.pl').
test.
```