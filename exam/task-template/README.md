## Building and running Oh Hell

### Using stack

Install the haskell stack from <https://docs.haskellstack.org/en/stable/>.
This configuration is known to work with 
```bash
template % stack --version
Version 2.9.3, Git revision 6cf638947a863f49857f9cfbf72a38a48b183e7e x86_64 hpack-0.35.1
```

To build 
```bash
$ stack build
```

To display the help message
```bash
$ stack run ohhell -- -h
```

To play the game interactively
```bash
$ stack run ohhell -- -i `whoami`
```

### Using cabal

Tested with
```bash
template % ghc --version
The Glorious Glasgow Haskell Compilation System, version 9.4.4
template % cabal --version
cabal-install version 3.8.1.0
compiled using version 3.8.1.0 of the Cabal library 
```

To build
```bash
$ cabal build
```

To display the help message
```bash
$ cabal run ohhell -- -h
```

To play the game interactively
```bash
$ cabal run ohhell -- -i `whoami`
```

