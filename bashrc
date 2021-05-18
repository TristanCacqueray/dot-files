# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:" ]]
then
    PATH="$HOME/.local/bin:$PATH"
fi
if ! [[ "$PATH" =~ "$HOME/.cabal/bin:" ]]
then
    PATH="$HOME/.cabal/bin:$PATH"
fi

export PATH
alias ls='ls -ap --color=auto'

# https://vrom911.github.io/blog/haskell-aliases
alias cbuild="cabal build --enable-tests --enable-benchmarks --write-ghc-environment-files=always -O0"
alias ctest="cabal test --enable-tests --test-show-details=direct -O0"
alias cbench="cabal bench --enable-benchmarks -O0"
alias crun="cabal run -O0"
alias cinstall="cabal install --installdir=$HOME/.local/bin --overwrite-policy=always --install-method=copy"
alias cclean="cabal clean"
alias cupdate="cabal update"
alias crepl="cabal repl --build-depends pretty-simple"
alias cdoc="cabal haddock --enable-documentation"
alias cdochackage="cabal haddock --enable-documentation --haddock-for-hackage"
alias cdist="cabal sdist"

# ssh
mkdir -p /run/user/$UID/ssh
export SSH_AUTH_SOCK=/run/user/$UID/gnupg/S.gpg-agent.ssh
