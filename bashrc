# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/.cabal/bin:$HOME/.cargo/bin:$HOME/bin:$PATH"
fi
export PATH
export LC_ALL=C.UTF-8
if [ -f $HOME/.nix-profile/bin/devenv-profile ]; then
   . $HOME/.nix-profile/bin/devenv-profile
fi

# Ensure emacs magit buffer are readable
export TERM=xterm-256color
alias tmux='tmux -2'

alias ls='ls -ap --color=auto'
alias ctop='systemd-cgtop -c --cpu=time -1 --depth=10 -k'
alias journalctlf='script -fq /dev/null -c "journalctl --no-hostname -f"  | grep -v syscall=179'

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
alias cghcid="ghcid --command='cabal v2-repl'"

test -z "$SSH_CONNECTION" || export TERM=xterm-256color

# erlang repl history
export ERL_AFLAGS="-kernel shell_history enabled"
