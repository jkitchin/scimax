export PATH="$HOME/.cask/bin:$HOME/.evm/bin:$PATH"

git clone https://github.com/rejeep/evm.git "$HOME/.evm"
evm config path /tmp
evm install "$EVMVERSION" --use --skip
export EMACS="$(evm bin)"

curl -fsSkL https://raw.github.com/cask/cask/master/go | python
