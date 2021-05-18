all:
	dhall-to-yaml --file ./config/alacritty.dhall --output ./config/alacritty.yml
	DARK=True dhall-to-yaml --file ./config/alacritty.dhall --output ./config/alacritty-xterm.yml

install:
	sh -c "ln -sf $$(realpath $$(pwd)/rcrc) ~/.rcrc"
	rcup -d . -v
