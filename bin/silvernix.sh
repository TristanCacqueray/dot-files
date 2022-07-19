#!/bin/sh
# Install podenv nix store on fedora silverblue

if ! test -d /nix; then
    sudo bash -c "chattr -i /; mkdir /nix; chattr +i /"
fi

if test -d ~/.local/share/podenv/volumes/nix-store/store/ && ! test -d /nix/store/; then
    sudo mount -o bind ~/.local/share/podenv/volumes/nix-store /nix;
fi

echo "# Run this command to get started:"
echo "export PATH=$PATH:/nix/var/nix/profiles/nix-install/bin"
