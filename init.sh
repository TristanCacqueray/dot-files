#!/bin/sh -e
# A script to setup a fresh system

# Copy paste:
cat <<EOF>/dev/null
mkdir -p ~/src/github.com/TristanCacqueray/dot-files
git clone https://github.com/TristanCacqueray/dot-files ~/src/github.com/TristanCacqueray/dot-files
ln -sf $(realpath ~/src/github.com/TristanCacqueray/dot-files/rcrc) ~/.rcrc
rcup -v -d ~/src/github.com/TristanCacqueray/dot-files
~/src/github.com/TristanCacqueray/dot-files/init.sh
EOF

function log {
    echo -e "\x1b[01;01;32m[+] $1\x1b[00m"
}

function podenv {
    log "Setup podenv"
    mkdir -p ~/src/github.com/podenv/
    git clone https://github.com/podenv/hub ~/src/github.com/podenv/local-hub
    git clone https://github.com/podenv/devenv ~/src/github.com/podenv/devenv
}

function dnsmasq {
    log "Setup dummy conn"
    cat << EOF | sudo tee /etc/NetworkManager/system-connections/dummy-local0.nmconnection > /dev/null
[connection]
id=dummy-local0
uuid=$(uuidgen)
type=dummy
interface-name=local0

[ipv4]
address1=192.168.42.42/32
dns-search=
method=manual

[ipv6]
addr-gen-mode=stable-privacy
dns-search=
method=auto
EOF
    sudo chmod 0600 /etc/NetworkManager/system-connections/dummy-local0.nmconnection

    log "Enable dnsmasq"
    cat << EOF | sudo tee /etc/NetworkManager/conf.d/00-use-dnsmasq.conf > /dev/null
[main]
dns=dnsmasq
EOF
    sudo chmod 0600 /etc/NetworkManager/conf.d/00-use-dnsmasq.conf

    log "enable /etc/hosts resolv"
    cat << EOF | sudo tee /etc/NetworkManager/dnsmasq.d/02-add-hosts.conf > /dev/null
listen-address=127.0.0.1,192.168.42.42
addn-hosts=/etc/hosts
cache-size=4096
resolv-file=/etc/resolv.conf
EOF
    sudo chmod 0600 /etc/NetworkManager/dnsmasq.d/02-add-hosts.conf
}

function gnome {
    log "Setup GNOME workspace"
    gsettings set org.gnome.mutter dynamic-workspaces false
    gsettings set org.gnome.desktop.wm.preferences num-workspaces 9
    for i in $(seq 1 9); do gsettings set org.gnome.shell.keybindings switch-to-application-${i} []; done
    for i in $(seq 1 9); do gsettings set org.gnome.desktop.wm.keybindings move-to-workspace-${i} "['<Super><Shift>${i}']"; done
    for i in $(seq 1 9); do gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-${i} "['<Super>${i}']"; done

    log "Setup awesome key bindings"
    gsettings set org.gnome.desktop.wm.keybindings cycle-windows "['<Super>Tab']"
    gsettings set org.gnome.desktop.wm.keybindings cycle-windows-backward "['<Shift><Super>Tab']"

    log "Disable sleep"
    gsettings set org.gnome.settings-daemon.plugins.power sleep-inactive-ac-timeout 0

    log "Disable bell and blink"
    gsettings set org.gnome.desktop.wm.preferences audible-bell false
    gsettings set org.gnome.desktop.interface cursor-blink false

    log "Replace caps-lock with control"
    gsettings set org.gnome.desktop.input-sources xkb-options "['caps:ctrl_modifier']"

    # TODO: add custom0 shortcut for term on Super+Enter
}

podenv
dnsmasq
gnome
