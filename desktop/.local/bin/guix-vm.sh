#!/bin/sh

qemu-system-x86_64 -enable-kvm -m 4096 -cpu host \
  -device virtio-blk,drive=myhd -drive if=none,file=$(realpath ~/vm/guix-system-vm-image-1.4.0.x86_64-linux.qcow2),id=myhd \
  -virtfs local,path=$(realpath ~/src/github.com/TristanCacqueray/podenv-guix/),mount_tag=host0,security_model=passthrough,id=host0
