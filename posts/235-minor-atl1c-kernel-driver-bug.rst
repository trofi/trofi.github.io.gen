---
title: "Minor atl1c kernel driver bug"
date: January 22, 2022
---

:PostID: 235
:Title: "Minor atl1c kernel driver bug"
:Keywords: linux, nixos
:Categories: notes

This weekend I converted my last physical non-NixOS machine to NixOS.
I was postponing it for a while as it's an internet facing machine that
was installed in 2009. I'd prefer smaller downtime if reasonably
possible. It does not have that many serices: **sshd**, **https**
serving a few static files, **IPv6** tunnel and a local CI builder.

This device has 1Gbit ethernet card supported by **atl1c** linux kernel
driver. It always functioned without any problems for me. But one minor
thing always bothered me: on a high network load **top** output shown
kernel threads with suspicious **%d** names:

.. code-block::

    # ping -f 172.16.0.1  # from remove host
    $ top  # atl1c host
    ...
    621 root 20 0 0 0 0 S 11.0 0.0 0:05.01 napi/eth%d-385
    622 root 20 0 0 0 0 S  5.6 0.0 0:02.64 napi/eth%d-386

These look like unsubstituted format strings.

I spent some time in **linux.git** and got nowhere. After that I reported
bug to `netdev@ ML <https://lore.kernel.org/netdev/YewoxYh2jNBnanUM@lunn.ch/T/>`_
and instantly got the patch from Andrew to try:

.. code-block:: diff

    --- a/drivers/net/ethernet/atheros/atl1c/atl1c_main.c
    +++ b/drivers/net/ethernet/atheros/atl1c/atl1c_main.c
    @@ -2706,6 +2706,15 @@ static int atl1c_probe(struct pci_dev *pdev, const struct pci_device_id *ent)
            goto err_alloc_etherdev;
        }
    
    +	if (rtnl_lock_killable()) {
    +		err = -EINTR;
    +		goto err_init_netdev;
    +	}
    +	err = dev_alloc_name(netdev, netdev->name);
    +	rtnl_unlock();
    +	if (err < 0)
    +		goto err_init_netdev;
    +
        err = atl1c_init_netdev(netdev, pdev);
        if (err) {
            dev_err(&pdev->dev, "init netdevice failed\n");
    

NixOS kernel patching nano howto
--------------------------------

What is the simplest way to try it out on a live system?

I gave `boot.kernelPatches <https://nixos.wiki/wiki/Linux_kernel#Custom_configuration>`_
mechanism a try:

.. code-block:: nix

    # in /etc/nixos/configuration.nix
    { config, pkgs, ... }: {
    ...
      boot.kernelPatches = [
        ({ name = "atl1c-netdev"; patch = ./atl1c-netdev.patch; })
      ];

This change applies extra local patche to current kernel. Building
patched kernel for the next boot:

.. code-block::

    # nixos-rebuild boot
    ...
    # systemctl reboot

Checking thread names after reboot:

.. code-block::

    # ping -f 172.16.0.1  # from remove host
    $ top  # atl1c host
    ...
    613 root 20 0 0 0 0 S 11.0 0.0 0:07.46 napi/eth0-385
    614 root 20 0 0 0 0 R  5.3 0.0 0:03.96 napi/eth0-386

The patch made things better!

Testing in a VM before giving it a go on real device
----------------------------------------------------

It's not very useful for this specific case as we are testing changes to
a hardware driver without emulation available. But for many other kernel
changes before booting into a brand new kernel we could use **qemu** to
have a smoke test for new functionality.

I do it all the time when I poke at kernel's VM subsystem.

You can even boot up your full system up to desktop environment:

.. code-block::

    $ nixos-rebuild build-vm
    ...
    Done.  The virtual machine can be started by running /nix/store/bnl5jjic2rpbxc8p7y92znyzzp1xz59i-nixos-vm/bin/run-nz-vm
    $ /nix/store/bnl5jjic2rpbxc8p7y92znyzzp1xz59i-nixos-vm/bin/run-nz-vm

On my desktop this boots up my **i3** desktop in a second. After a session
shutdown disk image takes only **8MB** of space:

.. code-block::

    $ ls -lh
    total 8.5M
    -rw-r--r-- 1 slyfox users 8.5M Jan 22 20:24 nz.qcow2
    lrwxrwxrwx 1 slyfox users   52 Jan 22 20:22 result -> /nix/store/bnl5jjic2rpbxc8p7y92znyzzp1xz59i-nixos-vm

It was instructive for me to have a peek at what the **run-nz-vm** script actually does:

.. code-block:: bash

    #! /nix/store/pbfraw351mksnkp2ni9c4rkc9cpp89iv-bash-5.1-p12/bin/bash
    NIX_DISK_IMAGE=$(readlink -f "${NIX_DISK_IMAGE:-./nz.qcow2}")
    if ! test -e "$NIX_DISK_IMAGE"; then
        /nix/store/k48rmda1r7is6v2n0jdpfmaax74l8lbq-qemu-host-cpu-only-6.1.0/bin/qemu-img create -f qcow2 "$NIX_DISK_IMAGE" \
           1024M
    fi
    # Create a directory for storing temporary data of the running VM.
    if [ -z "$TMPDIR" ] || [ -z "$USE_TMPDIR" ]; then
        TMPDIR=$(mktemp -d nix-vm.XXXXXXXXXX --tmpdir)
    fi
    # Create a directory for exchanging data with the VM.
    mkdir -p "$TMPDIR/xchg"
    cd "$TMPDIR"
    # Start QEMU.
    exec /nix/store/k48rmda1r7is6v2n0jdpfmaax74l8lbq-qemu-host-cpu-only-6.1.0/bin/qemu-kvm -cpu qemu64 \
        -name nz \
        -m 1024 \
        -smp 1 \
        -device virtio-rng-pci \
        -net nic,netdev=user.0,model=virtio -netdev user,id=user.0,"$QEMU_NET_OPTS" \
        -virtfs local,path=/nix/store,security_model=none,mount_tag=nix-store \
        -virtfs local,path="${SHARED_DIR:-$TMPDIR/xchg}",security_model=none,mount_tag=shared \
        -virtfs local,path="$TMPDIR"/xchg,security_model=none,mount_tag=xchg \
        -drive cache=writeback,file="$NIX_DISK_IMAGE",id=drive1,if=none,index=1,werror=report -device virtio-blk-pci,drive=drive1 \
        -device virtio-keyboard \
        -usb \
        -device usb-tablet,bus=usb-bus.0 \
        -kernel /nix/store/0cqyh4p5jr4dklzvh97b3ip9ky52596l-nixos-system-nz-22.05pre346004.5aaed40d22f/kernel \
        -initrd /nix/store/0cqyh4p5jr4dklzvh97b3ip9ky52596l-nixos-system-nz-22.05pre346004.5aaed40d22f/initrd \
        -append "$(cat /nix/store/0cqyh4p5jr4dklzvh97b3ip9ky52596l-nixos-system-nz-22.05pre346004.5aaed40d22f/kernel-params) init=/nix/store/0cqyh4p5jr4dklzvh97b3ip9ky52596l-nixos-system-nz-22.05pre346004.5aaed40d22f/init regInfo=/nix/store/n3phlr6zmr84gfmm03pq9hfi8p1i51qy-closure-info/registration console=ttyS0,115200n8 console=tty0 $QEMU_KERNEL_PARAMS" \
        $QEMU_OPTS \
        "$@"

Here the script creates **nz.qcow2** disk in  **qcow2** format of **1GB**
size max. But even that space is not populated! The whole of **/nix/store**
gets exported via **virtfs** which allows not to copy most of packages into
a VM at all! Do we even have a root device here?

Let's peek at **initrd** contents. How does it make the magic happen?

.. code-block::

    $ cat /nix/store/0cqyh4p5jr4dklzvh97b3ip9ky52596l-nixos-system-nz-22.05pre346004.5aaed40d22f/initrd | file -
    /dev/stdin: Zstandard compressed data (v0.8+), Dictionary ID: None
    $ cat /nix/store/0cqyh4p5jr4dklzvh97b3ip9ky52596l-nixos-system-nz-22.05pre346004.5aaed40d22f/initrd | zstd -d | file -
    /dev/stdin: ASCII cpio archive (SVR4 with no CRC)
    $ mkdir initramfs && cd initramfs
    $ cat /nix/store/0cqyh4p5jr4dklzvh97b3ip9ky52596l-nixos-system-nz-22.05pre346004.5aaed40d22f/initrd | zstd -d | cpio -id --no-preserve-owner
    $ ls
    dev  etc  init  nix  proc  sys
    $ find nix/store/ -maxdepth 1
    nix/store/
    nix/store/...-mdadm.conf
    nix/store/...-libunistring-0.9.10
    nix/store/...-pcre-8.45
    nix/store/...-mounts.sh
    nix/store/...-glibc-2.33-59
    nix/store/...-acl-2.3.1
    nix/store/...-udev-rules
    nix/store/...-linux-5.16-modules-shrunk
    nix/store/...-stage-1-init.sh
    nix/store/...-gnugrep-3.7
    nix/store/...-extra-utils
    nix/store/...-libidn2-2.3.2
    nix/store/...-findutils-4.8.0
    nix/store/...-kmod-debian-aliases-22-1.1.conf
    nix/store/...-keymap
    nix/store/...-link-units
    nix/store/...-attr-2.5.1
    nix/store/...-initrd-fsinfo
    nix/store/...-coreutils-9.0
    nix/store/...-initrd-kmod-blacklist-ubuntu

Here we see that **initramfs** already has a bit (actually, **40MB** due
to glibc) of **/nix/store** embedded in to support **init** top-level script.
**init** is a big but straightforward script. It does a few things:

- formats **/dev/vda** if filesystem is absent and places minimal
  root NixOS directory structure on it.
- mounts root device into **$target** (it suports discovery of ISO, standard NixOS,
  conversions from other distibutions, aka "lustrations"). One of the steps
  here is to mount passed in virtfs as a **$target/nix/store**. And make it writeble
  by overlaying **tmpfs** on top. For live ISO images this allows you to upgrade
  live image im-memory before installing it on disk.
- switches root to a new target

We can also boot **qemu** VM in text mode to ease copying terminal
output and poke at just created root:

.. code-block::

    $ ./result/bin/run-nz-vm -nographic
    ...
    <<< Welcome to NixOS 22.05pre346004.5aaed40d22f (x86_64) - ttyS0 >>>
    Run 'nixos-help' for the NixOS manual.
    login: foo (automatic login)
    $ mkdir /tmp/r
    $ sudo mount --bind / /tmp/r
    $ find /tmp/r/
    /tmp/r/
    /tmp/r/proc
    /tmp/r/dev
    /tmp/r/etc
    /tmp/r/etc/.clean
    /tmp/r/etc/binfmt.d
    /tmp/r/etc/binfmt.d/nixos.conf
    /tmp/r/etc/nscd.conf
    /tmp/r/etc/ssh
    ...

If you plan to work on boot loader changes you can build a VM with boot loader as
**nixos-rebuild build-vm-with-bootloader**.

Have fun!