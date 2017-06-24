---
title: masking a package in gentoo overlay
date: June 24, 2017
---

:PostID: 201
:Title: masking a package in gentoo overlay
:Keywords: gentoo, overlay, mask, selinux, profile
:Categories: notes

It's a post on how to selectively mask a ebuild in the overlay.
The running example will be a package **app-misc/foo** using **systemd** (systemd
is masked in selinux profiles).

Here is how our ebuild looks like:

.. code-block:: bash

    # Copyright 1999-2017 Gentoo Foundation
    # Distributed under the terms of the GNU General Public License v2
    
    DESCRIPTION="An example package with a depend masked in selinux profiles."
    HOMEPAGE="https://trofi.github.io/"
    
    KEYWORDS="~amd64 ~x86 ~ppc ~ppc64"
    LICENSE="public-domain"
    SLOT="0"
    
    # systemd is masked on celinux profiles
    DEPEND="sys-apps/systemd"
    RDEPEND=${DEPEND}

The problem with it is that it's not usable on selinux profiles due to systemd mask:

.. code-block::

    overlay-selective-mask-in-gentoo/app-misc $ repoman full
    
    RepoMan scours the neighborhood...
      dependency.bad [fatal]        3
       app-misc/foo/foo-0.ebuild: DEPEND: ~amd64(hardened/linux/amd64/no-multilib/selinux)
    ['sys-apps/systemd']
       app-misc/foo/foo-0.ebuild: DEPEND: ~amd64(hardened/linux/amd64/selinux)
    ['sys-apps/systemd']
       app-misc/foo/foo-0.ebuild: DEPEND: ~x86(hardened/linux/x86/selinux)
    ['sys-apps/systemd']

Thus our goal is to mask this package on selinux profiles. Unfortunately it
requires quite a few steps to do it:

1. Add **profiles/profiles.desc** file and populate it with all profiles we want
   to override.

   Repoman already shows us **3** profiles we need to override.

   We need to create **profiles/profiles.desc** file with the following contents:

   .. code-block::

       amd64           hardened/linux/amd64/selinux                            stable
       amd64           hardened/linux/amd64/no-multilib/selinux                stable
       x86             hardened/linux/x86/selinux                              stable

2. Create **package.mask** which we will use for our overrides.

   I've added our mask to new **profiles/features/selinux/package.mask** file:

   .. code-block::

       # Sergei Trofimovich <slyfox@gentoo.org> (24 Jun 2016)
       # requires systemd (masked on selinux)
       app-misc/foo

3. Create profile overrides for each profile.

   Let's look in detail on **hardened/linux/x86/selinux** profile. It's override requires
   a single file.

   Let's create **profiles/hardened/linux/x86/selinux/parent** with the following contents:

   .. code-block::

       gentoo:hardened/linux/x86/selinux
       :features/selinux

   Here **gentoo:hardened/linux/x86/selinux** refers to parent profile and **:features/selinux**
   refers to our tiny mixin with single **package.mask**.

4. Enable **portage-2** extension to **metadata/layout.conf**.

   Unfortunately **[repo]:<absolute-path>** is not described in **PMS** and is not portable across
   all Gentoo package managers.

   To enable it for portage we need to add the following in **metadata/layout.conf**:

   .. code-block::

       profile-formats = portage-2

Done!

Now repoman is happy:

.. code-block::

    overlay-selective-mask-in-gentoo/app-misc $ repoman full
    
    RepoMan scours the neighborhood...
    
    Note: use --without-mask to check KEYWORDS on dependencies of masked packages
    Note: use --include-dev (-d) to check dependencies for 'dev' profiles
    
    RepoMan sez: "If everyone were like you, I'd be out of business!"

Full example repository is available at: https://github.com/trofi/overlay-selective-mask-in-gentoo
