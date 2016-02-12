---
title: doxygen for lazies
date: February 2, 2016
---

:PostID: 190
:Title: doxygen for lazies
:Keywords: c, c++, java, doxygen, re2c, doxify
:Categories: notes

TL;DR: to get doxygen docs really fast you can run the
`doxify <https://github.com/trofi/home/blob/master/bin/doxify>`_
script as:

.. code-block:: bash

    $ doxify path-to-project path-to-docs
    # Done!
    $ firefox path-to-docs/index.html

.. raw:: html

   <!--more-->

Let's do the same for a small yet complex real-world C++ project: **re2c**.

The session looks like:

.. code-block:: bash

    $ git clone https://github.com/skvadrik/re2c
    $ doxify re2c/re2c/src re2c_docs
    warning: Output language Russian not supported! Using English instead.
    warning: failed to open layout file 'DoxygenLayout.xml' for reading!
    warning: Included by graph for 'c99_stdint.h' not generated, too many nodes. Consider increasing DOT_GRAPH_MAX_NODES.
    $ firefox re2c_docs/index.html

The docs generation process takes 9 seconds on my machive.
I've uploaded result `here <../posts.data/190-doxy/re2c_docs/index.html>`_.
I won't update that documentation thus it's frost in time.

By default doxygen generates documentation only for explicitly documented
files and functions thus we override that behaviour and force it to tell
us everything it knows about the project. The following **doxify** line
is responsible for it:

.. code-block:: bash

    doxygen_set_value "EXTRACT_ALL" "YES"

Most of the rest is a nicety to get a code browser inline with documentation.

Let's look at actually generated stuff. There is:

- `Namespaces </posts.data/190-doxy/re2c_docs/namespaces.html>`_
- `Classes </posts.data/190-doxy/re2c_docs/classes.html>`_
- `Files </posts.data/190-doxy/re2c_docs/files.html>`_

Under **Classes** there is **Class Hierarchy** dropdown.
Some complex projects have huge class hierachies.
One click and you know **re2c** is `not one of them </posts.data/190-doxy/re2c_docs/inherits.html>`_ :)

Under **Files** we can find a lot of useful info as well.
Picking `main.cc </posts.data/190-doxy/re2c_docs/main_8cc.html>`_ as an example:

- Transitive header inclusion graph. If you click on a header you'll get both direct and reverse inclusion graphs:
  `input_api.h </posts.data/190-doxy/re2c_docs/input__api_8h.html>`_
- Highlighted function definition with clickable cross-references: `main() funcion </posts.data/190-doxy/re2c_docs/main_8cc.html#a97b0fa62b7b0972875f5f589322c4c24>`_
- Original source code with highlight fancy and cross-reference links: `main() again </posts.data/190-doxy/re2c_docs/main_8cc_source.html#l00026>`_

More random examples:

- `re2c::Output </posts.data/190-doxy/re2c_docs/structre2c_1_1Output.html>`_
- clicking at the `legend </posts.data/190-doxy/re2c_docs/graph_legend.html>`_ under any graph will decipher arrow colors
- **~Output()** destructor has a `nice callgraph </posts.data/190-doxy/re2c_docs/structre2c_1_1Output.html#a1cc81b46a98f3ada41135bb395df0c55>`_
- **re2c::matches** does not call anyone but is called occasionally: `its reverse callgraph </posts.data/190-doxy/re2c_docs/namespacere2c.html#a79ad7b02c4996a9bab41faabd451d624>`_

Have fun!
