---
title: "one-liner to fix CSS on phones"
date: August 04, 2022
---

I noticed that this blog was unreadable on the phones: main text was
readable, but code snippets had 2.5x smaller fonts and required manual
zooming to see anything at all.

Apparently it was happening because on small screens browsers try to
be smart and scale some UI elements up. But not all HTML
tags get the same treatment. As a result you get size inconsistencies
where none were before.

Apparently you can (and need) to opt out of that resizing behavior! The
magic `HTML5` one-liner to do it is:

```html
<meta name="viewport" content="width=device-width, initial-scale=1.0">
```

There are many other subtleties one needs to account for, like scaling
pictures accordingly or handling sidebars to overflow in a neat way.
The set of techniques are called `HTML Responsive Web Design`.

Luckily I don't need to care about any of that.
