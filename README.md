# SLIME-BREAKPOINTS

SLIME extension for setting up breakpoints from Emacs.

## Install

ℹ️ Please consider using [SLIME :star:](https://github.com/mmontone/slime-star), that comes with this extension preinstalled.

Load `swank` and add this repository path to `swank::*load-path*`, in your Lisp compiler init file (~/.sbclrc if using SBCL):

```lisp
(require :swank)
(push #p"~/slime-breakpoints/" swank::*load-path*)
```

In Emacs, add this repository path to `load-path` and `slime-breakpoints` to `slime-contribs` in `~/.emacs` init file, like:

```
(push "~/slime-breakpoints" load-path)

(setq slime-contribs '(slime-fancy slime-breakpoints))

(slime-setup)
```

## Use

```
slime-break-on-entry	      C-c b RET
   Install breakpoint on FUNCTION-NAME.
slime-list-breakpoints	      C-c b l
   Open a buffer that list the current installed breakpoints.
slime-remove-all-breakpoints  C-c b q
   Remove all breakpoints.
slime-remove-breakpoint	      C-c b <deletechar>
   Remove breakpoint on FUNCTION-NAME.
slime-toggle-breakpoint	      C-c b SPC
   Toggle breakpoint on FUNCTION-NAME.
slime-break-with-last-expression
   Compile function at point with a BREAK at last expression position.
slime-trace-last-expression
   Compile function at point with a 'trace' expression at last expression position.
```

You can also use from Common Lisp directly. The `breakpoints` package exports this functions:

### break-on-entry

```lisp
(function-name)
```

Setup a breakpoint on entry on FUNCTION-NAME.

### breakpoint-installed-p

```lisp
(function-name)
```

Wether a breakpoint is installed on FUNCTION-NAME.

### reinstall-all-breakpoints

```lisp
nil
```

Reinstall all breakpoints.

When a function is recompiled, the breakpoint is lost. A call to this function reintalls all breakpoints.

### reinstall-breakpoint

```lisp
(function-name)
```

Reinstall breakpoint on FUNCTION-NAME.

When a function is recompiled, the breakpoint is lost. A call to this function reinstalls the breakpoint.

### remove-all-breakpoints

```lisp
nil
```

Remove all installed breakpoints.

### remove-breakpoint

```lisp
(function-name)
```

Remove breakpoint on FUNCTION-NAME.

### toggle-breakpoint

```lisp
(function-name)
```

Toggle breakpoint on FUNCTION-NAME.
