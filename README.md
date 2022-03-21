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
slime-break-on-entry	      M-x ... RET
   Install breakpoint on FUNCTION-NAME.
slime-list-breakpoints	      M-x ... RET
   Open a buffer that list the current installed breakpoints.
slime-remove-all-breakpoints  M-x ... RET
   Remove all breakpoints.
slime-remove-breakpoint	      M-x ... RET
   Remove breakpoint on FUNCTION-NAME.
slime-toggle-breakpoint	      M-x ... RET
   Toggle breakpoint on FUNCTION-NAME.
```
