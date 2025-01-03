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

### Breakpoints

- **slime-break-on-entry** *C-c b RET* - Install breakpoint on FUNCTION-NAME.
- **slime-list-breakpoints** *C-c b l* - Open a buffer that list the current installed breakpoints.
- **slime-remove-all-breakpoints**  *C-c b q* - Remove all breakpoints.
- **slime-remove-breakpoint** *C-c b <deletechar>* - Remove breakpoint on FUNCTION-NAME.
- **slime-toggle-breakpoint** *C-c b SPC* - Toggle breakpoint on FUNCTION-NAME.
- **slime-break-with-last-expression** - Compile function at point with a BREAK at last expression position.

### Tracing
- **slime-trace-last-expression** - Compile function at point with a 'trace' expression at last expression position.

### Stepping

- **slime-step-in-last-expression** - Compile function at point with a 'step' expression at last expression position.
- **slime-step-in-next-expression** - Compile function at point with a 'step' expression at next expression position.

### Debug printing

These commands require [cl-debug-print](https://github.com/masatoi/cl-debug-print) installed (it is available via Quicklisp).

- **slime-debug-print-next-expression** - Instrument next expression to be debug printed when evaluated.
- **slime-debug-print-last-expression** - Instrument last expression to be debug printed when evaluated.

## Common Lisp api

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

# SLDB-SOURCE-EVAL

SLIME debugger (SLDB) extension that adds debugger context based evaluation directly from Lisp source buffers.

![sldb-source-eval](sldb-source-eval.png)
![sldb-source-eval](sldb-source-eval.gif)

When SLIME debugger (SLDB) opens, move cursor to a backtrace frame, and press letter `v' for navigating to the frame source.
Use C-x C-e to evaluate expressions in the source buffer using the backtrace frame as context.

# SLDB-SHOW-FRAME-LOCAL

This is a prototype.

Show backtrace frames locals when the ursor over source code.

![sldb-show-frame-local](sldb-show-frame-local.gif)

## Setup

```lisp
(require 'sldb-show-frame-local)
(sldb-show-frame-local-on-cursor-move)
```
