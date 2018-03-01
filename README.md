# call-graph - Library to generate call graph for cpp functions

[![MELPA](https://melpa.org/packages/call-graph-badge.svg)](https://melpa.org/#/call-graph)

Generate call graph for cpp functions.

# Where does this library come from?

How many times have you had this feeling that
why can't we have this in emacs when you see
the fancy function call hierarchy in "modern" IDEs?
I hope one day, with this library, we won't have
to envy those "modern" IDEs for this again.

# Installation

Clone the repo, then in your Emacs init file:

```lisp
(add-to-list 'load-path "/path/to/repo")
(require 'call-graph)
(call-graph) ;; to launch it
```

Or install from [melpa](https://melpa.org/#/call-graph).

## Dependencies

* `GNU Global`
* `hierarchy`
* `tree-mode`
* `ivy`

Basicly `call-graph` just recursively call `Global` to find caller of
current function and eventually build up a `call-graph` tree.

# Usage

Place your cursor in the cpp function which you want to generate
call-graph for and execute call-graph.
You could bind it to <kbd>C-c g</kbd>.

```lisp
    (global-set-key (kbd "C-c g") 'call-graph)
```

# Keys

```lisp
    (define-key map (kbd "e") 'call-graph-widget-expand-all)
    (define-key map (kbd "c") 'call-graph-widget-collapse-all)
    (define-key map (kbd "p") 'widget-backward)
    (define-key map (kbd "n") 'widget-forward)
    (define-key map (kbd "q") 'call-graph-quit)
    (define-key map (kbd "+") 'call-graph-expand)
    (define-key map (kbd "_") 'call-graph-collapse)
    (define-key map (kbd "o") 'call-graph-goto-file-at-point)
    (define-key map (kbd "g") 'call-graph-at-point)
    (define-key map (kbd "d") 'call-graph-remove-caller)
    (define-key map (kbd "l") 'call-graph-select-caller-location)
    (define-key map (kbd "<RET>") 'call-graph-goto-file-at-point)
```

# Customization

Customize the location of the GNU GLOBAL binary.

```
    (setq call-graph-path-to-global "/home/huming/private/gtags-6.5.7/bin/")
```

Specify the parse depth of the call-graph.
default is 2, the more the depth is, the longer it takes.

```
    (setq call-graph-initial-max-depth 3)
```

Exclude UT/CT directories like /Dummy_SUITE/ /Dummy_Test/.

```
    (dolist (filter '("grep -v \"Test/\""
                    "grep -v \"_SUITE/\""
                    "grep -v \"/test-src/\""
                    "grep -v \"/TestPkg/\""))
    (add-to-list 'call-graph-filters filter))
```

# Screenshots

![call-graph-demo-1.gif](img/call-graph-demo-1.gif)
![call-graph-demo-2.gif](img/call-graph-demo-2.gif)

# Limitations

Currently when parsing calling relations, header files is excluded.
Lots more need to be improved.

# Features

- [x] Navigate to the caller file location.
- [x] Support filter when searching for callers.
- [x] Incrementally generate sub caller.
- [x] Support manually remove wrong callers.
- [ ] Support persistence of call-graph cache data.
- [ ] Add async support for call-graph generation.

# Contributing
Yes, please do! See [CONTRIBUTING](CONTRIBUTING.md) for guidelines.

# License

See [LICENSE](LICENSE). Copyright (c) 2018 Huming Chen <chenhuming@gmail.com>
