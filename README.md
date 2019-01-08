# call-graph - Library to generate call graph for c/c++ functions

[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](COPYING.md)
[![MELPA](https://melpa.org/packages/call-graph-badge.svg)](https://melpa.org/#/call-graph)
[![MELPA Stable](https://stable.melpa.org/packages/call-graph-badge.svg)](https://stable.melpa.org/#/call-graph)

Generate call graph for c/c++ functions.

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

Place your cursor in the c/c++ function which you want to generate
call-graph for and execute call-graph.
You could bind it to <kbd>C-c g</kbd>.

```lisp
    (global-set-key (kbd "C-c g") 'call-graph)
```

# Keys

```lisp
    (define-key map (kbd "e") 'cg/widget-expand-all)
    (define-key map (kbd "c") 'cg/widget-collapse-all)
    (define-key map (kbd "p") 'widget-backward)
    (define-key map (kbd "n") 'widget-forward)
    (define-key map (kbd "q") 'cg/quit)
    (define-key map (kbd "+") 'cg/expand)
    (define-key map (kbd "_") 'cg/collapse)
    (define-key map (kbd "o") 'cg/goto-file-at-point)
    (define-key map (kbd "g") 'cg/at-point)
    (define-key map (kbd "d") 'cg/remove-caller)
    (define-key map (kbd "l") 'cg/select-caller-location)
    (define-key map (kbd "r") 'cg/reset-caller-cache)
    (define-key map (kbd "t") 'cg/toggle-func-args)
    (define-key map (kbd "<RET>") 'cg/goto-file-at-point)
```

# Customization

Customize the location of the GNU GLOBAL binary.

```
    (setq cg-path-to-global "/home/huming/private/gtags-6.5.7/bin/")
```

Specify the parse depth of the call-graph.
default is 2, the more the depth is, the longer it takes.

```
    (setq cg-initial-max-depth 3)
```

Avoid truncating Imenu entries.

```
    (setq imenu-max-item-length "Unlimited")
```

Exclude UT/CT directories like /Dummy_SUITE/ /Dummy_Test/.

```
    (dolist (filter '("grep -v \"Test/\""
                    "grep -v \"_SUITE/\""
                    "grep -v \"/test-src/\""
                    "grep -v \"/TestPkg/\""))
    (add-to-list 'cg-search-filters filter))
```

Save caller-relations in .session.

```
    (add-hook 'kill-emacs-hook #'cg/prepare-persistent-data)

    (setq desktop-globals-to-save
        (append '((cg-persist-caller-cache . 1000)
                  tags-file-name
                  tags-table-list)))
```

# Screenshots

![call-graph-demo-1.gif](img/call-graph-demo-1.gif)
![call-graph-demo-2.gif](img/call-graph-demo-2.gif)

# Limitations

Currently when parsing calling relations, header files is excluded.
Lots more need to be improved.

# Features

- [x] Navigate to the caller file location.
- [x] Support cache when searching for callers.
- [x] Incrementally generate sub caller.
- [x] Support manually remove wrong callers.
- [x] Support persistence of call-graph cache data.
- [x] Toggle show function args in call-graph.

# Contributing
Yes, please do! See [CONTRIBUTING](CONTRIBUTING.md) for guidelines.

# License

See [LICENSE](LICENSE). Copyright (c) 2018 Huming Chen <chenhuming@gmail.com>
