# call-graph - Library to generate call graph for cpp functions

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
;; install hierarchy and tree-mode first
(add-to-list 'load-path "/path/to/repo")
(require 'call-graph)
(call-graph) ;; to launch it
```

## Dependencies

* `GNU Global`

Basicly `call-graph` just recursively use `Global` to find the caller of
current function and eventually build up this `call-graph` tree.

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
    (define-key map (kbd "d") 'call-graph-display-file-at-point)
    (define-key map (kbd "o") 'call-graph-goto-file-at-point)
    (define-key map (kbd "<RET>") 'call-graph-goto-file-at-point)
```

# Customization

Specify the parse depth of the call-graph.
default is 2, the more the depth is, the longer it takes.

```
    (setq call-graph-initial-max-depth 3)
```

Exclude UT/CT directories like /Dummy_SUITE/ /Dummy_Test/.

```
    (setq call-graph-filters '("grep -v \"Test/\"" "grep -v \"_SUITE/\""))
```

# Screenshots

![call-graph-demo-1.gif](https://github.com/beacoder/call-graph/blob/master/img/call-graph-demo-1.gif)
![call-graph-demo-2.gif](https://github.com/beacoder/call-graph/blob/master/img/call-graph-demo-2.gif)

# Limitations

Currently when parsing calling relations, header files is excluded.
Lots more need to be improved.

# Features

- [x] Navigate to the caller file location.
- [x] Use call-graph cache data to improve performance.
- [x] Support filter when searching for callers.
- [ ] Incrementally generate sub caller.
- [ ] Support mark sub caller tree as negtive match.
- [ ] Add font to distinguish between active/non-active caller.
- [ ] Support persistence of call-graph cache data.
- [ ] Add async support for call-graph generation.

# License

MIT

# Copyright

Huming Chen <chenhuming@gmail.com>
