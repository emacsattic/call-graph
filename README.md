# call-graph - Library to generate call graph for cpp functions

Generate call graph for cpp functions.

# Where does this library come from?

How many times have you had this feeling that
Why can't we have this in emacs when you see
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

## Preparations

Since currenly the implementation is based on GNU Global.
It's just recursively use global to find the caller of
current function and eventually build up this call-graph tree.

```
GNU Global is required
```

# Usage

Place your cursor in the cpp function which you want to generate
a call-graph for, and execute call-graph.
By default, this funciton is binded to "C-c g".

```lisp
    (global-set-key (kbd "C-c g") 'call-graph)
```

# Keys

```lisp
    (define-key map (kbd "e") 'call-graph-widget-expand-all)
    (define-key map (kbd "c") 'call-graph-widget-collapse-all)
    (define-key map (kbd "TAB") 'widget-forward)
    (define-key map (kbd "<backtab>") 'widget-backward)
```

# Customization

Specify the parse depth of the call-graph.
default is 2, the more depth is, the longer it takes.

```
    (setq call-graph-max-depth 3)
```

# Screenshots

![call-graph-demo-0.gif](https://github.com/beacoder/call-graph/blob/master/img/call-graph-demo-0.gif)
![call-graph-demo-1.gif](https://github.com/beacoder/call-graph/blob/master/img/call-graph-demo-1.gif)
![call-graph-demo-2.gif](https://github.com/beacoder/call-graph/blob/master/img/call-graph-demo-2.gif)

# Limitations

Currently when parsing the calling relations, header files is excluded.
Lots more need to be improved.

# Todo

- [ ] Navigate to the caller file location.
- [ ] Incrementally generate sub caller.
- [ ] Support mark sub caller tree as negtive match.
- [ ] Support persistence of call-graph cache data.

# License

MIT

# Copyright

Huming Chen <chenhuming@gmail.com>
