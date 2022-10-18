# move-mode

`move-mode` is an Emacs major-mode for editing smart contracts written
in the [Move](https://github.com/move-language/move) programming
language.  Supports Emacs 25.1 and above (tested on Emacs for Mac OS X
25.1-1, Emacs Mac Port 28.1).

There are multiple flavors of Move (Core Move, Sui Move, etc).  This
mode aims to be agnostic to the flavor of Move you are writing in by
offering [customizations](#Customization) to tweak the experience.

This mode does **not** provide auto-complete, goto definition, or
other language-server-based features out of the box, but check the
[LSP](#LSP) section for details on how to enable them using Move
Analyzer.

## Known Issues

- Filling a paragraph in an unterminated multi-line comment (starting
  with `/*` but with no matching `*/`), or inserting a newline to
  continue the comment causes Emacs to use `/*` as the fill prefix
  rather than the intended `*`.  This can be fixed by adding the
  closing `*/`.

## Installation

### Recommended: `straight.el` and `use-package`

The recommended approach is to install this package using
[`use-package`](https://github.com/jwiegley/use-package) and
[`straight.el`](https://github.com/radian-software/straight.el) by
including the following to your `init.el`:

``` emacs-lisp
(use-package move-mode
  :straight (:host github :repo "amnn/move-mode" :branch "main"))
```

### Just `straight.el`:

You can also use `straight.el` directly:

```emacs-lisp
(straight-use-package
  '(move-mode :type git :host github :repo "amnn/move-mode"))
```

### [MELPA](https://github.com/melpa/melpa)

`move-mode` is not available on MELPA yet, watch this space.

### (Not Recommended) Manual Install

Or install the package completely manually, by cloning the repo and
adding the following:

``` emacs-lisp
(add-to-list 'load-path "/path/to/move-move/repo")
(autoload 'move-mode "move-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.move\\'" . move-mode))
```


## Features

- `move-mode` recognises Move syntax such as keywords, scopes, variable
  declarations, literals, imports, and module, function and struct
  definitions.  It exposes these so font lock mode can apply syntax
  highlighting to them.
- Emacs' built-in indentation (usually bound to <kbd>TAB</kbd>) will
  bring the line/cursor to the correct column based on the level of
  nesting, and whether you are continuing an earlier expression or
  not.
- The keymap includes keybindings to interact with the current
  package, using the Move CLI, where the current package is defined as
  the parent directory that contains the `Move.toml`.  It adds the
  following bindings by default:
  - <kbd>C-c</kbd><kbd>C-c</kbd><kbd>C-b</kbd>: `move build` the
    current package.
  - <kbd>C-c</kbd><kbd>C-c</kbd><kbd>C-d</kbd>: `move disassemble` a
    module (requested interactively) in the current package.
  - <kbd>C-c</kbd><kbd>C-c</kbd><kbd>C-p</kbd>: `move prove` the
    current package.
  - <kbd>C-c</kbd><kbd>C-c</kbd><kbd>C-t</kbd>: `move test` the
    current package.

## Customization

- `move-indent-offset` (default: `4`) controls the gap between tab-stops.
- `move-bin` (default: `"move"`) controls the binary to send Move CLI
  commands to.
- `move-builtins` (default: `core-move-builtin-functions`) The
  keywords that are highlighted as builtin functions.  Defaults to the
  list that are recognised in Core Move, which is packaged as a
  constant with the mode.
  - NOTE: Prover-specific keywords are not highlighted by default, but
    are included with the mode as `move-prover-keywords`.  If you
    would like them to be highlighted, consider:

``` emacs-lisp
(customize-set-variable 'move-builtins
  (concat core-move-builtin-functions
          move-prover-keywords))

```

- `move-default-arguments` (default `""`) any extra commands to pass
  to Move CLI commands -- these are added to the end.

## LSP

`move-mode` does not configure an LSP server for Move by default, but
it is possible to integrate Move Analyzer with Emacs.

The analyzer is installed directly from its repo, via `cargo`:

``` shell
$ cargo install --git https://github.com/move-language/move move-analyzer
```

It can be modified using `--feature` flags passed to the `install`
command above. The two most common ones are:
- `--features "address20"` option for Move flavors requiring 20-byte long addresses (e.g., Sui Move)
- `--features "address32"` option for Move flavors requiring 32-byte long addresses (e.g., Aptos Move)

A full list of supported features can be found in the
`[features]` section of
[its Cargo.toml](https://github.com/move-language/move/blob/main/language/move-analyzer/Cargo.toml).

### [Eglot](https://github.com/joaotavora/eglot)

Once `move-analyzer` is installed, integrating it into Eglot is as
simple as adding the following configuration to your `init.el`:

``` emacs-lisp
(add-hook 'move-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs '(move-mode "move-analyzer"))
```

Eglot uses `project.el` (Emacs' built-in project management) to find
the root of your project.  It defaults to looking for the version
control repository root.  If you work with repositories where your
Move packages are nested in sub-directories, add the following to
allow Eglot to find projects by looking for `Move.toml`:

``` emacs-lisp
(defun my/move-lsp-project-root (dir)
  (and-let* (((boundp 'eglot-lsp-context))
             (eglot-lsp-context)
             (override (locate-dominating-file dir "Move.toml")))
    (cons 'Move.toml override)))

(add-hook 'project-find-functions #'my/move-lsp-project-root)
(cl-defmethod project-root ((project (head Move.toml)))
  (cdr project)))
```

NOTE: This will not affect finding project files outside of the LSP.

## Contributing

Contributions are very welcome! If you notice a bug, try updating
`move-mode`, and if it is still there please share a report as an
[issue](https://github.com/amnn/move-mode/issues), with:

- Details on your configuration (operating system, version and variant
  of Emacs, minimal Emacs `init.el`, version -- i.e. git revision --
  of `move-mode`).
- The sequence of actions you took.
- The expected outcome.
- The actual outcome, with screenshots if relevant.
  
If you are interested in working on features please take a look at current 
[open issues](https://github.com/amnn/move-mode/issues) for inspiration!
