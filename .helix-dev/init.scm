(require "helix/editor.scm")
(require "helix/keymaps.scm")

(require "../olive.scm")

(keymap (global)
        (normal ("-" olive-open)))
