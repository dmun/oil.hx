(require "macros.scm")

(provide entry
         entry?
         entry-id
         entry-parent
         entry-name
         entry-type
         entry-metadata
         directory?)

;; A single filesystem entry tracked by olive.
;;
;;   id       - stable integer identity, rendered as the /NNN prefix in the
;;              buffer. Never changes for the lifetime of the entry, which is
;;              what lets a save distinguish a rename from a delete + create.
;;   parent   - url of the directory currently containing this entry
;;   name     - filename within `parent`
;;   type     - 'file | 'directory | 'link
;;   metadata - #f, or a hash of adapter-provided extras (size, mtime, ...)
;;
;; Entries are immutable so a cache value is a real snapshot of the original
;; filesystem state. Cache updates replace entries instead of changing values
;; that may also be referenced by a pending action.
(struct entry (id parent name type metadata) #:transparent)

(fun directory? :: (e entry? -> boolean?)
     (equal? (entry-type e) 'directory))
