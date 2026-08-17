(provide entry
         entry?
         entry-id
         entry-parent
         entry-name
         entry-type
         entry-metadata
         set-entry-parent!
         set-entry-name!
         set-entry-type!
         set-entry-metadata!)

;; A single filesystem entry tracked by oil.
;;
;;   id       - stable integer identity, rendered as the /NNN prefix in the
;;              buffer. Never changes for the lifetime of the entry, which is
;;              what lets a save distinguish a rename from a delete + create.
;;   parent   - url of the directory currently containing this entry
;;   name     - filename within `parent`
;;   type     - 'file | 'directory | 'link
;;   metadata - #false, or a hash of adapter-provided extras (size, mtime, ...)
;;
;; `id` deliberately has no exported setter.
(struct entry (id parent name type metadata) #:mutable #:transparent)
