(provide change
         change?
         change-kind
         change-old
         change-new)

;; A single pending filesystem operation, produced by diffing a parsed oil
;; document against *directory-cache*.
;;
;;   kind - 'create | 'delete | 'move | 'copy
;;   old  - cached entry the change acts on, or #false for 'create
;;   new  - buffer entry describing the destination, or #false for 'delete
;;
;; A rename is just a 'move whose old and new share a parent.
(struct change (kind old new) #:transparent)
