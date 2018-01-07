;;----------------------------------------------------------------------------
;; Utility functions
;;----------------------------------------------------------------------------

;;; simplicistic queue implementation
;;; thanks to wasamasa, @see http://ix.io/DoE
(defun list-to-queue (list)
  (cons list (if (consp list) (last list) '())))

(defun queue-to-list (queue)
  (car queue))

(defun make-queue ()
  (cons '() '()))

(defun queue-empty-p (queue)
  (null (car queue)))

(defun queue-get (queue)
  (if (null (car queue))
      (error "Queue is empty")
      (let ((x (caar queue)))
        (setcar queue (cdar queue))
        (if (null (car queue)) (setcdr queue '()))
        x)))

(defun queue-put (queue x)
  (let ((entry (cons x '())))
    (if (null (car queue))
        (setcar queue entry)
        (setcdr (cdr queue) entry))
    (setcdr queue entry)
    x))

(provide 'cg-utils)
