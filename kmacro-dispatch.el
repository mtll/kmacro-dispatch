;;; Kmacro Dispatch -*- lexical-binding: t -*-
;;
;; Filename: kmacro-dispatch.el
;; Description: Dispatch keyboard macros multiple on regions
;; Author: David Feller
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (compat "29.1.4.4"))
;; Homepage: https://github.com/mtll/kmacro-dispatch
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Dispatch keyboard macros on multiple on regions
;;
;;; Code:

(require 'compat)
(require 'transient)
(require 'isearch)
(require 'kmacro)
(eval-when-compile
  (require 'subr-x))

;;;; Variables

(defgroup kmacro-dispatch nil
  "Dispatch keyboard macros on regions."
  :prefix "kdispatch-"
  :group 'editing)

(defface kdispatch-pulse-face
  '((default (:inherit pulse-highlight-face)))
  "Face for dispatch pulse."
  :group 'kmacro-dispatch)

(defvar kdispatch-macro-p nil
  "Non-nil during macro dispatch.")

(defvar kdispatch-error nil
  "If non-nil contains the error encountered during macro dispatch.")

(defvar kdispatch-macro-end-hook nil
  "Hook run after macro dispatch has completed.")

(defvar kdispatch-macro-start-hook nil
  "Hook run before macro dispatch begins.")

(defvar kdispatch-macro-iterator-hook nil
  "Hook run during each iteration of macro dispatch.
If any function returns a nil value then dispatch it halted.")

;;;; Utils

(defmacro kdispatch--thread (needle form &rest forms)
  (declare (indent 2))
  (if forms
      `(let ((,needle ,form))
         (kdispatch--thread ,needle ,@forms))
    form))

(defun kdispatch--create-marker (pos &optional buffer)
  "Create marker at POS in BUFFER."
  (let ((marker (make-marker)))
    (set-marker marker pos buffer)
    marker))

(defun kdispatch--isearch-matches-in-buffer (&optional buffer restrict)
  (with-current-buffer (or buffer (current-buffer))
    (let (bound)
      (save-excursion
        (pcase restrict
          ('after
           (unless isearch-forward
             (isearch-repeat 'forward))
           (goto-char isearch-other-end))
          ('before
           (when isearch-forward
             (isearch-repeat 'backward))
           (goto-char isearch-other-end))
          (_
           (goto-char (if isearch-forward (point-min) (point-max)))))
        (setq bound (if isearch-forward (point-max) (point-min)))
        (cl-loop for match = (isearch-search-string isearch-string bound t)
                 while match
                 when (funcall isearch-filter-predicate
                               (match-beginning 0) (match-end 0))
                 collect (cons (kdispatch--create-marker (match-beginning 0))
                               (kdispatch--create-marker (match-end 0))))))))

;;;; Core

(defun kdispatch--ensure-region-buffer (region)
  (when-let ((buffer (and region (marker-buffer (car region)))))
    (when (not (eq buffer (current-buffer)))
      (pop-to-buffer buffer)
      (deactivate-mark t)
      (unless (eq buffer (window-buffer (selected-window)))
        (error "Could not pop to buffer %s" buffer))))
  region)

(defun kdispatch--region-iterator (regions &optional reverse)
  (when reverse (setq regions (reverse regions)))
  (dolist (reg regions)
    (unless (markerp (car reg))
      (setcar reg (kdispatch--create-marker (car reg))))
    (unless (markerp (cdr reg))
      (setcdr reg (kdispatch--create-marker (cdr reg)))))
  (lambda (state)
    (pcase state
      (:finalize
       (pcase-dolist (`(,beg . ,end) regions)
         (set-marker beg nil)
         (set-marker end nil)))
      (_
       (kdispatch--ensure-region-buffer (pop regions))))))

(defun kdispatch--point-iterator (points &optional reverse)
  (setq points (mapcar (lambda (pt)
                         (if (markerp pt)
                             pt
                           (kdispatch--create-marker pt)))
                       (if reverse (nreverse points) points)))
  (lambda (state)
    (pcase state
      (:finalize
       (dolist (pt points)
         (set-marker pt nil)))
      (_
       (when-let ((pt (pop points)))
         (kdispatch--ensure-region-buffer (cons pt pt)))))))

(defun kdispatch--merge-undo (iterator)
  (let (kdispatch-undo-handles)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (pcase-dolist (`(,_ . ,handle) kdispatch-undo-handles)
           (if kdispatch-error
               (cancel-change-group handle)
             (accept-change-group handle)
             (undo-amalgamate-change-group handle))))
        (_
         (prog1
             (funcall iterator state)
           (unless (alist-get (current-buffer) kdispatch-undo-handles)
             (activate-change-group
              (setf (alist-get (current-buffer) kdispatch-undo-handles)
                    (prepare-change-group))))))))))

(defun kdispatch--save-excursion (iterator)
  (let (kdispatch-saved-excursions)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (pcase-dolist (`(,buffer ,pt . ,saved) kdispatch-saved-excursions)
           (with-current-buffer buffer
             (goto-char pt)
             (set-marker pt nil)
             (save-mark-and-excursion--restore saved))))
        (_
         (prog1 (funcall iterator state)
           (unless (alist-get (current-buffer) kdispatch-saved-excursions)
             (setf (alist-get (current-buffer) kdispatch-saved-excursions)
                   (cons (point-marker) (save-mark-and-excursion--save))))))))))

(defun kdispatch--save-restriction (iterator)
  (let (kdispatch-saved-restrictions)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (pcase-dolist (`(,buffer ,beg . ,end) kdispatch-saved-restrictions)
           (with-current-buffer buffer
             (widen)
             (narrow-to-region beg end))))
        (_
         (prog1
             (funcall iterator state)
           (if-let ((restriction (alist-get (current-buffer) kdispatch-saved-restrictions)))
               (progn
                 (widen)
                 (narrow-to-region (car restriction) (cdr restriction)))
             (setf (alist-get (current-buffer) kdispatch-saved-restrictions)
                   (cons (point-min-marker)
                         (point-max-marker))))))))))

(defun kdispatch--change-region (iterator)
  (lambda (state)
    (let ((ret (funcall iterator state)))
      (when (and (not (eq state :finalize))
                 (consp ret))
        (delete-region (car ret) (cdr ret)))
      ret)))

(defun kdispatch--at-end (iterator)
  (lambda (state)
    (pcase (funcall iterator state)
      (`(,beg . ,end) (cons end beg))
      (ret ret))))

(defun kdispatch--skip-empty (iterator)
  (lambda (state)
    (let ((ret (funcall iterator state)))
      (unless (eq state :finalize)
        (while (and ret (= (car ret) (cdr ret)))
          (setq ret (funcall iterator state))))
      ret)))

(defun kdispatch--pulse-on-record (iterator)
  (lambda (state)
    (pcase (funcall iterator state)
      ((and `(,beg . ,end)
            (guard (eq state :record))
            ret)
       (pulse-momentary-highlight-region beg end 'kdispatch-pulse-face)
       ret)
      (ret ret))))

(defun kdispatch--pulse-line-on-record (iterator)
  (lambda (state)
    (pcase (funcall iterator state)
      ((and `(,beg . ,_)
            (guard (eq state :record))
            ret)
       (pulse-momentary-highlight-one-line beg 'kdispatch-pulse-face)
       ret)
      (ret ret))))

(defun kdispatch--save-windows (iterator)
  (let (wconf)
    (lambda (state)
      (pcase state
        (:finalize
         (funcall iterator state)
         (set-window-configuration wconf))
        (_
         (unless wconf
           (setq wconf (current-window-configuration)))
         (funcall iterator state))))))

(defmacro kdispatch--define-dispatcher (name arglist &rest body)
  "Define a macro dispatcher.
The iterator must be the first argument in ARGLIST.

\(fn NAME ARGLIST [DOCSTRING] BODY...)"
  (declare (doc-string 3) (indent 2))
  (let ((iterator (car arglist))
        (sym (make-symbol "loop-function-symbol"))
        (docstring (if (stringp (car body)) (pop body) "")))
    `(defun ,name ,arglist
       ,docstring
       (let* ((undo-outer-limit nil)
              (undo-limit most-positive-fixnum)
              (undo-strong-limit most-positive-fixnum)
              (kdispatch-macro-p t)
              (kdispatch-error nil)
              (,sym (make-symbol "kmacro-loop-function"))
              (,iterator (lambda (&optional state)
                           (pcase (funcall ,iterator (or state :loop))
                             (`(,beg . ,end)
                              (goto-char beg)
                              (push-mark end t)
                              (set-marker beg nil)
                              (when (markerp end) (set-marker end nil))
                              (and (run-hook-with-args-until-failure
                                    'kdispatch-macro-iterator-hook)
                                   t))))))
         (fset ,sym ,iterator)
         (advice-add 'kmacro-loop-setup-function :before-while ,sym)
         (unwind-protect
             (condition-case err
                 (progn
                   (run-hooks 'kdispatch-macro-start-hook)
                   ,@body)
               (t
                (setq kdispatch-error err)
                (signal (car err) (cdr err))))
           (advice-remove 'kmacro-loop-setup-function ,sym)
           (funcall ,iterator :finalize)
           (run-hook-wrapped 'kdispatch-macro-end-hook
                             (lambda (hook)
                               (ignore-errors (funcall hook)))))))))

(kdispatch--define-dispatcher kdispatch--macro (iterator &optional macro)
  (pcase-exhaustive macro
    ((pred kmacro-p)
     (funcall macro 0))
    ((or (pred stringp) (pred vectorp))
     (kmacro-call-macro 0 nil nil macro))
    ('nil
     (when (funcall iterator :record)
       (kmacro-start-macro nil)
       (unwind-protect
           (recursive-edit)
         (if (not defining-kbd-macro)
             (user-error "Not defining keyboard macro")
           (kmacro-end-macro 0)))))))

(kdispatch--define-dispatcher kdispatch--macro-append (iterator &optional skip-exec)
  (when (funcall iterator :record)
    (kmacro-start-macro (if skip-exec '(16) '(4)))
    (unwind-protect
        (recursive-edit)
      (when (not defining-kbd-macro)
        (user-error "Not defining keyboard macro"))
      (kmacro-end-macro 0))))

(kdispatch--define-dispatcher kdispatch--macro-step-edit (iterator)
  (when (funcall iterator :record)
    (let ((sym (make-symbol "kbd-terminate-hook"))
          apply)
      (fset sym (lambda () (setq apply kmacro-step-edit-replace)))
      (add-hook 'kbd-macro-termination-hook sym)
      (unwind-protect
          (kmacro-step-edit-macro)
        (remove-hook 'kbd-macro-termination-hook sym))
      (unless apply
        (user-error "Keyboard macro edit aborted")))
    (kmacro-call-macro 0)))

;;;; Transients

(defclass kdispatch-transient-switches (transient-switches)
  ((required :initarg :required :initform nil))
  "Class used for sets of mutually exclusive command-line switches.
Does not allow a null value.")

(cl-defmethod transient-infix-read ((obj kdispatch-transient-switches))
  "Cycle through the mutually exclusive switches.
The last value is \"don't use any of these switches\"."
  (let ((choices (mapcar (apply-partially #'format (oref obj argument-format))
                         (oref obj choices))))
    (if-let ((value (oref obj value)))
        (or (cadr (member value choices))
            (when (oref obj required) (car choices)))
      (car choices))))

(cl-defmethod transient-format-value ((obj kdispatch-transient-switches))
  (with-slots (value argument-format choices) obj
    (format
     (propertize "%s" 'face 'transient-delimiter)
     (mapconcat
      (lambda (choice)
        (propertize choice 'face
                    (if (equal (format argument-format choice) value)
                        'transient-argument
                      'transient-inactive-value)))
      choices
      (propertize "|" 'face 'transient-delimiter)))))

(defun kdispatch-recursive-edit-kmacro (arg)
  "Edit last keyboard macro inside a recursive edit.
Press \\[exit-recursive-edit] to exit the recursive edit and abort
the edit in the macro."
  (interactive "P")
  (save-mark-and-excursion
    (save-window-excursion
      (kmacro-edit-macro (not arg))
      (when-let ((buffer (get-buffer "*Edit Macro*")))
        (save-excursion
          (goto-char (point-min))
          (when (re-search-forward "finish; press \\(.*\\) to cancel" (line-end-position) t)
            (goto-char (match-beginning 1))
            (delete-region (match-beginning 1) (match-end 1))
            (insert (substitute-command-keys "\\[exit-recursive-edit]"))))
        (delete-other-windows)
        (advice-add 'edmacro-finish-edit :after 'exit-recursive-edit)
        (unwind-protect
            (recursive-edit)
          (advice-remove 'edmacro-finish-edit 'exit-recursive-edit)
          (kill-buffer buffer))))))

(defun kdispatch-recursive-edit-lossage ()
  "Edit lossage macro inside a recursive edit.
Press \\[exit-recursive-edit] to exit the recursive edit and abort
the edit in the macro."
  (interactive)
  (save-mark-and-excursion
    (save-window-excursion
      (kmacro-edit-lossage)
      (when-let ((buffer (get-buffer "*Edit Macro*")))
        (when (re-search-forward "finish; press \\(.*\\) to cancel" (line-end-position) t)
          (goto-char (match-beginning 1))
          (delete-region (match-beginning 1) (match-end 1))
          (insert (substitute-command-keys "\\[exit-recursive-edit]")))
        (delete-other-windows)
        (advice-add 'edmacro-finish-edit :after 'exit-recursive-edit)
        (unwind-protect
            (recursive-edit)
          (advice-remove 'edmacro-finish-edit 'exit-recursive-edit)
          (kill-buffer buffer))))))

(defun kdispatch--kmacro-display (macro &optional trunc)
  (pcase macro
    ((or 'nil '[] "") "nil")
    (_ (let* ((m (format-kbd-macro macro))
              (l (length m))
              (z (and trunc (> l trunc))))
         (format "%s%s"
                 (if z (substring m 0 (1- trunc)) m)
                 (if z "..." ""))))))

(defun kdispatch--kmacro-ring-display ()
  (with-temp-message ""
    (concat
     (propertize "Kmacro Ring: " 'face 'transient-heading)
     (propertize (format "%s" (or (if defining-kbd-macro
                                      kmacro-counter
                                    kmacro-initial-counter-value)
                                  (format "[%s]" kmacro-counter)))
                 'face 'transient-value)
     " - "
     (when (length> kmacro-ring 1)
       (kdispatch--thread -it-
           (car (last kmacro-ring))
         (kmacro--keys -it-)
         (kdispatch--kmacro-display -it- 15)
         (concat -it- ", ")))
     (propertize (kdispatch--kmacro-display last-kbd-macro 15)
                 'face 'transient-value)
     (if (kmacro-ring-empty-p)
         ""
       (kdispatch--thread -it-
           (car kmacro-ring)
         (kmacro--keys -it-)
         (kdispatch--kmacro-display -it- 15)
         (concat ", " -it-))))))

(defun kdispatch--kmacro-counter-display ()
  (with-temp-message ""
    (concat
     (propertize "Kmacro Counter: " 'face 'transient-heading)
     (propertize (format "%s" (or (if defining-kbd-macro
                                      kmacro-counter
                                    kmacro-initial-counter-value)
                                  (format "[%s]" kmacro-counter)))
                 'face 'transient-value))))

(defun kdispatch--in-kbd-macro-p ()
  (or defining-kbd-macro executing-kbd-macro))

(defun kdispatch--kmacro-ring-empty-p ()
  ;; Avoid the messages kmacro-ring-empty-p dispays
  (while (and (null last-kbd-macro) kmacro-ring)
    (kmacro-pop-ring1))
  (null last-kbd-macro))

(transient-define-infix kdispatch--set-counter-format-infix ()
  "Set `kmacro-counter-format'."
  :class 'transient-lisp-variable
  :set-value (lambda (_ format) (kmacro-set-format format))
  :variable 'kmacro-counter-format
  :reader (lambda (&rest _)
            (read-string "Macro Counter Format: ")))

(transient-define-prefix kdispatch-kmacro-prefix ()
  "Transient menu for kmacro functions."
  [:description
   kdispatch--kmacro-ring-display
   :if-not kdispatch--in-kbd-macro-p
   [("i" "Insert Counter" kmacro-insert-counter)
    ("s" "Set Counter" kmacro-set-counter :transient t)
    ("+" "Add to Counter" kmacro-add-counter :transient t)
    ("f" "Set Format" kdispatch--set-counter-format-infix :transient t)]
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("w" "Swap" kmacro-swap-ring :transient t)
    ("o" "Pop" kmacro-delete-ring-head :transient t)]]
  ["Commands:"
   :if-not kdispatch--in-kbd-macro-p
   [("k" "Call Macro" kmacro-call-macro)
    ("a" "Append to Macro" (lambda ()
                             (interactive)
                             (kmacro-start-macro '(4))))
    ("A" "Append w/o Executing" (lambda ()
                                  (interactive)
                                  (kmacro-start-macro '(16))))
    ("r" "Record Macro" kmacro-start-macro)
    ("d" "Name Last Macro" kmacro-name-last-macro)]
   [("e" "Edit Macro" kmacro-edit-macro)
    ("E" "Edit Lossage" kmacro-edit-lossage)
    ("m" "Kmacro to Register" kmacro-to-register)
    ("c" "Apply Macro on Lines" apply-macro-to-region-lines)
    ("q" "Step Edit Macro" kmacro-step-edit-macro)]]
  [:if
   kdispatch--in-kbd-macro-p
   ["Commands:"
    ("q" "Query" kbd-macro-query)
    ("d" "Redisplay" kmacro-redisplay)]
   [:description
    kdispatch--kmacro-counter-display
    ("i" "Insert Counter" kmacro-insert-counter)
    ("s" "Set Counter" kmacro-set-counter :transient t)
    ("+" "Add to Counter" kmacro-add-counter :transient t)
    ("f" "Set Format" kdispatch--set-counter-format-infix)]])

(defun kdispatch--set-macro-ring-head (macro)
  (interactive
   (list (get-register (register-read-with-preview "Kmacro: "))))
  (unless (or (null macro)
              (stringp macro)
              (vectorp macro)
              (kmacro-p macro))
    (user-error "Invalid keyboard macro"))
  (kmacro-push-ring macro)
  (kmacro-swap-ring))

(transient-define-argument kdispatch--macro-infix ()
  "Dispatch `last-kbd-macro'.
APPLY simply executes the macro at each region.  APPEND executes
the macro and records additional keys on the first iteration.
STEP-EDIT uses `kmacro-step-edit-macro' to edit the macro before
dispatch."
  :class 'kdispatch-transient-switches
  :description "Last Kmacro"
  :key "k"
  :argument "last-kmacro="
  :argument-format "last-kmacro=%s"
  :argument-regexp "\\(last-kmacro=\\(apply\\|append\\|step-edit\\)\\)"
  :choices '("apply" "step-edit" "append"))

(transient-define-argument kdispatch--matches-infix ()
  "Restrict dispatch to only some isearch matches.
AFTER means only those matchs after, and including, the current match.
BEFORE means only those matches before, and including, the current match."
  :class 'kdispatch-transient-switches
  :description "Restrict Matches Inclusive"
  :if-not (lambda () (bound-and-true-p multi-isearch-buffer-list))
  :key "j"
  :argument "matches="
  :argument-format "matches=%s"
  :argument-regexp "\\(matches=\\(after\\|before\\)\\)"
  :choices '("after" "before"))

(transient-define-argument kdispatch--region-infix ()
  "How to dispatch on each region.
START means place the point at the start of the region before
each iteration.  END means place the point at the end of the
region before each iteration.  CHANGE means delete the region
before each iteration."
  :class 'kdispatch-transient-switches
  :required t
  :key "w"
  :description "Regions"
  :argument "region="
  :argument-format "region=%s"
  :argument-regexp "\\(region=\\(start\\|change\\|end\\)\\)"
  :choices '("start" "end" "change")
  :init-value (lambda (obj) (oset obj value "region=start")))

(transient-define-argument kdispatch--order-infix ()
  "Dispatch on regions from last to first."
  :class 'transient-switch
  :key "o"
  :description "Order"
  :argument "reverse")

(transient-define-argument kdispatch--empty-infix ()
  "Include empty regions in dispatch."
  :class 'transient-switch
  :key "u"
  :description "Include Empty"
  :argument "empty")

(transient-define-argument kdispatch--save-excursion-infix ()
  "Save the point and mark in each buffer during dispatch."
  :class 'transient-switch
  :key "se"
  :description "Excursions"
  :argument "excursion"
  :init-value (lambda (obj) (oset obj value "excursion")))

(transient-define-argument kdispatch--save-restriction-infix ()
  "Save and restore the current restriction in each buffer during dispatch."
  :class 'transient-switch
  :key "sr"
  :description "Restrictions"
  :argument "restrictions"
  :init-value (lambda (obj) (oset obj value "restrictions")))

(transient-define-argument kdispatch--merge-undo-infix ()
  "Merge all macro iterations into a single undo in each buffer."
  :class 'transient-switch
  :key "su"
  :description "Merge Undo"
  :argument "undo"
  :init-value (lambda (obj) (oset obj value "undo")))

(transient-define-argument kdispatch--save-windows-infix ()
  "Save and restore current window configuration during dispatch."
  :class 'transient-switch
  :key "sw"
  :description "Windows"
  :argument "windows")

(transient-define-suffix kdispatch--suffix (args)
  "Dispatch on the current region.
If the region is discontiguous (e.g. a rectangular region) then
dispatch on each contiguous component of the region."
  :transient 'transient--do-exit
  :key "r"
  :description "On Regions"
  (interactive (list (transient-args transient-current-command)))
  (kdispatch--thread -it-
      (region-bounds)
    (kdispatch--region-iterator -it- (member "reverse" args))
    (if (member "empty" args) -it- (kdispatch--skip-empty -it-))
    (if (member "undo" args) (kdispatch--merge-undo -it-) -it-)
    (if (member "restriction" args) (kdispatch--save-restriction -it-) -it-)
    (if (member "excursion" args) (kdispatch--save-excursion -it-) -it-)
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (kdispatch--change-region -it-))
      ("end" (kdispatch--at-end -it-))
      ("start" -it-))
    (kdispatch--pulse-on-record -it-)
    (if (member "windows" args) (kdispatch--save-windows -it-) -it-)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (kdispatch--macro -it- last-kbd-macro))
      ("append" (kdispatch--macro-append -it-))
      ("step-edit" (kdispatch--macro-step-edit -it-))
      (_ (kdispatch--macro -it-)))))

(transient-define-suffix kdispatch--point-and-mark-suffix (args)
  "Dispatch on the current point and mark."
  :transient 'transient--do-exit
  :key "v"
  :description "On Point and Mark"
  (interactive (list (transient-args transient-current-command)))
  (kdispatch--thread -it-
      (list (point) (mark t))
    (kdispatch--point-iterator -it- (member "reverse" args))
    (if (member "undo" args) (kdispatch--merge-undo -it-) -it-)
    (if (member "restriction" args) (kdispatch--save-restriction -it-) -it-)
    (if (member "excursion" args) (kdispatch--save-excursion -it-) -it-)
    (if (member "windows" args) (kdispatch--save-windows -it-) -it-)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (kdispatch--macro -it- last-kbd-macro))
      ("append" (kdispatch--macro-append -it-))
      ("step-edit" (kdispatch--macro-step-edit -it-))
      (_ (kdispatch--macro -it-)))))

(transient-define-suffix kdispatch--regions-suffix (iterator args)
  :transient 'transient--do-exit
  :key "r"
  :description "On Regions"
  (interactive (list (oref transient-current-prefix scope)
                     (transient-args transient-current-command)))
  (kdispatch--thread -it-
      (funcall iterator (member "reverse" args))
    (if (member "empty" args) -it- (kdispatch--skip-empty -it-))
    (if (member "undo" args) (kdispatch--merge-undo -it-) -it-)
    (if (member "restriction" args) (kdispatch--save-restriction -it-) -it-)
    (if (member "excursion" args) (kdispatch--save-excursion -it-) -it-)
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (kdispatch--change-region -it-))
      ("end" (kdispatch--at-end -it-))
      ("start" -it-))
    (kdispatch--pulse-on-record -it-)
    (if (member "windows" args) (kdispatch--save-windows -it-) -it-)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (kdispatch--macro -it- last-kbd-macro))
      ("append" (kdispatch--macro-append -it-))
      ("step-edit" (kdispatch--macro-step-edit -it-))
      (_ (kdispatch--macro -it-)))))

(transient-define-suffix kdispatch--lines-suffix (args)
  "Dispatch on each line between `point' and `mark'."
  :transient 'transient--do-exit
  :key "l"
  :description "On Lines"
  (interactive (list (transient-args transient-current-command)))
  (kdispatch--thread -it-
      (save-excursion
        (let ((beg (region-beginning))
              (end (region-end))
              (emptyp (member "empty" args))
              regions)
          (goto-char beg)
          (move-beginning-of-line 1)
          (while (< (point) end)
            (when-let ((eol (line-end-position))
                       (_ (or emptyp (not (= (point) eol)))))
              (push (cons (point) eol) regions))
            (forward-line))
          regions))
    (kdispatch--region-iterator -it- (not (member "reverse" args)))
    (if (member "undo" args) (kdispatch--merge-undo -it-) -it-)
    (if (member "restriction" args) (kdispatch--save-restriction -it-) -it-)
    (if (member "excursion" args) (kdispatch--save-excursion -it-) -it-)
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (kdispatch--change-region -it-))
      ("end" (kdispatch--at-end -it-))
      ("start" -it-))
    (kdispatch--pulse-on-record -it-)
    (if (member "windows" args) (kdispatch--save-windows -it-) -it-)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (kdispatch--macro -it- last-kbd-macro))
      ("append" (kdispatch--macro-append -it-))
      ("step-edit" (kdispatch--macro-step-edit -it-))
      (_ (kdispatch--macro -it-)))))

(transient-define-suffix kdispatch--isearch-suffix (args)
  "Dispatch on current isearch matches."
  :transient 'transient--do-exit
  :key "m"
  :description "On Matches"
  (interactive (list (transient-args transient-current-command)))
  (kdispatch--thread -it-
      (prog1
          (if (bound-and-true-p multi-isearch-buffer-list)
              (mapcan 'kdispatch--isearch-matches-in-buffer
                      (append
                       (remq (current-buffer) multi-isearch-buffer-list)
                       (list (current-buffer))))
            (kdispatch--isearch-matches-in-buffer
             (current-buffer)
             (pcase (transient-arg-value "matches=" args)
               ("after" 'after)
               ("before" 'before))))
        (isearch-exit))
    (kdispatch--region-iterator -it- (member "reverse" args))
    (if (member "undo" args) (kdispatch--merge-undo -it-) -it-)
    (if (member "restriction" args) (kdispatch--save-restriction -it-) -it-)
    (if (member "excursion" args) (kdispatch--save-excursion -it-) -it-)
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (kdispatch--change-region -it-))
      ("end" (kdispatch--at-end -it-))
      ("start" -it-))
    (kdispatch--pulse-on-record -it-)
    (if (member "windows" args) (kdispatch--save-windows -it-) -it-)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (kdispatch--macro -it- last-kbd-macro))
      ("append" (kdispatch--macro-append -it-))
      ("step-edit" (kdispatch--macro-step-edit -it-))
      (_ (kdispatch--macro -it-)))))

(transient-define-suffix kdispatch--text-property-suffix (prop value args)
  "Dispatch on regions of text with a text property."
  :transient 'transient--do-exit
  :key "t"
  :description "On Text Prop"
  (interactive
   (let* ((prop (intern (completing-read
                         "Property: "
                         (cl-loop for prop in (text-properties-at (point))
                                  by #'cddr
                                  collect prop)
                         nil t)))
          (vals (mapcar (lambda (s) (cons (message "%s" s) s))
                        (ensure-list (get-text-property (point) prop))))
          (val (alist-get (completing-read "Value: " vals) vals
                          nil nil #'string=)))
     (list prop val (transient-args transient-current-command))))
  (kdispatch--thread -it-
      (save-excursion
        (goto-char (point-min))
        (let (regions match)
          (while (setq match (text-property-search-forward
                              prop value t))
            (push (cons (prop-match-beginning match)
                        (prop-match-end match))
                  regions))
          regions))
    (kdispatch--region-iterator -it- (not (member "reverse" args)))
    (if (member "undo" args) (kdispatch--merge-undo -it-) -it-)
    (if (member "restriction" args) (kdispatch--save-restriction -it-) -it-)
    (if (member "excursion" args) (kdispatch--save-excursion -it-) -it-)
    (pcase-exhaustive (transient-arg-value "region=" args)
      ("change" (kdispatch--change-region -it-))
      ("end" (kdispatch--at-end -it-))
      ("start" -it-))
    (kdispatch--pulse-on-record -it-)
    (if (member "windows" args) (kdispatch--save-windows -it-) -it-)
    (pcase (transient-arg-value "last-kmacro=" args)
      ("apply" (kdispatch--macro -it- last-kbd-macro))
      ("append" (kdispatch--macro-append -it-))
      ("step-edit" (kdispatch--macro-step-edit -it-))
      (_ (kdispatch--macro -it-)))))

(transient-define-prefix kdispatch-prefix ()
  "Transient menu for macro dispatch on regions."
  [:description
   kdispatch--kmacro-ring-display
   [("c" "Set Counter" kmacro-set-counter :transient t)
    ("f" "Set Format" kdispatch--set-counter-format-infix)
    ("e" "Edit Macro"
     (lambda (arg)
       (interactive "P")
       (kdispatch-recursive-edit-kmacro arg)
       (transient-resume))
     :transient transient--do-suspend)
    ("E" "Edit Lossage"
     (lambda ()
       (interactive)
       (kdispatch-recursive-edit-lossage)
       (transient-resume))
     :transient transient--do-suspend)]
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("M" "Display"
     (lambda ()
       (interactive)
       (kmacro-display last-kbd-macro t))
     :transient t)
    ("g" "Push Register" kdispatch--set-macro-ring-head :transient t)]]
  [:description
   "Dispatch"
   [(kdispatch--suffix)
    (kdispatch--lines-suffix)
    (kdispatch--point-and-mark-suffix)
    (kdispatch--text-property-suffix)]
   [(kdispatch--macro-infix)
    (kdispatch--region-infix)
    (kdispatch--empty-infix)
    (kdispatch--order-infix)]]
  [:description
   "Save State"
   [(kdispatch--merge-undo-infix)
    (kdispatch--save-windows-infix)]
   [(kdispatch--save-restriction-infix)
    (kdispatch--save-excursion-infix)]]
  (interactive)
  (kmacro-display last-kbd-macro t)
  (transient-setup 'kdispatch-prefix))

(transient-define-prefix kdispatch-isearch-prefix ()
  "Transient menu for macro dispatch on regions."
  [:description
   kdispatch--kmacro-ring-display
   [("c" "Set Counter"
     (lambda ()
       (interactive)
       (with-isearch-suspended
        (call-interactively 'kmacro-set-counter)))
     :transient t)
    ("f" "Set Format" kdispatch--set-counter-format-infix)
    ("e" "Edit Macro"
     (lambda (arg)
       (interactive "P")
       (with-isearch-suspended (kdispatch-recursive-edit-kmacro arg))
       (transient-resume))
     :transient transient--do-suspend)
    ("E" "Edit Lossage"
     (lambda ()
       (interactive)
       (with-isearch-suspended (kdispatch-recursive-edit-lossage))
       (transient-resume))
     :transient transient--do-suspend)]
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("M" "Display"
     (lambda ()
       (interactive)
       (kmacro-display last-kbd-macro t))
     :transient t)
    ("g" "Push Register" kdispatch--set-macro-ring-head :transient t)]]
  ["Dispatch"
   [(kdispatch--isearch-suffix)]
   [(kdispatch--macro-infix)
    (kdispatch--region-infix)
    (kdispatch--matches-infix)
    (kdispatch--order-infix)]]
  [:description
   "Save State"
   [(kdispatch--merge-undo-infix)
    (kdispatch--save-windows-infix)]
   [(kdispatch--save-restriction-infix)
    (kdispatch--save-excursion-infix)]]
  (interactive)
  (kmacro-display last-kbd-macro t)
  (transient-setup 'kdispatch-isearch-prefix))

(transient-define-prefix kdispatch-regions-prefix (iterator)
  "Transient menu for macro dispatch on regions."
  [:description
   kdispatch--kmacro-ring-display
   [("c" "Set Counter" kmacro-set-counter :transient t)
    ("f" "Set Format" kdispatch--set-counter-format-infix)
    ("e" "Edit Macro"
     (lambda (arg)
       (interactive "P")
       (kdispatch-recursive-edit-kmacro arg)
       (transient-resume))
     :transient transient--do-suspend)
    ("E" "Edit Lossage"
     (lambda ()
       (interactive)
       (kdispatch-recursive-edit-lossage)
       (transient-resume))
     :transient transient--do-suspend)]
   [("n" "Next" kmacro-cycle-ring-previous :transient t)
    ("p" "Previous" kmacro-cycle-ring-next :transient t)
    ("M" "Display"
     (lambda ()
       (interactive)
       (kmacro-display last-kbd-macro t))
     :transient t)
    ("g" "Push Register" kdispatch--set-macro-ring-head :transient t)]]
  ["Dispatch"
   [(kdispatch--regions-suffix)]
   [(kdispatch--macro-infix)
    (kdispatch--region-infix)
    (kdispatch--empty-infix)
    (kdispatch--order-infix)]]
  [:description
   "Save State"
   [(kdispatch--merge-undo-infix)
    (kdispatch--save-windows-infix)]
   [(kdispatch--save-restriction-infix)
    (kdispatch--save-excursion-infix)]]
  (interactive (list nil))
  (unless iterator (user-error "No regions"))
  (kmacro-display last-kbd-macro t)
  (transient-setup 'kdispatch-regions-prefix nil nil :scope iterator))

(provide 'kmacro-dispatch)
;;; kmacro-dispatch.el ends here
