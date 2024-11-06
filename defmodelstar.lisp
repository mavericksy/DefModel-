;; Postmodern defmodel*
;; based on cmoore' https://github.com/cmoore/defmodel
;;
;; Expands on a couple functions and adds a little error handling
;;
;;
(defpackage #:defmodelstar
  (:use :cl))
;;
;; TODO signal errors.
;;
(define-condition defmodel-error (error)
  ((text :initarg text :reader text)
   (inner :initarg inner :reader inner :type error)))
;;
;;
(define-condition defmodel-exists (simple-condition)
  ((text :initarg text :reader text)
   (inner :initarg inner :reader inner :type error)))
;;
;;
(defparameter *create-tables* (make-hash-table :synchronized t))
;;
(defun add-create-table (name def)
  (setf (gethash name *create-tables*) def))
;;
;;
;; FIXME these symbol string functions are madness
;; TODO clean up the interning and formatting of symbols and names below
;;
(defun symb (a b)
  (intern (format nil "~a-~a" (symbol-name a) (symbol-name b))))

;; FIXME assumptions made here. obj.* is the assumption, fix in the SQL
(defun obj-symb (a b)
  (intern (format nil "obj.~a-~a" (symbol-name a) (symbol-name b))))

(defun symb1 (a b)
  (intern (format nil "~a~a" (symbol-name a) (symbol-name b))))

(defun symb2 (a b)
  (intern (format nil "~a-~a"
                  (symbol-name a) (symbol-name b))
          :keyword))

(defun strg (a b)
  (string-downcase (format nil "~a~a" a (symbol-name b))))

(defun strg1 (a)
  (string-downcase (format nil "~a" (symbol-name a))))
;;
;;
;;
(defmacro defmodel* (name sorting slot-definitions
                     &optional (table-prefix "t_"))
  "
  Define the form and functions of the models.
    name             - what to call the class
    acl              - whether to include acl mixin
    user-ref         - whether to include the user FK mixin
    sorting          - priority sequence to run the create-tables
    slot-definitions - Postmodern DAO-CLASS definitions
    table-prefix     - Prefix all database relations default t_
 "
  (let ((exports (mapcan (lambda (spec)
                           (when (getf (cdr spec) :export)
                             (let ((n (getf (cdr spec) :accessor)))
                               (list n))))
                         slot-definitions))
        (indexable (mapcan (lambda (spec)
                             (when (getf (cdr spec) :index)
                               (let ((n (car spec)))
                                 (list n))))
                           slot-definitions))
        (keys (mapcan (lambda (spec)
                        (when (getf (cdr spec) :key)
                          (let ((n (car spec)))
                            (list n))))
                      slot-definitions)))
    `(progn

;;;;;;;;;;;;;;
       ;; DEFCLASS ;;
;;;;;;;;;;;;;;

       (defclass ,name ()
         ;;
         (,@slot-definitions)
         ;;
         (:metaclass pomo:dao-class)
         (:table-name ,(strg table-prefix name))
         (:keys ,@(if (zerop (list-length keys))
                      (list 'uid)
                      keys)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; INSTANCE-TABLE-DEFINITION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'table-definition) ()
         (pomo:dao-table-definition ',name))

;;;;;;;;;;;;;;;;;;;;;
       ;; ALTER TABLe DAO ;;
;;;;;;;;;;;;;;;;;;;;;
       ;; (with-pg
       ;;   (dao-alter-table ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; INSTANCE-DROP-TABLE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

       ;; FIXME protect this command better, for great good
       (defun ,(symb name 'drop-table) ()
         (with-pg
             (when (pomo:table-exists-p ',(symb1 :t_ name))
               (pomo:execute ,(ppcre:regex-replace
                               "-"
                               (string-downcase
                                (format nil "drop table ~a"
                                        (strg table-prefix name)))
                               "_")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; INSTANCE-CREATE-TABLE-DAO ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'create-table-dao) ()
         (with-pg
             (unless (pomo:table-exists-p ',(symb1 :t_ name))
               (pomo:execute (pomo:dao-table-definition ',name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; PUSH INSTANCE-CREATE-TABLE-DAO TO *CREATE-TABLES* ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (add-table ,sorting
                  #'(lambda ()
                      (,(symb name 'create-table-dao))))

;;;;;;;;;;;;;;;;;;;;;;;;
       ;; TABLE-CREATE-TABLE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'create-table) ()
         (handler-case
             (with-pg
                 (unless (pomo:table-exists-p ',(symb1 :t_ name))
                   (pomo:execute (pomo:create-table ',name))))
           (error (c) (error 'defmodel-error :text "Create Table Error" :inner c))))
       ;;
       (export ',(symb name 'create-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; TABLE-CREATE-INDEXES                              ;;
       ;; Will throw an error if the indexes already exist. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'create-indexes) ()
         (with-pg
             ,@(mapcar (lambda (idx)
                         `(pomo:query (:create-index
                                       (quote ,(symb idx :idx))
                                       :on ,(strg "t_" name)
                                       :fields (quote ,idx))))
                       indexable)))
       ;;
       ;; Export symbols for all accessors marked as 'export'
       ,@(mapcar (lambda (name) `(export ',name))
                 exports)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (table-create :slot "value" :slot "value" ...) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defmacro ,(symb name 'create) (&rest args)
         `(handler-case
              (with-pg
                  (pomo:make-dao ',',name ,@args))
            ;;
            (pomo:database-error (e)
              (if (eql "23505" (pomo:database-error-code e))
                  (signal 'defmodel-exists "Item exists")
                  (error 'defmodel-error
                         :text "Create ERROR UNKNOWN"
                         :inner c)))
            ;;
            (error (c)
              (error 'defmodel-error :text "Create ERROR" :inner c))))
       ;;
       (export ',(symb name 'create))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (table-get-all) -> (#<TABLE> #<TABLE> ...) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'get-all) ()
         (with-pg
             (pomo:select-dao ',name)))
       ;;
       (export ',(symb name 'get-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; (table-get "uid" ->) #<TABLE> ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'get) (id)
         (with-pg
             (pomo:get-dao ',name id)))
       ;;
       (export ',(symb name 'get))

;;;;;;;;;;;;;;;;;;;;;;;
       ;; <TABLE>-QUERY-DAO ;;
;;;;;;;;;;;;;;;;;;;;;;;

       (defmacro ,(symb name 'query-dao) (expression)
         `(with-pg
              (pomo:query-dao ',',name ,expression)))
       ;;
       (export ',(symb name 'query-dao))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; (table-select (:and (:= 'slot "value")  ;;
       ;;                     (:not-null 'slot))) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defmacro ,(symb name 'select) (sql-test &optional sort)
         `(with-pg
              (pomo:select-dao ',',name ,sql-test ,sort)))
       ;;
       (export ',(symb name 'select))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (defvar x (table-get "uid")) ;;
;;; (setf (table-slot x) "Honk") ;;
;;; (table-update x)             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'update) (,name)
         (with-pg
             (pomo:update-dao ,name)))
       ;;
       (export ',(symb name 'update))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (defvar x (table-get "uid")) ;;
;;; (table-delete x)             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'delete) (,name)
         (with-pg
             (pomo:delete-dao ,name)))
       ;;
       (export ',(symb name 'delete))
       ;;
       )))
