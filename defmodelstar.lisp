;; Postmodern defmodel*
;; based on cmoore' https://github.com/cmoore/defmodel
;;
;; Expands on a couple functions and adds a little error handling
;;
;; Opinionated usage of a user class called `usr`
;;
;; Adds the ACL. TODO make video for the ACL and the defmodel*
;;
;; FIXME these symbol string functions are madness
;; TODO clean up the interning and formatting of symbols and names below
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
(defmacro uid-slot-definition (keys)
  ``((uid :col-type uuid
          :initform (util::uuid)
          :initarg :uid
          :accessor uid
          ,@(when (zerop (list-length ,keys))
              `(:col-primary-key t
                :col-unique t
                :key t))
          :export t)))
;;
;;
(defmacro user-id-slot-definitions ()
  ``((usr-uid :col-type uuid
              :initarg :usr-uid
              :reader usr-uid
              :export t
              :col-references ((t-usr uid)))))
;;
;;
(defmacro acl-slot-definitions ()
  ``((acl-unix :col-type integer
          :col-default 384 ; 0600
          :initform 384 ; 0600
          :accessor acl-unix
          :initarg :acl-unix
                          :export t)
    (acl-owner :col-type uuid
           :accessor acl-owner
           :initarg :acl-owner
           :export t
               :index t)
    (acl-group :col-type uuid
           :accessor acl-group
           :initarg :acl-group
           :export t
               :index t)
    (acl-status :col-type integer
            :initform 0
            :accessor acl-status
            :initarg :acl-status
                :export t)))
;;
;;
(defmacro defmodel* (name acl user-ref sorting slot-definitions
                     &optional
                       (impl-actions nil impl-actions-supplied-p)
                       (exclude nil exclude-supplied-p)
                       (trigger nil trigger-supplied-p))
  "
  Define the form and functions of the models.
    name             - what to call the class
    acl              - whether to include acl mixin
    user-ref         - whether to include the user FK mixin
    sorting          - priority sequence to run the create-tables
    slot-definitions - Postmodern DAO-CLASS definitions
    impl-actions     - Which actions will this object have
    exclude          - leave out the base uid slot definition
    trigger          - create the trigger function
 "
  (declare (ignorable impl-actions exclude trigger))
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
         (,@(unless (and exclude-supplied-p exclude)
              (uid-slot-definition keys))
          ;;
          ,@(when user-ref
              (user-id-slot-definitions))
          ;;
          ,@(when acl
              (acl-slot-definitions))
          ;;
          ,@slot-definitions)
         ;;
         (:metaclass pomo:dao-class)
         (:table-name ,(strg "t_" name))
         (:keys ,@(if (zerop (list-length keys))
                      (list 'uid)
                      keys)))

       ,@(unless (and exclude-supplied-p exclude)
           `((defgeneric ,(symb name 'uid) (obj))

             (defmethod ,(symb name 'uid) ((obj ,name))
               (with-slots (uid) obj
                 uid))

             (defmethod print-object ((obj ,name) stream)
               (print-unreadable-object (obj stream :type t)
                 (with-accessors ((id ,(symb name 'uid)))
                     obj
                   (format stream "ID ~a" id))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; INSTANCE-TABLE-DEFINITION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'table-definition) ()
         (pomo:dao-table-definition ',name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; CREATE IMPLEMENTED ACTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       ;; TODO implementedaction entries as post create hook
       ,@(when (and impl-actions impl-actions-supplied-p)
           (mapcar (lambda (lst)
                     `(add-implaction-def ,(format nil "~A~A~A" (strg1 (symb1 :t_ name)) (first lst) (second lst))
                       (lambda ()
                         (handler-case
                             (implementedaction-create
                              :table ',(strg1 (symb1 :t_ name))
                              :action ,(first lst)
                              :status ,(second lst))
                           (pomo:database-error ()
                             (log:warn :db "Impl Action Exists"))))))
                   impl-actions))
       ;;
       (export ',(symb name 'uid))

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
                                        (strg "t_" name)))
                               "_")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; INSTNACE-CREATE-TABLE-DAO ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'create-table-dao) ()
         (with-pg
             (unless (pomo:table-exists-p ',(symb1 :t_ name))
               (pomo:execute (pomo:dao-table-definition ',name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;; PUSH INSTANCE-CREATE-TABLE-DAO TO *FOP-TABLES* ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       (add-table ,sorting
                  #'(lambda ()
                      (,(symb name 'create-table-dao))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       ;;  Create trigger if needed ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       ,(when (and trigger trigger-supplied-p)
          `(progn
             (defun ,(symb name 'create-trigger) ()
               (with-pg
                   (when (pomo:table-exists-p ',(symb1 :t_ name))
                     (handler-case
                         (pomo:execute
                          ,(format nil "CREATE TRIGGER tg_~A_audit_log
                                     BEFORE INSERT OR UPDATE OR DELETE ON ~A.~A
                                     FOR EACH ROW EXECUTE
                                     FUNCTION fn_audit_trigger()"
                                   (string-downcase (strg "t_" name))
                                   "public"
                                   (string-downcase (strg "t_" name))))
                       (pomo:database-error ()
                         (log:warn :db "Trigger already set"))))))
             (add-trigger-def ,(format nil "~Aaudit" name)
                              #',(symb name 'create-trigger))
             (export ',(symb name 'create-trigger))))

;;;;;;;;;;;;;;;;;;;;;;;;
       ;; TABLE-CREATE-TABLE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

       (defun ,(symb name 'create-table) ()
         (handler-case
             (with-pg
                 (unless (pomo:table-exists-p ',(symb1 :t_ name))
                   (pomo:execute (pomo:create-table ',name))))
           (error (c) (log:error :db "Error ~a" c))))
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
                  (log:warn :db "Item exists")))
            ;;
            (error (c)
              (log:error :db "ERROR ~A" c))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; (find-by-table-slot "value") -> (#<TABLE> #<TABLE>) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

       ;; ,@(mapcar
       ;;    (lambda (slot)
       ;;      `(progn
       ;;         (defun ,(symb 'find-by slot) (value)
       ;;           (,(symb name 'select)
       ;;            (:= ',(intern
       ;;                   (cadr
       ;;                    (split-sequence:split-sequence
       ;;                     #\-
       ;;                     (string slot))))
       ;;                value)
       ;;            ',(intern
       ;;               (cadr
       ;;                (split-sequence:split-sequence
       ;;                 #\-
       ;;                 (string slot))))))
       ;;         ;;
       ;;         (export ',(symb 'find-by slot))))
       ;;    exports)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The ACL is comprised of four SQL queries.
;;;
;;; Actionable Objects
;;; Table Privileges
;;; Object Privileges
;;; Acl Entries
;;;;;;;;;;;;;;;;;;;;;;;;;

       ,(when acl
;;;
          `(progn
             ;;
             ;; ACL Entries for an object
             ;;
             (defun ,(symb name 'acl-entries) (obj-id)
               (with-pg
                   (pomo:query
                    (:select
                     'tp.role 'tp.who
                     (:as
                      (:case
                          ((:= 'tp.role "user")
                           (:coalesce 'tu.name "--DNE--"))
                        ((:= 'tp.role "group")
                         "")
                        ((:= 'tp.role "owner_group")
                         "")
                        (:else "NONE"))
                      'name)
                     'tp.action 'tp.priv-type
                     (:as 'tp.related_table 'table)
                     'tp.related_uuid 'ia.status
                     :from (:as 't_privilege 'tp)
                     ;;
                     :inner-join (:as 't_action 'ta)
                     :on (:= 'ta.title 'tp.action)
                     ;;
                     :inner-join (:as ',(symb1 :t_ name) 'obj)
                     :on (:= 'obj.uid (:type obj-id uuid))
                     ;;
                     :inner-join (:as 't_implementedaction 'ia)
                     :on (:and (:= 'ia.table ,(strg1 (symb1 :t_ name)))
                               (:= 'ia.action 'ta.title))
                     :left-join (:as 't_usr 'tu)
                     :on (:and
                          (:= 'tp.role "user")
                          (:= 'tp.who 'tu.uid))
                     ;;
                     ;;
                     :where
                     (:and (:or
                            (:and (:= 'tp.priv-type "object")
                                  (:= 'tp.related_uuid (:type obj-id uuid)))
                            (:in 'tp.priv-type (:set "table" "global"))
                            (:and
                             (:= 'tp.role "self")
                             (:= 'tp.related_table
                                 "t_usr")))
                           (:= 'tp.related_table ',(strg1 (symb1 :t_ name))))))))
;;;
;;;
             (defun ,(symb name 'object-privileges) (obj-id user-id)
               (with-pg
                   (pomo:query
                    (:with (:as 'userGroupBit
                                (:parens
                                 (:select (:sum 'group_bit) :from (:as 't_group 'tg)
                                  :join (:as 't_groupusers 'tgu)
                                  :on (:= 'tgu.group-uid 'tg.uid)
                                  :where (:= 'tgu.user-uid (:type user-id uuid)))))
                           ;;
                           (:select (:distinct 'ta.title)
                            :from (:as 't_action 'ta)
                            ;;
                            :inner-join (:as ',(symb1 :t_ name) 'obj)
                            :on (:= 'obj.uid (:type obj-id uuid))
                            ;;
                            :inner-join (:as 't_implementedaction 'ia)
                            :on (:and
                                 (:= 'ia.action 'ta.title)
                                 (:= 'ia.table (strg1 ',(symb1 :t_ name)))
                                 (:or
                                  (:= 'ia.status 0)
                                  (:<> (:& 'ia.status 'obj.acl-status) 0)))
                            ;;
                            :left-join (:as 't_privilege 'tp)
                            :on (:and
                                 (:= 'pr.related_table ',(symb1 :t_ name))
                                 (:= 'tp.action 'ta.title)
                                 (:or
                                  (:and (:= 'tp.priv-type "object")
                                        (:= 'tp.related-uuid (:type obj-id uuid)))
                                  (:and (:= 'tp.priv-type "self")
                                        (:= (:type user-id uuid)(:type obj-id uuid))
                                        (:= ',(symb1 :t_ name) "t_usr"))))
                            :where
                            (:and
                             'ta.apply_object
                             (:or
                             ;; Root can do anything
                              (:<> 0 0)
                              ;; READ PERMISSIONS
                              (:and
                               (:= 'ta.title "read")
                               (:or
                                (:<> (:& 'obj.unix 4) 0) ; other_read
                                (:and
                                 (:<> (:& 'obj.unix 32) 0) ; group_read
                                 ;; User is in group
                                 (:<>
                                  (:& (:select '* :from 'userGroupBit)
                                      (:select 'group_bit :from (:as 't_group 'tg)
                                       :where (:= 'tg.uid 'obj.group)))
                                  0))
                                (:and
                                 (:<> (:& 'obj.unix 256) 0) ; owner_read
                                 (:= 'obj.owner (:type user-id uuid)))))
                              ;; WRITE PERMISSIONS
                              (:and
                               (:= 'ta.title "write")
                               (:or
                                (:<> (:& 'obj.unix 2) 0) ; other_write
                                (:and
                                 (:<> (:& 'obj.unix 16) 0) ; group_write
                                 ;; User is in group
                                 (:<>
                                  (:& (:select '* :from 'userGroupBit)
                                      (:select 'group_bit :from (:as 't_group 'tg)
                                       :where (:= 'tg.uid 'obj.group)))
                                  0))
                                (:and
                                 (:<> (:& 'obj.unix 128) 0) ; owner_write
                                 (:= 'obj.owner (:type user-id uuid)))))
                              ;; DELETE PERMISSIONS
                              (:and
                               (:= 'ta.title "delete")
                               (:or
                                (:<> (:& 'obj.unix 1) 0) ; other_delete
                                (:and
                                 (:<> (:& 'obj.unix 8) 0) ; group_delete
                                 ;; User is in group
                                 (:<>
                                  (:& (:select '* :from 'userGroupBit)
                                      (:select 'group_bit :from (:as 't_group 'tg)
                                       :where (:= 'tg.uid 'obj.group)))
                                  0))
                                (:and
                                 (:<> (:& 'obj.unix 64) 0) ; owner_delete
                                 (:= 'obj.owner (:type user-id uuid)))))
                              ;;
                              ;; User owns this object
                              (:and (:= 'tp.role "user")
                                    (:= 'tp.who (:type user-id uuid)))
                              ;; Owner is this user
                              (:and (:= 'tp.role "owner")
                                    (:= 'obj.owner (:type user-id uuid)))
                              ;; Group owns object and user is in group
                              (:and (:= 'tp.role "owner_group")
                                    (:<>
                                     (:& (:select '* :from 'userGroupBit)
                                         (:select 'group_bit :from (:as 't_group 'tg)
                                          :where (:= 'obj.group 'tg.uid)))
                                     0))
                              ;; Group priv for object
                              (:and (:= 'tp.role "group")
                                    (:<>
                                     (:& (:select '* :from 'userGroupBit)
                                         (:select 'group_bit :from (:as 't_group 'tg)
                                          :where (:= 'tp.who 'tg.uid)))
                                     0))
                              (:= 'tp.role "self"))))))))
;;;
;;;
             (defun ,(symb name 'table-privileges) (user-id)
               (with-pg
                   (pomo:query
                    (:with (:as 'userGroupBit
                                (:parens
                                 (:select (:sum 'group_bit) :from (:as 't_group 'tg)
                                  :join (:as 't_groupusers 'tgu)
                                  :on (:= 'tgu.group-uid 'tg.uid)
                                  :where (:= 'tgu.user-uid (:type user-id uuid)))))
                           ;;
                           (:select (:as 'ta.title 'title)
                            :from (:as 't_action 'ta)
                            ;;
                            :left-join (:as 't_privilege 'tp)
                            :on (:and
                                 (:= 'tp.related_table (strg1 ',(symb1 :t_ name)))
                                 (:= 'tp.action 'ta.title)
                                 (:= 'tp.priv-type "table"))
                            ;;
                            ;; Only applying to the entire table
                            :where (:and
                                    (:not 'ta.apply-object)
                                    (:or
                                     (:<> (:& (:select '* :from 'userGroupBit) 1) 0)
                                     (:and
                                      (:= 'tp.role "user")
                                      (:= 'tp.who (:type user-id uuid))))))))))
;;;
;;;
             (defun ,(symb name 'actionable-objects) (action user-id)
               (with-pg
                   (pomo:query
                    (:with (:as 'userGroupBit
                                (:parens
                                 (:select (:sum 'group_bit) :from (:as 't_group 'tg)
                                  :join (:as 't_groupusers 'tgu)
                                  :on (:= 'tgu.group-uid 'tg.uid)
                                  :where (:= 'tgu.user-uid (:type user-id uuid)))))
                           ;;
                           (:select 'obj.*
                            :from (:as ',(symb1 :t_ name) 'obj)
                            ;; Implemented Actions
                            ;; Action must be present for table and as either
                            ;; applying to status 0 or status of action applies
                            ;; to obj status
                            :inner-join (:as 't_implementedaction 'ia)
                            :on (:and
                                 (:= 'ia.table (strg1 ',(symb1 :t_ name)))
                                 (:= 'ia.action action)
                                 (:or (:= 'ia.status 0)
                                      (:<> (:& 'ia.status 'obj.acl-status) 0)))
                            ;; Actions
                            ;; Is this Action registered
                            :inner-join (:as 't_action 'ta)
                            :on (:= 'ta.title action)
                            ;; Privileges
                            ;; Does a privilege exist, for the table
                            ;; with action equal to t_action entry
                            ;; and either an object privilege which needs related_uuid
                            ;; or global privilege
                            ;; or is a self priv checked on user id and must be t_usr
                            :left-join (:as 't_privilege 'tp)
                            :on (:and
                                 (:= 'tp.related_table (strg1 ',(symb1 :t_ name)))
                                 (:= 'tp.action 'ta.title)
                                 (:or
                                  (:and (:= 'tp.priv-type "object")
                                        (:= 'tp.related_uuid 'obj.uid))
                                  (:= 'tp.priv-type "global")
                                  (:and (:= 'tp.role "self")
                                        (:= (:type user-id uuid) 'obj.uid)
                                        (:= (strg1 ',(symb1 :t_ name)) "t_usr"))))
                            ;;
                            ;; Only applying this to objects
                            :where
                            (:and
                             'ta.apply_object
                             (:or
                             ;; Root can do anything
                              (:<> 0 0)
                              ;; READ PERMISSIONS
                              (:and
                               (:= 'ta.title "read")
                               (:or
                                (:<> (:& 'obj.acl-unix 4) 0) ; other_read
                                (:and
                                 (:<> (:& 'obj.acl-unix 32) 0) ; group_read
                                 ;; User is in group
                                 (:<>
                                  (:& (:select '* :from 'userGroupBit)
                                      (:select 'group_bit :from (:as 't_group 'tg)
                                       :where (:= 'tg.uid 'obj.acl-group)))
                                  0))
                                (:and
                                 (:<> (:& 'obj.acl-unix 256) 0) ; owner_read
                                 (:= 'obj.acl-owner (:type user-id uuid)))))
                              ;; WRITE PERMISSIONS
                              (:and
                               (:= 'ta.title "write")
                               (:or
                                (:<> (:& 'obj.acl-unix 2) 0) ; other_write
                                (:and
                                 (:<> (:& 'obj.acl-unix 16) 0) ; group_write
                                 ;; User is in group
                                 (:<>
                                  (:& (:select '* :from 'userGroupBit)
                                      (:select 'group_bit :from (:as 't_group 'tg)
                                       :where (:= 'tg.uid 'obj.acl-group)))
                                  0))
                                (:and
                                 (:<> (:& 'obj.acl-unix 128) 0) ; owner_write
                                 (:= 'obj.acl-owner (:type user-id uuid)))))
                              ;; DELETE PERMISSIONS
                              (:and
                               (:= 'ta.title "delete")
                               (:or
                                (:<> (:& 'obj.acl-unix 1) 0) ; other_delete
                                (:and
                                 (:<> (:& 'obj.acl-unix 8) 0) ; group_delete
                                 ;; User is in group
                                 (:<>
                                  (:& (:select '* :from 'userGroupBit)
                                      (:select 'group_bit :from (:as 't_group 'tg)
                                       :where (:= 'tg.uid 'obj.acl-group)))
                                  0))
                                (:and
                                 (:<> (:& 'obj.acl-unix 64) 0) ; owner_delete
                                 (:= 'obj.acl-owner (:type user-id uuid)))))
                              ;;
                              ;; User owns this object
                              (:and (:= 'tp.role "user")
                                    (:= 'tp.who (:type user-id uuid)))
                              ;; Owner is this user
                              (:and (:= 'tp.role "owner")
                                    (:= 'obj.acl-owner (:type user-id uuid)))
                              ;; Group owns object and user is in group
                              (:and (:= 'tp.role "owner_group")
                                    (:<>
                                     (:& (:select '* :from 'userGroupBit)
                                         (:select 'group_bit :from (:as 't_group 'tg)
                                          :where (:= 'tg.uid 'obj.acl-group)))
                                     0))
                              ;; Group priv for object
                              (:and (:= 'tp.role "group")
                                    (:<>
                                     (:& (:select '* :from 'userGroupBit)
                                         (:select 'group_bit :from (:as 't_group 'tg)
                                          :where (:= 'tp.who 'tg.uid)))
                                     0))
                              (:= 'tp.role "self")))))
                    (:dao ,name))))

             (defun ,(symb name 'get-all-actionable-objects) (action usr-id)
               (with-pg
                   (,(symb name 'actionable-objects) action usr-id)))
             ;;
             (export ',(symb name 'get-all-actionable-objects)))))))
