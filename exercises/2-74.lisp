;;;; 2-74
;;;; 2-4-3
;;;; 2022/07/09

;;; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company
;;; consisting of a large number of independent divisions located all over the
;;; world. The company's computer facilities have just been interconnected by
;;; means of a clever network-interfacing scheme that makes the entire network
;;; appear to any user to be a single computer. Insatiable's president, in her
;;; first attempt to exploit the ability of the network to extract
;;; administrative information from division files, is dismayed to discover
;;; that, although all the division files have been implemented as data
;;; structures in Scheme, the particular data structure used varies from
;;; division to division. A meeting of division managers is hastily called to
;;; search form a strategy to integrate the files that will satisfy
;;; headquarters' needs while preserving the existing autonomy of the divisions.
;;; Show how such a strategy can be implemented with data-directed
;;; programming. As an example, suppose that each division's personnel records
;;; consist of a single file, which contains a set of records keyed on
;;; employees' names. The structure of the set varies from division to
;;; division. Furthermore, each employee's record is itself a set (structured
;;; differently from division to division) that contains information keyed under
;;; identifiers such as `address` and `salary`. In particular:

;;; a. Implement for headquarters a `get-record` procedure that retrieves a
;;; specified employee's record from a specified personnel file. The procedure
;;; should be applicable to any division's file. Explain how the individual
;;; divisions' file should be structured. In particular, what type information
;;; must be supplied?

;;             |              type
;;  operation  | unordered-list | ordered-list
;; ____________|________________|______________
;;  get-record |   procedure    |   procedure

(defun get-record (tagged-personnel-file employee-name)
  (let ((type-tag (type-tag personnel-file))
        (personnel-file (contents tagged-personnel-file)))
    (funcall (get 'get-record type-tag)
             personnel-file employee-name)))

(put 'get-record 'unordered-list
     (lambda (personnel-file employee-name)
       (labels ((iterator (rest-records)
                  (if rest-records
                      (let ((current-record (car rest-records)))
                        (if (equal employee-name (employee-name current-record))
                            current-record
                            (iterator (cdr rest-records))))
                      '())))
         (iterator personnel-file))))
;; (put 'get-record 'ordered-list ...)

;;; b. Implement for headquarters a `get-salary` procedure that returns the
;;; salary information from a given employee's record from any division's
;;; personnel file. How should the record be structured in order to make this
;;; operation work?

(defun get-salary (tagged-personnel-file employee-name)
  (let ((record (get-record tagged-personnel-file employee-name)))
    (get-value 'salary record)))

;; Structure: json.

;;; c. Implement for headquarters a `find-employee-record` procedure. This
;;; should search all the divisions' files for the record of a given employee and
;;; return the record. Assume that this procedure takes as arguments an
;;; employee's name and a list of all the divisions' file.

(defun get-employee-record (employee-name &rest tagged-personnel-files)
  (labels ((iterator (rest-tagged-personnel-files)
             (if rest-tagged-personnel-files
                 (let ((record (get-record tagged-personnel-file
                                           (car rest-tagged-personnel-files))))
                   (if record
                       (cons record (iterator (cdr rest-tagged-personnel-files)))
                       (iterator (cdr rest-tagged-personnel-files))))
                 '())))
    (iterator tagged-personnel-files)))

;;; d. When Insatiable takes over a new company, what changes must be made in
;;; order to incorporate the new personnel information into the central system?
