#lang racket/gui
(require db)

;;; purpose

; to generate an editor dialog based solely on the name of an SQL table and the row ID.
;
; 1) list table column names and types (DESCRIBE TABLE);
;
; 2) gather row contents from ID into a list;
;
; 3) for each column,
;      - convert the column name to human readable;
;      - convert the column type to gui control type (all text-fields for start);
;      - generate the control and add it to the dialog;
;      - fill the control with the current table column's value;
;      - build a list of the control names for later value fetching.

; 4) when the user clicks on the OK button,
;      - if at least one is empty, display warning ("At least one field was left empty. Are you sure?");
;      - if the result list matches the original content, do nothing.
;      - (set! results
;          (map
;            (λ (control) (send control get-value))
;            control-list))
;      - return results

;;; defs

(define appname "Table Editor")

(define my-db
  (mysql-connect #:server "my_server"
                 #:port 3306
                 #:database "my_database"
                 #:user "my_login"
                 #:password "my_password"))

;; parametric composition macro
;; macro to compose functions passing an 'x' parameter.
;; returns a function that takes one parameter
;; the result of each composed function is passed on to the next through the x variable.
(define-syntax (composex stx)
  (syntax-case stx ()
    ((_ f1 ...)
     (with-syntax ([x-var (datum->syntax stx 'x)])
       #'(compose1 (λ (x-var) f1) ...)))))

;; returns a clean label given an SQL table column name
(define (column->label column-name)
  ((composex (string-trim      x)
             (string-titlecase x)
             (string-replace   x #rx"_|-" " "))
   column-name))

;; return a list of the table field types
(define (get-table-data db table)
  (let* ((query        (string-append "DESCRIBE " table))
         (fields       (query-rows db query)) ; -> vector
         (column-names (map (λ (v) (vector-ref v 0)) fields))
         (column-types (map (λ (v) (vector-ref v 1)) fields)))
    (values column-names column-types)))

;; return a list of row's values from the given table
(define (get-table-values db table primary-id primary-value)
  (let* ((query  (string-append "SELECT * FROM " table " WHERE " primary-id "=" primary-value))
         (values (map ~a (vector->list (query-row db query)))))
    values))

;; generate a text-field with the right params
(define (make-text-field parent label value)
  (new text-field%
       [label label]
       [init-value value]
       [style (list 'single)]
       [parent parent]       
       [vert-margin 5]
       [horiz-margin 10]))

;; generate a row editor dialog
(define (create-table-editor title msg db table primary-id primary-value)

  ; gather table data
  (define-values (column-names
                  column-types)
    (get-table-data db table))

  ; gather table initial values
  (define row-values (get-table-values db table primary-id primary-value))

  (define (show-dialog)
    (send dialog-frame show #t))

  (define (hide-dialog)
    (send dialog-frame show #f))

  (define dialog-width 320)
  (define dialog-height 100)

  (define-values (screen-width
                  screen-height)
    (get-display-size))
  
  (define dialog-frame  (new dialog%
                             [label title]
                             [width 320]
                             [height 100]
                             [x (- (/ screen-width 2) (/ dialog-width 2))]
                             [y (- (/ screen-height 2) (/ dialog-height 2))]))

  ; convert column names to human-readable text-field labels
  (define control-labels (map column->label column-names))
  
  ; generate one text field for each table column and fill it with the initial value
  (define control-list
    (map (λ (label value) (make-text-field dialog-frame label value)) control-labels row-values))

  (define button-panel  (new horizontal-pane%
                             [parent dialog-frame]
                             [alignment (list 'right 'bottom)]))

  (define cancel-button (new button%
                             [label "Cancel"]
                             [parent button-panel]
                             [vert-margin 5]
                             [callback (λ (b e) (hide-dialog))]))

  (define results #f)
  (define ok-button     (new button%
                             [label "OK"]
                             [parent button-panel]
                             [style (list 'border)]
                             [vert-margin 5]
                             [horiz-margin 10]
                             [callback (λ (b e)
                                         (begin (set! results
                                                      (map (λ (c) (send c get-value)) control-list))
                                                (hide-dialog)))]))

  (show-dialog)
  results)

;;; main

(create-table-editor appname "" my-db "my-table" "my-primary-id" "my-primary-value")

; EOF
