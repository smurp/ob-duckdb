;;; ob-duckdb.el --- Babel Functions for DuckDB Databases -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2024 Free Software Foundation, Inc.

;; Author: Shawn Murphy <smurp@smurp.com>
;; Keywords: literate programming, reproducible research
;; URL: https://github.com/smurp/ob-duckdb
;; Based on ob-sqlite.el by Eric Schulte and Nick Savage

;; ;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating duckdb source code.

;;; Code:

(require 'org-macs)
(org-assert-version)

(require 'ob)
(require 'ob-sql)

(declare-function org-table-convert-region "org-table"
		  (beg0 end0 &optional separator))
(declare-function orgtbl-to-csv "org-table" (table params))
(declare-function org-table-to-lisp "org-table" (&optional txt))

(defvar org-babel-default-header-args:duckdb '())

(defvar org-babel-header-args:duckdb
  '((db        . :any)
    (header    . :any)
    (echo      . :any)
    (bail      . :any)
    (csv       . :any)
    (column    . :any)
    (html      . :any)
    (line      . :any)
    (list      . :any)
    (separator . :any)
    (nullvalue . :any))
  "DuckDB specific header args.") ;; TODO update from the SQLite orig

(defun org-babel-expand-body:duckdb (body params)
  "Expand BODY according to the values of PARAMS."
  (let ((prologue (cdr (assq :prologue params)))
	(epilogue (cdr (assq :epilogue params))))
    (mapconcat 'identity
               (list
                prologue
                (org-babel-sql-expand-vars
                 body (org-babel--get-vars params) t)
                epilogue)
               "\n")))

(defvar org-babel-duckdb-command "duckdb")

(defun org-babel-execute:duckdb (body params)
  "Execute Duckdb BODY according to PARAMS.
This function is called by `org-babel-execute-src-block'."
  (let ((result-params (split-string (or (cdr (assq :results params)) "")))
	(db (cdr (assq :db params)))
	(separator (cdr (assq :separator params)))
	(nullvalue (cdr (assq :nullvalue params)))
	(headers-p (equal "yes" (cdr (assq :colnames params))))
	(others (delq nil (mapcar
			   (lambda (arg) (car (assq arg params)))
			   (list :header :echo :bail :column
				 :csv :html :line :list)))))
    (with-temp-buffer
      (insert
       (org-babel-eval
	(org-fill-template
	 "%cmd %header %separator %nullvalue %others %csv %db "
	 (list
	  (cons "cmd" org-babel-duckdb-command)
	  (cons "header" (if headers-p "-header" "-noheader"))
	  (cons "separator"
		(if separator (format "-separator %s" separator) ""))
	  (cons "nullvalue"
		(if nullvalue (format "-nullvalue %s" nullvalue) ""))
	  (cons "others"
		(mapconcat
		 (lambda (arg) (format "-%s" (substring (symbol-name arg) 1)))
		 others " "))
	  ;; for easy table parsing, default header type should be -csv
	  (cons "csv" (if (or (member :csv others) (member :column others)
			      (member :line others) (member :list others)
			      (member :html others) separator)
			  ""
			"-csv"))
          (cons "db" (or db ""))))
	;; body of the code block
	(org-babel-expand-body:duckdb body params)))
      (org-babel-result-cond result-params
	(buffer-string)
	(if (equal (point-min) (point-max))
	    ""
	  (org-table-convert-region (point-min) (point-max)
				    (if (or (member :csv others)
					    (member :column others)
					    (member :line others)
					    (member :list others)
					    (member :html others) separator)
					nil
				      '(4)))
	  (org-babel-duckdb-table-or-scalar
	   (org-babel-duckdb-offset-colnames
	    (org-table-to-lisp) headers-p)))))))

(defun org-babel-duckdb-expand-vars (body vars)
  "Expand the variables held in VARS in BODY."
  (declare (obsolete "use `org-babel-sql-expand-vars' instead." "9.5"))
  (org-babel-sql-expand-vars body vars t))

(defun org-babel-duckdb-table-or-scalar (result)
  "Cleanup cells in the RESULT table.
If RESULT is a trivial 1x1 table, then unwrap it."
  (if (and (equal 1 (length result))
	   (equal 1 (length (car result))))
      (org-babel-read (caar result) t)
    (mapcar (lambda (row)
	      (if (eq 'hline row)
		  'hline
		(mapcar #'org-babel-duckdb--read-cell row)))
	    result)))

(defun org-babel-duckdb-offset-colnames (table headers-p)
  "If HEADERS-P is non-nil then offset the first row as column names in TABLE."
  (if headers-p
      (cons (car table) (cons 'hline (cdr table)))
    table))

(defun org-babel-prep-session:duckdb (_session _params)
  "Raise an error because support for Duckdb sessions isn't implemented.
Prepare SESSION according to the header arguments specified in PARAMS."
  (error "Duckdb sessions not yet implemented"))

(defun org-babel-duckdb--read-cell (cell)
  "Process CELL to remove unnecessary characters."
  (org-babel-read cell t))

(provide 'ob-duckdb)

;;; ob-duckdb.el ends here
