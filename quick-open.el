;;;;
;;;; quick-open.el
;;;; -------------
;;;; Created by Michael Dickens on 2013-06-21.
;;;; 
;;;; A utility for quickly finding and opening a file.
;;;;
;;;; IDO's ido-find-file is way better than this, so just use that
;;;; instead.
;;;; 

;; TODO: Byte-compile this into a .elc to make it run faster
;; TODO: Optimize this to make it run faster
;; TODO: Tab completion
;; TODO: Add feature where if there are multiple hits, you can type in
;; part of the file path for the hit you want
;; TODO: Recognize identical file paths
;; TODO: Research indexing and make quick-open's indexing more efficient

;; Change this to whatever path to `data` you want to use.
(defconst *quick-open-path* "~/.emacs.d/mdickens/quick-open/data")

;;;; 
;;;; General helper functions
;;;; 

;;; From http://www.emacswiki.org/emacs/ElispCookbook
(defun read-file (filename)
  "Read the contents of a file and return as a string."
  (if (> (length filename) 100)
      (error "File name is too long"))
  (with-current-buffer (find-file-noselect filename)
    (buffer-string)))

(defun read-list-from-file (filename divider)
  (remove "" (split-string (read-file filename) divider)))

;;; From http://www.emacswiki.org/emacs/ElispCookbook
(defun write-string-to-file (string filename)
   (with-temp-file filename
     (insert string)))

(defun line-to-file (line filename)
  (write-string-to-file
   (reduce (lambda (str x) (concat str x "\n"))
	   (nconc (read-list-from-file filename "\n") (list line))
	   :initial-value "")
   filename))
 
(defun make-path (dir filename)
  (if (equal "/" (substring dir -1 (length dir)))
      (concat dir filename)
      (concat dir "/" filename)))

(defun split-path-and-filename (path)
  "Given a path, returns a pair containing the path minus the filename
  and the filename itself."
  (let ((index (search "/" path :from-end t)))
    (list (substring path 0 index)
          (substring path (1+ index)))))


(defun join-pairs-in-list (xs)
  "Given a list xs, joins each two consecutive elements into a
  list. If xs has an odd number of elements, joins the last element to
  nil.

This sometimes needs to deal with really long lists. It's not
  recursive because (non-tail-) recursion gobbles up too much memory."
  (let ((res nil)
        (cur nil))
    (dolist (x xs)
      (cond
       ((or (null cur)
	    (= (length cur) 1))
	(push x cur))
       (t
        (if (null res)
          (setq res (list cur))
	  (push cur res))
        (setq cur (list x)))))
    (nreverse 
     (mapcar #'nreverse
      (cond
       ((null cur) res)
       ((= (length cur) 1) (cons (cons nil cur) res))
       (t (cons cur res)))))))

(defun traverse-dir (path-outer fun &optional condition)
  "Traverses the given directory and calls (fun) on each path-file
pair. If condition is given, only traverses directories such that
  (condition curr) returns non-nil."
  (cl-labels 
      ((rec (prev-path curr)
        (let ((path (make-path prev-path curr)))
	  (cond
	   ((or (string= curr ".")
		(string= curr "..")
		;; TODO: this never actually calls condition
		;; (and (functionp condition)
		;;      (not (funcall condition curr))))
		)
	    nil)
	   ((file-directory-p path)
	    (dolist (x (directory-files path))
	      (rec path x)))
	   ((file-exists-p path) (funcall fun prev-path curr))
	   (t nil)))))
    (rec path-outer "")))


;;;;
;;;; Global variables and constants
;;;; 


;; It would be awesome if Emacs Lisp supported lexical scoping by
;; default so I didn't have to use globals.

(defconst *seconds-per-day* 86400)

(defconst *quick-open-path-divider* "\n"
  "Divides the path and filename in the file database.")
(defconst *quick-open-file-db-name* 
  (make-path *quick-open-path* "file-db.txt")
  "Stores paths and filenames by placing a paired path and filename on
adjacent lines and placing each pair on a new line.")

(defconst *quick-open-last-modified-name* 
  (make-path *quick-open-path* "db-last-modified.txt"))
(defconst *quick-open-paths-name*
  (make-path *quick-open-path* "paths.txt"))
(defconst *quick-open-ignore-name*
  (make-path *quick-open-path* "ignore.txt"))

(defvar *quick-open-file-db* nil 
  "A hash where the keys are filenames and the values are lists of
the absolute paths of files with those names, excluding the filename
  itself from the path.")

;; For debugging, when opening this file more than once
(setq *quick-open-file-db* nil)


;;;; 
;;;; Primary functions
;;;; 

(defun quick-open-get-paths ()
  "Returns a list of directories in which to search for filenames."
  (read-list-from-file *quick-open-paths-name* "\n"))

(defun quick-open-add-path (path)
  (interactive "sDirectory path: ")
  (add-line-to-file path *quick-open-paths-name*))

(defun quick-open-get-ignore ()
  "Returns a list of regular expressions giving files to ignore."
  (read-list-from-file *quick-open-ignore-name* "\n"))

(defun quick-open-add-ignore (name)
  (interactive "sDirectory or file name: ")
  (add-line-to-file name *quick-open-ignore-name*))

(defun make-path-for-db (path filename)
  (concat path *quick-open-path-divider* filename))

(defun quick-open-init-db ()
  (when (null *quick-open-file-db*)
    (setq *quick-open-file-db* (make-hash-table :test 'equal))
    (dolist (pair (join-pairs-in-list 
		   (read-list-from-file *quick-open-file-db-name*
					"\n"))) 
      (when (and (car pair) (cadr pair))
	(quick-open-add-file-to-db (car pair)
				   (cadr pair))))))

(defun quick-open-write-db ()
  (when (not (null *quick-open-file-db*))
    (with-temp-file *quick-open-file-db-name*
      (maphash 
       (lambda (filename paths)
         (dolist (path paths)
           (insert (make-path-for-db path filename))
           (insert "\n")))
       *quick-open-file-db*)
      nil)))

(defun quick-open-db-query (filename)
  (gethash filename *quick-open-file-db*))

(defun quick-open-add-file-to-db (path filename)
  (let ((path-list (gethash filename *quick-open-file-db*)))
    (unless (member path path-list)
      (puthash filename (cons path path-list)
               *quick-open-file-db*))))

(defun quick-open-remove-file-from-db (path filename)
  (let ((path-list (gethash filename *quick-open-file-db*)))
    (if (member path path-list)
        (puthash filename (remove path path-list)
                 *quick-open-file-db*))))

;; TODO: Make this more sophisticated, where the user can write
;; TODO: Don't re-open the ignore file every time this is called.
(defun* quick-open-index-condition (filename)
  "Ignores any files or directories in the ignore file. Returns t if
there are no matches and nil if there is a match (i.e. the return
value indicates whether to read the file)."
  (not (member filename (quick-open-get-ignore))))

(defun quick-open-index-dir (path)
  "Index the given directory."
  (traverse-dir path #'quick-open-add-file-to-db
		#'quick-open-index-condition))

;; TODO: If a directory hasn't been modified, do not traverse it.
(defun quick-open-index-directories ()
  "Index the directories in the directory list."
  (let ((mtime (string-to-number 
                (read-file *quick-open-last-modified-name*))))
    (message "Indexing directories. This could take a while...")
    (dolist (dir (quick-open-get-paths))
      (quick-open-index-dir dir))
    (write-string-to-file 
     (format "%f" (float-time))
     *quick-open-last-modified-name*)
    (message "Done.")))


(defun quick-open-search (filename) 
  "Deprecated"
  (interactive "sQuick open: ")
  (let ((paths nil))
    (dolist (dir (quick-open-get-paths))
      (traverse-dir dir
       (lambda (path file) 
         (push (make-path path file) paths))))
    (cond
     ((= (length paths) 1)
      (find-file (car paths)))
     ((null paths)
      (print (format "File %s not found" filename)))
     (t
      ;; TODO: prompt the user to choose a file from the list
      (print (format "Multiple possible files found: %s" paths))))))

(defun quick-open-file-not-found (filename)
  (let 
   ((response 
    (read-from-minibuffer 
     ;; TODO: only search once. Don't loop.
    (format 
     "File %s not found. Would you like to run a search for it? [y/N] "
     filename))))
    (when (string= (downcase response) "y")
      (quick-open-index-directories)
      (quick-open filename))))

(defun quick-open-find-file (path filename)
  "Tries to open the given file. If it is not found, calls
quick-open-file-not-found."
  (let ((full-filename (make-path path filename)))
    (if (file-exists-p full-filename)
        (find-file full-filename)
      (quick-open-remove-file-from-db path filename)
      (message "File not found."))))

(defun quick-open-choose-file (paths filename)
  "Given a list of paths and a shared filename, prompt the user to choose
  which file to open."
  (let* ((len (length paths))
	 (numbered-file-list
	  (concat
	   (apply #'concat
		  "Multiple possible files found:\n"
		  (let ((n 0))
		    (mapcar
		     (lambda (path) 
		       (format "%d: %s\n" (incf n) 
			       (make-path path filename)))
		     paths)))))
	 (filenum 
	  (string-to-number 
	   (read-from-minibuffer
	    (concat numbered-file-list
		    (format "\nWhich would you like to open? [1-%d] "
			    len))))))
    (while (or (= filenum 0) (> filenum len))
      (setq filenum
	(string-to-number 
	 (read-from-minibuffer 
	  (concat numbered-file-list 
		  "\nInvalid input. Please try again: ")))))
    (quick-open-find-file (nth (1- filenum) paths) filename)))

(defun quick-open (filename)
  (interactive "sQuick open: ")
  (quick-open-init-db)
  (let ((paths (quick-open-db-query filename)))
    (cond
     ((= (length paths) 1)
      (quick-open-find-file (car paths) filename))
     ((null paths)
      (quick-open-file-not-found filename))
     (t
      (quick-open-choose-file paths filename)))))

(defun quick-open-wrapup ()
  (quick-open-write-db))

;;;; 
;;;; Initialization routines
;;;;

;; When a file is opened, add it to the file db.
;; TODO: Test this.
(add-hook 'find-file-hook
          (lambda ()
            (quick-open-init-db)
            (apply 'quick-open-add-file-to-db
             (split-path-and-filename (buffer-file-name)))))

;; Evaluate (quick-open-wrapup) when Emacs quits.
(add-hook 'kill-emacs-hook 'quick-open-wrapup)
