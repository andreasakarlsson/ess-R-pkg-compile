;;; ess-R-pkg-compile.el ---
;;
;; Filename: ess-R-pkg-compile.el
;; Description: Facilitates compilation of R packages from Emacs.
;; Author: Andreas Karlsson
;; Maintainer: Andreas Karlsson
;; Created: fre jul 14 12:16:42 2017 (+0200)
;; Version: 0.1
;; Package-Requires: ((emacs "25")) (ess)
;; Last-Updated:
;;           By:
;;     Update #: 822
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;  Compiling is a repetitive task when developing an R package.  By using the
;;  post compilation hook and reasonable assumptions on the users next action
;;  some of that repetitiveness can be taken away.  The functionality provided
;;  here does not attempt to replace the versatility of your command line.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TODO:
;; 1. DONE: Soft detach() and then library() in existing *R* process if fail; suggest to restart R process
;;   a. pure find R process function (do this only ones then pass variable)
;;   b. provide wrapper for (compile) including post-compilation hook
;;   c. detach and load R package function
;;   e  detach dependencies
;;   f. if detaching fails suggest to restart R process
;; 2. DONE: handle multiple R processes
;; 3  setting/option to restore workspace on restart of R process
;; 4. md5-checks on header files in src folder (alist "/src/.*\\.h$" etc) if changes append "--preclean"
;; Compare and inspiration from devtools:
;; https://cran.r-project.org/web/packages/devtools/news.html
;; See the function ess-load-library in:
;; /home/andkar/.emacs.d/elpa/ess-20170710.118/lisp/ess-r-mode.el
;; See how to make a melpa package:
;; https://github.com/melpa/melpa
;;
;; This seems very useful:
;; ess-build-eval-command is a compiled Lisp function in ‘ess-inf.el’.
;;
;; (ess-build-eval-command STRING &optional VISIBLY OUTPUT FILE &rest ARGS)
;;
;; Format an evaluation command.
;; Wrap STRING with ‘ess-quote-special-chars’ and dispatch on the
;; dialect-specific ‘ess-build-eval-command’ function and
;; ‘ess-eval-command’, in that order. If none of the above is
;; defined, return nil.
;;
;; Use ‘ess-defmethod’ to define dialect specific overrides.
;;
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'ess-utils)
(defvar ess-R-pkg-compile--buffer-kill-time
  nil
  "Seconds after which the buffer from a successful compilation
  is killed. Nil will result in buffer not being killed.")

;; subfunction used below
(defun extract-R-process-buffer-name (r-process)
  "Return name of R buffers from *R* processes."
  (buffer-name (process-buffer
		(get-process (car r-process)))))

;;;*;;; R Process Detection

(defun find-R-process ()
  "Return *R* process."
  "First check if there is a *R* process associated with the
buffer, otherwise check if there is a single *R* process, if there are more
than one let the user choose."
  (update-ess-process-name-list)
  ;; take the associate *R* process
  (if ess-current-process-name
      (extract-R-process-buffer-name
       (list ess-current-process-name))
    ;; take the only available *R* process
    (if  (= (length ess-process-name-list) 1)
	(extract-R-process-buffer-name (car ess-process-name-list))
      ;; let the user choose if more than one *R* process
      (if (>= (length ess-process-name-list) 2)
	  (completing-read "For which R process are you building your package?: "
			   (mapcar 'extract-R-process-buffer-name
				   ess-process-name-list))
	;; or create a new
	;; (let (ess-ask-for-ess-directory nil)
	;; 	(inferior-ess-same-window nil)
	;; 	(ess-gen-proc-buffer-name-function (lambda (nm) "*R:pkg-compile*"))
	;; 	(R)
	;; 	(return "*R:pkg-compile*"))
	(progn (ess-switch-process)
	       (update-ess-process-name-list)
	       (extract-R-process-buffer-name (car ess-process-name-list)))
	;; (and (print "No *R* process was identified.") nil)
	)))) ;; alt. use ess-switch-process


(defun unload-R-package (R-buffer pkg)
  "Detaches the R package.
R-BUFFER is the active *R* process and PKG is the package name."
  (with-current-buffer R-buffer
    (progn (ess-eval-linewise (format "%s%s%s" "detach(\"package:" pkg "\", unload=TRUE)"))
	   (display-buffer R-buffer))))

(defun load-R-package (R-buffer pkg)
  "Load the R package.
R-BUFFER is the active *R* process and PKG is the package name."
  (with-current-buffer R-buffer
    (progn (ess-eval-linewise (format "%s%s%s" "require(\"" pkg "\")"))
	   (display-buffer R-buffer))))

(defun check-if-error (r-buffer)
  "Search for error.
Look in *R* process buffer for an error between detach and end of buffer."
  (and (string-match "Error"
		     (with-current-buffer r-buffer
		       (progn
			 (goto-char (point-max))
			 (buffer-substring-no-properties
			  (re-search-backward "> detach\\(.*\\)" nil t)
			  (point-max))))) t))
(defun check-dependency (r-buffer)
  "Simple parsing of the R-BUFFER for depending packages.  Return name of dependant package if any otherwise nil."
  (let* ((str (with-current-buffer r-buffer
		       (progn
			 (goto-char (point-max))
			 (buffer-substring-no-properties
			  (re-search-backward "> detach\\(.*\\)" nil t)
			  (point-max))))))
    (when (string-match "is required by ‘\\(.*\\)’ so will not be detached" str)
      (match-string 1 str))))


(defun restart-R-process ()
  "Find R process and offer to kill and then restart it."
  (interactive)
  (let ((R-buffer (find-R-process)))
    (if (get-buffer-process R-buffer)
	(when (kill-buffer R-buffer)
	  ;;(setq ess-dialect "R")
	  (let ((ess-ask-for-ess-directory nil)
		(inferior-ess-same-window nil)
		(ess-gen-proc-buffer-name-function (lambda (nm) R-buffer)))
	    (R)))
      (message "No R-process detected"))))

(defun delete-compilation-window (close-time)
  "Time until the compilation window closes after a successful build."
  (when close-time
    (let ((win  (get-buffer-window buf 'visible)))
      (when win (progn (sit-for close-time) (delete-window win))))))


(defmacro post-compilation-macro (r-buffer pkg)
  "Create a R-BUFFER and PKG specific function for the 'compilation-finish-functions' hook."
`(defun post-compilation (buf strg)
  "Useful things to do in the compilation BUF after compiling an R package.
  Tries to unload R package or restarts *R* before loading the R package.  It also closes the compilation buffer if sucessful."
  (delete-compilation-window ess-R-pkg-compile--buffer-kill-time)
  (unload-R-package ,r-buffer ,pkg)
  (sit-for 0.2)
  (when (check-if-error ,r-buffer)
    (while (check-dependency ,r-buffer)
      (unload-R-package ,r-buffer (check-dependency ,r-buffer)))
    (unload-R-package ,r-buffer ,pkg))
  (when (check-if-error ,r-buffer)
    (restart-R-process))
  (sit-for 0.2)
  (load-R-package ,r-buffer ,pkg)
  ;; Only want explicit use of this; hence remove hook each time
  (remove-hook 'compilation-finish-functions 'post-compilation)
  (pop-to-buffer ,r-buffer)
  (print strg)))


(defun ess-R-pkg-compile--compile (compile-str path pkg)
  "Wrapper of compile and a post compilation hook.
Where COMPILE-STR is the compilation command e.g. \"R CMD
INSTALL\", PATH is the path to the folder where the package is
located excluding the package name, PKG is the name of your
package."
  (setq ess-R-pkg-compile--current-package pkg)
  ;; This became uglier then I had hoped: I rewrote the post-compilation fun as
  ;; a macro to avoid making a global variable. However the function generated
  ;; by the macro does not see the pkg variable. This could perhaps be solved
  ;; with some cleaver quoting and evaluation. Otherwise I should switch back to
  ;; the function to keep thins simple.
  (post-compilation-macro (find-R-process) ess-R-pkg-compile--current-package)
  ;; N.b. the hook will remove itself
  (add-hook 'compilation-finish-functions 'post-compilation)

  (compile (format "%s %s"
		   compile-str (concat (file-name-as-directory path)
				       ess-R-pkg-compile--current-package))))

(provide 'ess-R-pkg-compile)

 ; Local variables section

;;; This file is automatically placed in Outline minor mode.
;;; The file is structured as follows:
;;; Chapters:     ^L ;
;;; Sections:    ;;*;;
;;; Subsections: ;;;*;;;
;;; Components:  defuns, defvars, defconsts
;;;              Random code beginning with a ;;;;* comment

;;; Local variables:
;;; mode: emacs-lisp
;;; outline-minor-mode: nil
;;; mode: outline-minor
;;; outline-regexp: "\^L\\|\\`;\\|;;\\*\\|;;;\\*\\|(def[cvu]\\|(setq\\|;;;;\\*"
;;; End:

;;; ess-R-pkg-compile.el ends here
