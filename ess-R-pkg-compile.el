;;; ess-R-pkg-compile.el --- builds R packages and reloads within R process
;;
;; Filename: ess-R-pkg-compile.el
;; Description: Facilitates compilation of R packages
;; Author: Andreas Karlsson
;; Maintainer: Andreas Karlsson
;; Created: fre jul 14 12:16:42 2017 (+0200)
;; Version: 0.1
;; Package-Requires: ((emacs "25") (ess "17"))
;; Last-Updated:
;;           By:
;;     Update #: 1498
;; URL: https://github.com/andreasakarlsson/ess-R-pkg-compile
;; Doc URL:
;; Keywords: tools, processes, ESS, R-package, compilation
;; Compatibility: Tested on R version 3.4.4
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
;; 1 DONE: Soft detach() and then library() in existing *R* process if fail; suggest to restart R process
;;   a. pure find R process function (do this only ones then pass variable)
;;   b. provide wrapper for (compile) including post-compilation hook
;;   c. detach and load R package function
;;   e  detach dependencies
;;   f. if detaching fails suggest to restart R process
;; 2 DONE: handle multiple R processes
;; 3 DONE: check if compilation succeeded before closing
;; + When to restart-R-process
;; + change input to only one string parse for package name, skip default folder
;; + As an option: always ask which R process ess-request-a-process
;; + Universal argument C-u for manipulating compilation string (e.g. --preclean)
;; 4 keep compilation function but:  1, allow some changes in mini-buffer 1, make a wrapper inspired by old .el code to choose
;; 5 setting/option to restore workspace on restart of R process
;; 6 md5-checks on header files in src folder (alist "/src/.*\\.h$" etc) if changes append "--preclean"
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
;;;; * Defining variables

(defcustom ess-R-pkg-compile--default-R-pkg-parent-path nil
  "String containing the default path to the parent directory of the R packages.

Used by \\[ess-R-pkg-compile]."
  :group 'ess-S
  :type 'string)

(defcustom ess-R-pkg-compile--default-compile-str
  "R CMD INSTALL"
  "String containing the default build command e.g. \"R CMD INSTALL\".

Used by \\[ess-R-pkg-compile]."
  :group 'ess-S
  :type 'string)

(defcustom ess-R-pkg-compile--assume-R-process
  t
  "Attempt to assume the intended R-process for the compilation.
Either by the process associated with the current buffer, or if
only one process exist.  If set to nil the user will always be
prompted to select R process.

Used by \\[ess-R-pkg-compile]."
  :group 'ess-S
  :type 'boolean)

(defcustom ess-R-pkg-compile--buffer-kill-time
  0
  "Define seconds after which to kill the buffer after a successful compilation.
Nil will result in buffer not being killed.

Used by \\[ess-R-pkg-compile]."
  :group 'ess-S
  :type 'number)

(defcustom ess-R-pkg-compile--build-list
  nil
  "List from which a user interactively can pick an R package build call.
Each element must be generated by ESS-R-PKG-COMPILE--MACRO and
added to ESS-R-PKG-COMPILE--BUILD-LIST.

Used by \\[ess-R-pkg-compile]."
  :group 'ess-S
  :type 'alist)

;;;; * R Process Detection

(defun ess-R-pkg-compile--extract-R-process-buffer-name (r-process)
  "Return name of R buffers from the process R-PROCESS."
  (buffer-name (process-buffer
		(get-process (car r-process)))))

(defun ess-R-pkg-compile--find-R-process ()
  "Return *R* process."
  "First check if there is a *R* process associated with the
buffer, otherwise check if there is a single *R* process, if there are more
than one let the user choose."
  (update-ess-process-name-list)
  ;; take the associate *R* process
  (if ess-current-process-name
      (ess-R-pkg-compile--extract-R-process-buffer-name
       (list ess-current-process-name))
    ;; take the only available *R* process
    (if (= (length ess-process-name-list) 1)
	(ess-R-pkg-compile--extract-R-process-buffer-name (car
	ess-process-name-list))
      ;; let the user choose if more than one *R* process
      (if (>= (length ess-process-name-list) 2)
	  (completing-read "For which R process are you building your package?: "
			   (mapcar 'ess-R-pkg-compile--extract-R-process-buffer-name
				   ess-process-name-list))
	;; or create a new
	;; (let (ess-ask-for-ess-directory nil)
	;; 	(inferior-ess-same-window nil)
	;; 	(ess-gen-proc-buffer-name-function (lambda (nm) "*R:pkg-compile*"))
	;; 	(R)
	;; 	(return "*R:pkg-compile*"))
	(progn (ess-switch-process)
	       (update-ess-process-name-list)
	       (ess-R-pkg-compile--extract-R-process-buffer-name (car
	       ess-process-name-list)))
	;; (and (print "No *R* process was identified.") nil)
	)))) ;; alt. use ess-switch-process



(defun ess-R-pkg-compile--unload-R-package (R-buffer pkg)
  "Unload R package namespace.
R-BUFFER is the active *R* process and PKG is the package name."
  (with-current-buffer R-buffer
    (progn (ess-eval-linewise
            (format "%s%s%s%s%s" "if (isNamespaceLoaded(\"" pkg
                    "\")) unloadNamespace(asNamespace(\"" pkg "\"))"))
                                      (display-buffer R-buffer))))

(defun ess-R-pkg-compile--load-R-package (R-buffer pkg)
  "Load the R package.
R-BUFFER is the active *R* process and PKG is the package name."
  (with-current-buffer R-buffer
    (progn (ess-eval-linewise (format "%s%s%s" "library(\"" pkg "\")"))
	   (display-buffer R-buffer))))

(defun ess-R-pkg-compile--check-if-error (R-buffer)
  "Parse the R-BUFFER for error message.
Look in the *R* process buffer for an error after attempting to
unload the package from the namespace."
  (and (string-match "Error"
		     (with-current-buffer R-buffer
		       (progn
			 (goto-char (point-max))
			 (buffer-substring-no-properties
                          (re-search-backward
                           (concat "^> if (isNamespaceLoaded(\""
                                   "\\([[:ascii:]]*\\)\")) "
                                   "unloadNamespace(asNamespace(\""
                                   "\\([[:ascii:]]*\\)\"))$") nil t)
			  (point-max))))) t))
(defun ess-R-pkg-compile--check-dependency (R-buffer)
  "Simple parsing of the R-BUFFER for depending packages.
Return name of dependant package if any otherwise nil."
  (let* ((str (with-current-buffer R-buffer
                (progn
                  (goto-char (point-max))
                  (buffer-substring-no-properties
                   (re-search-backward
                    (concat "^> if (isNamespaceLoaded(\""
                            "\\([[:ascii:]]*\\)\")) "
                            "unloadNamespace(asNamespace(\""
                            "\\([[:ascii:]]*\\)\"))$") nil t)
                   (point-max))))))
    (when (string-match
           (concat " package ‘\\([[:ascii:]]*\\)’ "
                   "is required by ‘\\([[:ascii:]]*\\)’ "
                   "so will not be detached") str)
    (match-string 2 str))))



(defun ess-R-pkg-compile--restart-R-process ()
  "Find R process and offer to kill and then restart it."
  (interactive)
  (let ((R-buffer (ess-R-pkg-compile--find-R-process)))
    (if (get-buffer-process R-buffer)
	(when (kill-buffer R-buffer)
	  ;;(setq ess-dialect "R")
	  (let ((ess-ask-for-ess-directory nil)
		(inferior-ess-same-window nil)
		(ess-gen-proc-buffer-name-function (lambda (nm) R-buffer)))
	    (R)))
      (message "No R-process detected"))))


(defun ess-R-pkg-compile--delete-compilation-window (buf close-time)
  "Closes the *compilation* buffer BUF after CLOSE-TIME seconds."
  (when close-time
    (let ((win  (get-buffer-window buf 'visible)))
      (when win (progn (sit-for close-time) (delete-window win))))))

(defmacro ess-R-pkg-compile--post-compilation-macro (R-buffer pkg)
  "Create a R-BUFFER and PKG specific function for the 'compilation-finish-functions' hook."
  `(defun post-compilation (buf strg)
     "Useful things to do in the compilation BUF after compiling an R package.
  Tries to unload R package or restarts *R* before loading the R
  package.  It also closes the compilation buffer if successful."
     (if (not (string-match "exited abnormally" strg))
	   ;; If compilation succeeded
	   (progn
	     (ess-R-pkg-compile--delete-compilation-window buf
	     ess-R-pkg-compile--buffer-kill-time)
	     (ess-R-pkg-compile--unload-R-package ,R-buffer ,pkg)
	     (sit-for 0.2)
	     (when (ess-R-pkg-compile--check-if-error ,R-buffer)
	       (while (ess-R-pkg-compile--check-dependency ,R-buffer)
                 (message "Attempting to resolve dependency")
		 (ess-R-pkg-compile--unload-R-package
		 ,R-buffer (ess-R-pkg-compile--check-dependency
		 ,R-buffer)))
	       (ess-R-pkg-compile--unload-R-package ,R-buffer ,pkg))
	     (when (ess-R-pkg-compile--check-if-error ,R-buffer)
	       (ess-R-pkg-compile--restart-R-process))
	     (sit-for 0.2)
	     (ess-R-pkg-compile--load-R-package ,R-buffer ,pkg)
             (pop-to-buffer ,R-buffer))
	 ;; If compilation failed
       (message "Compilation appear to have failed, ess-R-pkg-compile is aborting."))
       ;; Do this independent of compilation result
       ;; Only want explicit use of this; hence remove hook each time
       (remove-hook 'compilation-finish-functions 'post-compilation)))


(defun ess-R-pkg-compile--compile (pkg &optional parent-path compile-str)
  "Wrapper of compile and a post compilation hook.
Where PKG is the name of your package, PARENT-PATH is the path to
the folder where the package is located excluding the package
name and COMPILE-STR is the compilation command e.g. \"R CMD
INSTALL\", ."
  (let ((parent-path (or parent-path ess-R-pkg-compile--default-R-pkg-parent-path))
        (compile-str (or compile-str ess-R-pkg-compile--default-compile-str)))
  ;; Expanding macro arguments  prior to setting up the hook.
  (eval `(ess-R-pkg-compile--post-compilation-macro
  ,(ess-R-pkg-compile--find-R-process) ,pkg))

  ;; N.b. the hook will remove itself
  (add-hook 'compilation-finish-functions 'post-compilation)

  (compile (format "%s %s"
		   compile-str (concat (file-name-as-directory parent-path)
				       pkg)))))



;;;; * Package UI

(defun ess-R-pkg-compile--prefix-build-flag ()
  "This function check if the 'universal-argument' was invoked.
If so the user can enter a string with additional build flags."
  (cond
   ((equal current-prefix-arg nil) (list "")); no C-u -> no additional flag
   (t (list (read-string "Additional build flags:" )))))

(defmacro ess-R-pkg-compile--macro (pkg &optional parent-path compile-str)
  "Macro that generate a function which builds an R package.
Creates and names the interactive function
ESS-R-PKG-COMPILE--MY-PACKAGE for building and reloading
R-packages.  The first required argument PKG, should be a string
with the name of the package, which must be the same as the
package folder.  The optional string PARENT-PATH should be the
folder wherein the R package folder is located, will default to
ESS-R-PKG-COMPILE--DEFAULT-R-PKG-PARENT-PATH.  To use a package
specific build call COMPILE-STR can be set, which otherwise
defaults ESS-R-PKG-COMPILE--DEFAULT-COMPILE-STR."
  `(defun ,(intern (format "ess-R-pkg-compile--%s" pkg)) (&optional build-flag)
     "Package specific function for interactively rebuilding and
reloading your R-package.  \\<universal-argument> &
\\[ESS-R-PKG-COMPILE--MY-PACKAGE] will prompt for entering
additional BUILD-FLAG string."
     (interactive (ess-R-pkg-compile--prefix-build-flag))
     (let ((parent-path
            (or ,parent-path ess-R-pkg-compile--default-R-pkg-parent-path))
           (compile-str
            (or ,compile-str ess-R-pkg-compile--default-compile-str)))
       (ess-R-pkg-compile--compile ,pkg parent-path
                                   (format "%s %s" compile-str build-flag)))))

(defun ess-R-pkg-compile--select-build (&optional build-flag)
  "Allow the user to interactively pick an R package build call.
Each element must be generated by ESS-R-PKG-COMPILE--MACRO and
added to ESS-R-PKG-COMPILE--BUILD-LIST.  \\<universal-argument> &
\\[ESS-R-PKG-COMPILE--SELECT-BUILD] will prompt for entering
additional BUILD-FLAG string."
  (interactive (ess-R-pkg-compile--prefix-build-flag))
  (funcall (car (read-from-string
                      (completing-read
                               "Select package:" ess-R-pkg-compile--build-list)))
           build-flag))

(provide 'ess-R-pkg-compile)

;;; Local Variables:
;;; outline-regexp: ";;;; "
;;; eval:(progn (outline-minor-mode 1) (while (re-search-forward "^;;;; \\* " nil t) (outline-toggle-children)))
;;; End:

;;; ess-R-pkg-compile.el ends here
