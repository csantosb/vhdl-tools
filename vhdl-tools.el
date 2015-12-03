;; ;;; vhdl-tools.el --- Jumps to definition of signal, constant or function

;; Copyright (C) 2003  Free Software Foundation, Inc.

;; Author:  wandad guscheh <wandad.guscheh@fh-hagenberg.at>
;; Keywords: vhdl

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Usage: Open any vhdl file and invoke vhdl-goto-type-def with key sequence (\C-c\C-s by default).
;; Cursor will jump to corresponding definition if there is one. Functions searches packages
;; too. If no buffer with package is open, functions asks for location of package.

;; To get back to the start of the search, press \C-x\C-x if corresponding definition has been found
;; in the same file, \C-x b RET if the search has jumped to another buffer.

;; Functions works for signals, constants, types, subtypes, components and subprograms.
;; Works only well for vhdl files with more or less correct syntax. Finds also signals in entity definition.

;; Also have a look at customization possibilities with \M-x customize-group vhdl-tools. Change option
;; use-ido-find-file to nil if ido-find-file is not installed on your system.

;; If you have any suggestions or found any bugs please mail me at <wandad.guscheh@fh-hagenberg.at>.

;;; Code:

(defgroup vhdl-tools nil "Some customizations of vhdl-tools packages" :group 'local)

(defcustom vhdl-tools-allowed-chars-in-signal "a-z0-9A-Z_"
  "Regexp with allowed characters in signal, constant or function.
Needed to determine end of name."
  :type 'string :group 'vhdl-tools)

(defun vhdl-tools-get-name (&optional dont-downcase)
  "Extract word at current position DONT-DOWNCASE.
To determine end of word, vhdl-tools-allowed-chars-in-signal is used."
  (thing-at-point 'symbol t))

(defun vhdl-tools-get-buffer (entity-or-package-name)
  "Return buffer where ENTITY-OR-PACKAGE-NAME is found."
  (save-excursion
    (let ((thisfile (format "%s.vhd" entity-or-package-name)))
      ;; if open buffer exists, return it
      (if (get-buffer thisfile)
	  (get-buffer thisfile)
	;; if file exist, open it and return buffer
	(if (file-exists-p thisfile)
	    (progn
	      (find-file-noselect thisfile)
	      (get-buffer thisfile))
	  ;; search over all existing buffers
	  (let ((current-buffer-list (buffer-list))
		(counter 0)
		found)
	    ;; loop over all buffers
	    (while (and (nth counter current-buffer-list)
			(not found))
	      (set-buffer (nth counter current-buffer-list))
	      (if (equal entity-or-package-name (vhdl-tools-get-entity-or-package-name))
		  (setq found t)
		(setq counter (1+ counter))))
	    (if found
		(nth counter current-buffer-list)
	      nil)))))))

(defun vhdl-tools-get-entity-or-package-name ()
  "Return name of entity / package or empty string if nothing found."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^ *\\(entity\\|package\\) +" nil t nil)
        (vhdl-tools-get-name)
      "")))

(defun vhdl-tools-get-entity-name-of-architecture()
  "Search for architecture and return its entity or empty string if nothing found."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "\\(^\\)\\s-*architecture\\s-+[a-zA-Z0-9_]+\\s-+of\\s-+" nil t nil)
        (vhdl-tools-get-name)
      "")))

(defun vhdl-tools-package-names ()
  "Return a list of strings of all used packages or nil if nothing found.
Only use the form work.NAME.something."
  (save-excursion
    (let ((packages '()))
      (goto-char (point-min))
      (while (re-search-forward "^ *use  *work\." nil t nil)
        (forward-char)
        (push (vhdl-tools-get-name) packages))
      ;; REFACTOR: Remove ??
      (if (vhdl-tools-set-entity-of-arch)
	  (while (re-search-forward "^ *use  *work\." nil t nil)
	    (forward-char)
	    (push (vhdl-tools-get-name) packages)))
      packages)))

(defun vhdl-tools-set-entity-of-arch ()
  "."
  (let ((package-buffer))
    (if (equal (vhdl-tools-get-entity-or-package-name) "")
	tototiti
      ;; (if (setq package-buffer (vhdl-tools-get-buffer (vhdl-tools-get-entity-name-of-architecture)))
      ;;     (progn
      ;;       (set-buffer package-buffer)
      ;;       (goto-char (point-min)))
      ;;   (if (setq package-buffer
      ;; 	    (vhdl-tools-ask-for-package (concat
      ;; 					 (vhdl-tools-get-entity-name-of-architecture)
      ;; 					 " entity file")))
      ;;       (progn
      ;;         (set-buffer package-buffer)
      ;;         (goto-char (point-min)))))
      )
    (if package-buffer t nil)))

(defun vhdl-tools-ask-for-package (package-name)
  "Given PACKAGE-NAME, return its buffer.
Assumes package is contained in a file with same name; when no buffer exists,
open corresponding file; when no file is found, ask user where to find it."
  ;; When file exists in current dir, open it so that its buffer is available
  dskfljsfd
  dfsklj
  (if (file-exists-p (format "%s.vhd" package-name))
      (find-file-noselect (format "%s.vhd" package-name))
    ;; otherwise, ask user for it and open, no selecting it
    (find-file-noselect (read-file-name (format "Where is %s ? " package-name)))))

(defun vhdl-tools-process-file (name)
  "Search within a package or a vhdl file for NAME.
Test if it is a type definition or not."
  (let ((found nil)
	should-be-in-entity
	beginning-of-entity-port
	end-of-entity
	end-of-entity-port
	apoint
	(current-pos (point)))
    (save-excursion
      (goto-char (point-min))
      ;; search for entity ... is line
      (setq beginning-of-entity-port
	    (re-search-forward
	     (concat "^[ \t]*entity[ \n\t]+[" vhdl-tools-allowed-chars-in-signal "]+[ \n\t]+is") nil t nil))
      (if beginning-of-entity-port
          (progn
            (setq end-of-entity (save-excursion (re-search-forward "^[ \t]*end")))
            (re-search-forward "port[ \n\t]*(" nil t nil)
            (setq end-of-entity-port (progn (up-list) (point)))
            (goto-char (point-min))
            (setq should-be-in-entity (re-search-forward (concat " +" name "[ \n\t]+") nil t nil))
            (if (and should-be-in-entity
		     (< beginning-of-entity-port should-be-in-entity)
		     (> end-of-entity-port should-be-in-entity)
                     (< (save-excursion (re-search-forward ":" nil t nil))
			(save-excursion (re-search-forward "\n" nil t nil)))
                     (< (point)
			(save-excursion (re-search-forward ":" nil t nil)))
                     (< end-of-entity-port
			end-of-entity))
                (setq found (point)))))
      (goto-char (point-min))
      (while (and (not found)
		  (re-search-forward "^ *\\(component\\|function\\|procedure\\|constant\\|file\\|type\\|subtype\\)[ \n\t]+" nil t nil))
        (if (equal name (vhdl-tools-get-name))
            (setq found (point))))
      (goto-char (point-min))
      (while (and (not found)
		  (re-search-forward "^[ \t]*signal[ \n\t]+" nil t nil))
        (if (equal name (vhdl-tools-get-name))
            (setq found (point))
          (while (> (save-excursion (search-forward ":" nil t nil))
		    (if (setq apoint (save-excursion (search-forward "," nil t nil))) apoint 0))
            (search-forward "," nil t nil)
            (if (equal name (vhdl-tools-get-name))
                (setq found (point)))))))
    (if found found nil)))

(defun vhdl-goto-type-def ()
  "Read word at point and try to find corresponding signal or type definition.
This function first tries to find a signal or type definition in the buffer from
where the function have been called.  It can only jump to signal, constant,
type and subtype definitions.  Works also for signals in an entity (in and out
ports, function will then jump to the entity).  To go back to the point where
the function has been called press.  If there was nothing found, it reads the
packages used, and works through all opened buffers to find packages used in
the vhdl file.  If a definition has been found in a package, package will be
displayed.  To go back to original vhdl file press."
  (interactive)
  (setq current-pos (point))
  ;; check if found definition in calling file
  (if (not (setq found (vhdl-tools-process-file (vhdl-tools-get-name))))
      ;; no definition found in calling file found
      (let ((to-search-for (vhdl-tools-get-name))
	    (package-list (vhdl-tools-package-names))
            (counter 0)
	    found
	    package-buffer
	    (to-open-packages '()))
	;; loop over all packages _____________________________________
        (while (and (not found)
		    (nth counter package-list))
          (setq package-buffer (vhdl-tools-get-buffer (nth counter package-list)))
          (if (not package-buffer)
	      ;; package not open: open it afterwards
              (setq to-open-packages (append (list (nth counter package-list)) to-open-packages))
	    ;; package open: search here
	    (save-excursion
	      (set-buffer package-buffer)
	      (setq found (vhdl-tools-process-file to-search-for))))
	  (setq counter (1+ counter)))
	;; loop over ____________________________________________________
        (setq counter 0)
        (if (not found)
            (save-excursion
	      ;; REFACTOR: Remove ??
              (if (vhdl-tools-set-entity-of-arch)
                  (progn
		    ;; TODO: Never executed
		    fdklsjsdfj
                    (setq found (vhdl-tools-process-file to-search-for))
                    (setq package-buffer (current-buffer))))))
	;;
        (while (and (not found)
		    (nth counter to-open-packages))
	  dfskjsdlfkkfds
          (if (setq package-buffer (vhdl-tools-ask-for-package (nth counter to-open-packages)))
              (save-excursion
		(set-buffer package-buffer)
		(setq found (vhdl-tools-process-file to-search-for))))
	  (setq counter (1+ counter)))
        (if found
            (progn
              (switch-to-buffer package-buffer)
              (goto-char found)
	      (back-to-indentation)
	      (recenter-top-bottom))
          (message "sorry, no corresponding definition found")))
    ;; found in current file
    (progn
      (push-mark current-pos t nil)
      (goto-char found)
      (back-to-indentation)
      (recenter-top-bottom))))

(provide 'vhdl-tools)
;;; vhdl-tools.el ends here
