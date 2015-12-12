;;; vhdl-tools.el --- Utilities to navigate vhdl sources

;; Based on `vhdl-goto-def'

;; Copyright (C) 2003 Free Software Foundation, Inc.
;; Copyright (C) 2015 Cayetano Santos

;; Original author:  wandad guscheh <wandad.guscheh@fh-hagenberg.at>
;; Author:           Cayetano Santos
;; Keywords: vhdl

;; Filename:
;; Description:
;; URL: https://github.com/emacs-helm/helm-recoll
;; Keywords: convenience
;; Compatibility: GNU Emacs >= 24.3
;; Version: 0.2
;; Package-Requires: ((helm "1.7.7"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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


;;; Code:

(require 'vhdl-mode)
(require 'ggtags)
(require 'outline)

;;; Variables

(defgroup vhdl-tools nil "Some customizations of vhdl-tools packages" :group 'local)

(defcustom vhdl-tools-allowed-chars-in-signal "a-z0-9A-Z_"
  "Regexp with allowed characters in signal, constant or function.
Needed to determine end of name."
  :type 'string :group 'vhdl-tools)

(defcustom vhdl-tools-outline-regexp "^\\s-*-- [*]\\{1,8\\} "
  "Regexp ...")

(defcustom vhdl-tools-imenu-regexp "^\\s-*--\\s-\\([*]\\{1,8\\}\\s-.+\\)"
  "Regexp ...")

;;; Helper

(defun vhdl-tools-push-marker ()
  ;; push tag (stolen from elisp-slime-nav.el)
  (if (fboundp 'xref-push-marker-stack)
      (xref-push-marker-stack)
    (with-no-warnings
      (ring-insert find-tag-marker-ring (point-marker))))
  (setq ggtags-tag-ring-index nil))

(defun vhdl-tools-get-name (&optional dont-downcase)
  "Extract word at current position DONT-DOWNCASE.
To determine end of word, vhdl-tools-allowed-chars-in-signal is used."
  (thing-at-point 'symbol t))

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


;;; Get definition

;;;###autoload
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

(defun vhdl-tools-package-names ()
  "Return a list of strings of all used packages or nil if nothing found.
Only use the form work.NAME.something."
  (save-excursion
    (let ((packages))
      ;; search for packages in current buffer
      (goto-char (point-min))
      (while (re-search-forward "^ *use  *work\." nil t nil)
        (forward-char)
	(when (not (member (vhdl-tools-get-name) packages))
	  (push (vhdl-tools-get-name) packages)))
      ;; search in all open buffers
      (dolist (var (buffer-list))
	(set-buffer var)
	(goto-char (point-min))
	(while (re-search-forward "^ *use  *work\." nil t nil)
	  (forward-char)
	  (when (not (member (vhdl-tools-get-name) packages))
	    (push (vhdl-tools-get-name) packages))))
      ;; search in all files in current dir
      (dolist (var (file-expand-wildcards "*.vhd"))
	(when (not (get-buffer var))
	  (find-file-noselect var))
	(set-buffer var)
	(goto-char (point-min))
	(while (re-search-forward "^ *use  *work\." nil t nil)
	  (forward-char)
	  (when (not (member (vhdl-tools-get-name) packages))
	    (push (vhdl-tools-get-name) packages))))
      packages)))

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

(defun vhdl-tools-goto-type-def ()
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
  ;; when no symbol at point, move forward to next symbol
  (vhdl-tools-push-marker)
  (when (not (vhdl-tools-get-name))
    (back-to-indentation))
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
	  (setq package-buffer
		(vhdl-tools-get-buffer (nth counter package-list)))
	  (with-current-buffer package-buffer
	    (setq found (vhdl-tools-process-file to-search-for)))
	  (setq counter (1+ counter)))
	;; loop over ____________________________________________________
	(if found
	    (progn
	      (switch-to-buffer package-buffer)
	      (goto-char found)
	      (back-to-indentation)
	      (recenter-top-bottom))
	  (message "sorry, no corresponding definition found")))
    ;; found in current file
    (progn
      (goto-char found)
      (back-to-indentation)
      (recenter-top-bottom))))


;;; Jumping

;;;; Jump into module

;;;###autoload
(defun vhdl-tools-jump-into-module()
  "When point is at an instance, jump into the module.
Additionally, move point to signal at point.
Declare a key-bind to get back to the original point."
  (interactive)
  ;; when no symbol at point, move forward to next symbol
  (when (not (vhdl-tools-get-name))
    (back-to-indentation))
  ;; when nil, do nothing
  (when (vhdl-tools-get-name)
    ;; necessary during hook (see later)
    (setq vhdl-tools-jump-into-module-name (vhdl-tools-get-name))
    (vhdl-tools-push-marker)
    (save-excursion
      ;; locate component name to jump into
      (search-backward-regexp "port map")
      (forward-line -1)
      (end-of-line)
      (backward-char 2)
      ;; empty old content in hook
      (setq ggtags-find-tag-hook nil)
      ;; update hook to execute an action
      ;; once jumped to new buffer
      (add-hook 'ggtags-find-tag-hook
		'(lambda()
		   (when (search-forward vhdl-tools-jump-into-module-name nil t)
		     (back-to-indentation)
		     (recenter-top-bottom)
		     (let ((beacon-blink-duration 1))
		       (beacon-blink))
		     ;; erase modified hook
		     (setq vhdl-tools-jump-into-module-name nil)
		     ;; erase hook
		     (setq ggtags-find-tag-hook nil))
		   ;; remove last jump so that `pop-tag-mark' will get to
		   ;; original position before jumping
		   (ring-remove find-tag-marker-ring 0)))
      ;; jump !
      (call-interactively 'ggtags-find-definition))))

;;;; Jump to first
;; Utility to jump to first time a symbol appears on file

;;;###autoload
(defun vhdl-tools-jump-first ()
  "Jump to first occurrence of symbol at point.
When no symbol at point, move point to indentation."
  (interactive)
  ;; when no symbol at point, move forward to next symbol
  (when (not (vhdl-tools-get-name))
    (back-to-indentation))
  ;; when nil, do nothing
  (when (vhdl-tools-get-name)
    (vhdl-tools-push-marker)
    (let ((vhdl-tools-jump-first-name (vhdl-tools-get-name)))
      (goto-char (point-min))
      (search-forward-regexp vhdl-tools-jump-first-name nil t)
      (back-to-indentation))))

;;;; Jump Upper
;; Utility to jump to upper level

;;;###autoload
(defun vhdl-tools-jump-upper ()
  "Get to upper level module and move point to signal at point.
When no symbol at point, move point to indentation."
  (interactive)
  ;; when no symbol at point, move forward to next symbol
  (when (not (vhdl-tools-get-name))
    (back-to-indentation))
  (let ((vhdl-tools-thing (vhdl-tools-get-name))
	(beacon-blink-duration 1))
    (vhdl-tools-push-marker)
    (save-excursion
      ;; get back to entity
      (search-backward-regexp "^entity")
      (forward-word)
      (forward-char 2)
      ;; Jump by searching with prefilling minubuffer
      (funcall `(lambda ()
		  (minibuffer-with-setup-hook
		      (lambda ()
			(insert (format ".*:.*%s$" ,(vhdl-tools-get-name))))
		    (helm-do-grep-1 '(,default-directory)))))
      ;; search, when nil, do nothing
      (when vhdl-tools-thing
	(search-forward-regexp vhdl-tools-thing nil t)
	(back-to-indentation)
	(recenter-top-bottom)))))


;;; Links
;;
;; The goal here is, using the ggtags infrastructure, to implement a mechanism to
;; follow links in comments.
;;
;; For example, in the form of =tag@tosearch=
;;
;; "TM_IO_Sequencer@Pixel"
;;
;; will get to the definition of ~TM_IO_Sequencer~, and then forward search for
;; ~Pixel~. To achieve this, I update a hook before switching buffers with
;; ~find-tag~.

;;;; Link Store

;;;###autoload
(defun vhdl-tools-store-link()
  "Store current line as a link."
  (interactive)
  (let* ((myline (save-excursion
		   (back-to-indentation)
		   (set-mark-command nil)
		   (end-of-line)
		   (buffer-substring-no-properties (region-beginning) (region-end))))
	 (myentity (save-excursion
		     (search-backward-regexp "entity")
		     (forward-word)
		     (forward-char 2)
		     (vhdl-tools-get-name)))
	 (mylink (format "%s\@%s" myentity myline)))
    (message mylink)
    (setq vhdl-tools-store-link-link mylink)))

;;;; Link Paste

;;;###autoload
(defun vhdl-tools-paste-link()
  "Paste previous stored link."
  (interactive)
  (insert (format "`%s`" vhdl-tools-store-link-link)))

;;;; Link Follow

;;;###autoload
(defun vhdl-tools-follow-links(arg)
  "Follow links in the form of Tag:ToSearch'."
  (interactive "P")
  ;; get item in the form of tag@tosearch
  (save-excursion
    (let* ((tmp-point-min (progn  ;; beginning of item
			    (search-backward-regexp "\`" )
			    (+ 1 (point))))
	   (tmp-point-max (progn ;; end of item
			    (forward-char 1)
			    (search-forward-regexp "\`" )
			    (- (point) 1)))
	   (vhdl-tools-follow-links-item ;; item
	    (buffer-substring-no-properties
	     tmp-point-min tmp-point-max)))
      ;; tag
      (setq vhdl-tools-follow-links-tag
	    (substring vhdl-tools-follow-links-item 0
		       (string-match "@" vhdl-tools-follow-links-item)))
      ;; tosearch
      (setq vhdl-tools-follow-links-tosearch
	    ;; with a prefix argument, ignore tosearch
	    (when (not (equal arg '(4)))
	      nil
	      (if (string-match "@" vhdl-tools-follow-links-item)
		  (substring
		   vhdl-tools-follow-links-item
		   (+ 1 (string-match "@" vhdl-tools-follow-links-item)) nil)
		nil)))))
  ;; when tosearch non nil, update hook to execute an action
  (when vhdl-tools-follow-links-tosearch
    ;; empty old content in hook
    (setq ggtags-find-tag-hook nil)
    (vhdl-tools-push-marker)
    ;; declare action after jumping to new buffer
    (add-hook 'ggtags-find-tag-hook
	      '(lambda()
		 ;; action: forward search
		 ;; if no tosearch is found, do nothing
		 (when (search-forward vhdl-tools-follow-links-tosearch nil t)
		   ;; otherwise, do this
		   (back-to-indentation)
		   (recenter-top-bottom)
		   (let ((beacon-blink-duration 1))
		     (beacon-blink)))
		 ;; erase modified hook
		 (setq vhdl-tools-follow-links-tosearch nil)
		 (setq ggtags-find-tag-hook nil)))
    ;; jump !
    (ggtags-find-definition vhdl-tools-follow-links-tag)))


;;; Headings

;;;; Get to next

;;;###autoload
(defun vhdl-tools-headings-next()
  "Get to next heading."
  (interactive)
  (re-search-forward outline-regexp)
  (beginning-of-line))

;;;; Get to previous

;;;###autoload
(defun vhdl-tools-headings-prev()
  "Get to previous heading."
  (interactive)
  (re-search-backward outline-regexp)
  (beginning-of-line))


;;; Helm-imenu navigation

;;;; Helper

;;;###autoload
(defun vhdl-tools-imenu-with-initial-minibuffer (str)
  (interactive)
  (funcall `(lambda ()
	      (interactive)
	      (minibuffer-with-setup-hook
		  (lambda () (insert (format "%s " ,str)))
		(call-interactively 'helm-semantic-or-imenu)))))

;;;; Standard Imenu

;;;###autoload
(defun vhdl-tools-imenu()
  (interactive)
  (let ((imenu-generic-expression vhdl-imenu-generic-expression))
    (set-buffer-modified-p t)
    (save-buffer)
    (call-interactively 'imenu)))

;;;; Instances

;;;###autoload
(defun vhdl-tools-imenu-instance()
  (interactive)
  (let ((imenu-generic-expression vhdl-imenu-generic-expression)
	(helm-imenu-delimiter " ")
	(helm-autoresize-max-height 100)
	(helm-candidate-number-limit 50))
    (set-buffer-modified-p t)
    (save-buffer)
    (vhdl-tools-imenu-with-initial-minibuffer "^Instance")))

;;;; Processes

;;;###autoload
(defun vhdl-tools-imenu-processes()
  (interactive)
  (let ((imenu-generic-expression vhdl-imenu-generic-expression)
	(helm-imenu-delimiter " ")
	(helm-autoresize-max-height 100)
	(helm-candidate-number-limit 50))
    (set-buffer-modified-p t)
    (save-buffer)
    (vhdl-tools-imenu-with-initial-minibuffer "^Process")))

;;;; Headers

;;;###autoload
(defun vhdl-tools-imenu-headers()
  (interactive)
  (let ((imenu-generic-expression `(("" ,vhdl-tools-imenu-regexp 1)))
	(helm-imenu-delimiter " ")
	(helm-autoresize-max-height 100)
	(helm-candidate-number-limit 50))
    (set-buffer-modified-p t)
    (save-buffer)
    (call-interactively 'helm-semantic-or-imenu)))

;;;; All

;;;###autoload
(defun vhdl-tools-imenu-all()
  "In a vhdl buffer, call `helm-semantic-or-imenu', show all items.
  Processes, instances and doc headers are shown in order of appearance."
  (interactive)
  (let ((imenu-generic-expression
	 `(;; process
	   ("" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\(postponed\\s-+\\|\\)process\\)" 1)
	   ;; instance
	   ("" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\s-*:\\(\\s-\\|\n\\)*\\(entity\\s-+\\(\\w\\|\\s_\\)+\\.\\)?\\(\\w\\|\\s_\\)+\\)\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map\\>" 1)
	   ("" ,vhdl-tools-imenu-regexp 1)
	   ("Subprogram" "^\\s-*\\(\\(\\(impure\\|pure\\)\\s-+\\|\\)function\\|procedure\\)\\s-+\\(\"?\\(\\w\\|\\s_\\)+\"?\\)" 4)
	   ;; ("Instance" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\s-*:\\(\\s-\\|\n\\)*\\(entity\\s-+\\(\\w\\|\\s_\\)+\\.\\)?\\(\\w\\|\\s_\\)+\\)\\(\\s-\\|\n\\)+\\(generic\\|port\\)\\s-+map\\>" 1)
	   ("Component" "^\\s-*\\(component\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)" 2)
	   ("Procedural" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(procedural\\)" 1)
	   ;; ("Process" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(\\(postponed\\s-+\\|\\)process\\)" 1)
	   ("Block" "^\\s-*\\(\\(\\w\\|\\s_\\)+\\)\\s-*:\\(\\s-\\|\n\\)*\\(block\\)" 1)
	   ("Package" "^\\s-*\\(package\\( body\\|\\)\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)" 3)
	   ("Configuration" "^\\s-*\\(configuration\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)" 2)
	   ("" "^\\s-*\\(architecture\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\s-+of\\s-+\\(\\w\\|\\s_\\)+\\)" 2)
	   ("Entity" "^\\s-*\\(entity\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)" 2)
	   ("Context" "^\\s-*\\(context\\)\\s-+\\(\\(\\w\\|\\s_\\)+\\)" 2)))
	(helm-imenu-delimiter " ")
	(helm-autoresize-max-height 100)
	(helm-candidate-number-limit 50))
    (set-buffer-modified-p t)
    (save-buffer)
    (call-interactively 'helm-semantic-or-imenu)))


;;; Minor Mode

;;;; Keybindings

(defvar vhdl-tools-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c M-D") #'vhdl-tools-goto-type-def)
    (define-key m (kbd "C-c M-j") #'vhdl-tools-follow-links)
    (define-key m (kbd "C-c M-w") #'vhdl-tools-store-link)
    (define-key m (kbd "C-c M-y") #'vhdl-tools-paste-link)
    (define-key m (kbd "C-c M-.") #'vhdl-tools-jump-into-module)
    (define-key m (kbd "C-c M-a") #'vhdl-tools-jump-first)
    (define-key m (kbd "C-c M-u") #'vhdl-tools-jump-upper)
    (define-key m (kbd "C-c C-n") #'vhdl-tools-headings-next)
    (define-key m (kbd "C-c C-h") #'vhdl-tools-headings-prev)
    ;;
    (define-prefix-command 'vhdl-tools-imenu-map)
    (define-key m (kbd "C-x c i") 'vhdl-tools-imenu-map)
    (define-key vhdl-tools-imenu-map (kbd "m") #'vhdl-tools-imenu)
    (define-key vhdl-tools-imenu-map (kbd "i") #'vhdl-tools-imenu-instance)
    (define-key vhdl-tools-imenu-map (kbd "p") #'vhdl-tools-imenu-processes)
    (define-key vhdl-tools-imenu-map (kbd "*") #'vhdl-tools-imenu-headers)
    (define-key vhdl-tools-imenu-map (kbd "a") #'vhdl-tools-imenu-all)
    m)
  "Keymap for `vhdl-tools'.")

;;;; Mode

;;;###autoload
(define-minor-mode vhdl-tools-mode
  "Utilities for navigating vhdl sources.

Key bindings:
\\{vhdl-tools-map}"
  :lighter " vhdl-t"
  :keymap vhdl-tools-map
  :group 'vhdl-tools
  :global nil
  (if vhdl-tools-mode
      (progn
	;; try to keep things as they were
	(setq vhdl-tools-outline-active outline-minor-mode)
	(outline-minor-mode 1)
	(setq vhdl-tools-ggtags-active ggtags-mode)
	(ggtags-mode 1)
	;; custom outline regexp
	(setq-local vhdl-tools-outline-regexp-old outline-regexp)
	(setq-local outline-regexp vhdl-tools-outline-regexp)
	;; notify
	(message "VHDL Tools enabled."))
    (progn
      ;; try to keep things as they were
      (when (not vhdl-tools-outline-active)
	(outline-minor-mode -1))
      (when (not vhdl-tools-ggtags-active)
	(ggtags-mode -1))
      ;; custom outline regexp
      (setq-local outline-regexp vhdl-tools-outline-regexp-old)
      ;; notify
      (message "VHDL Tools disabled."))))

(provide 'vhdl-tools)

;;; vhdl-tools.el ends here
