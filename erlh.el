;;; erlh.el --- emacs environment for erlang

;; Copyright (C) 2014  Nic Ferrier

;; Created: Tue Sep 23 13:23:34 BST 2014
;; Author: Nic Ferrier <nferrier@ferrier.me.uk>
;; Keywords: languages
;; Package-Version: 0.0.1
;; Package-requires: ((dash "2.3.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I am mucking about with erlang. I found the existing emacs tooling
;; was a bit lacking for the way people are recommending doing erlang
;; these days. I am therefore updating the tooling.

;;; Code:

(require 'dash)

(defgroup erlh nil
  "A customize group for Erlang from the erlh package."
  :group 'languages)

(defcustom erlh-kerl-installs-dir "~/"
  "The root of where the kerl installations are.

If the kerl installations are in multiple places then, tough.  At
least for now.

You should set this to something specific for your kerl
installations of erlang."
  :group 'erlh
  :type 'directory)

(defcustom erlh-kerl-installs-pattern "erlang.*"
  "A pattern for matching kerl installation directories.

This is combined with `erlh-kerl-installs-dir' to find kerl
installations of erlang."
  :group 'erlh
  :type 'directory)

(defun erlh/make-script (kerl-env-dir)
  "Make a shell script to proxy many erl programs around kerl.

KERL-ENV-DIR is the directory the kerl specified environment can
be found in.  The resulting script will source kerl \"activate\" from
there.

Returns the name of the script."
  (let ((script-name (expand-file-name ".erl" default-directory)))
    (with-temp-file script-name
      (insert "#!/bin/bash
source " (file-name-as-directory 
          (expand-file-name kerl-env-dir)) "activate
erlangexe=$(basename $0)
exec ${erlangexe#.erl-}
"))
    ;; Now make it executable
    (chmod
     script-name
     (file-modes-symbolic-to-number
      "u+x" (file-modes script-name)))
    script-name))

(defun erlh/find-kerl-root ()
  "Find the current kerl/erlang root. 

Uses `erlh-kerl-installs-dir' and `erlh-kerl-installs-pattern'
and asks the user if there is more than one."
  ;; FIXME we could improve this by having per-project erlang install
  ;; roots as well
  ;;
  ;; something like:
  ;;
  ;; (find-file (expand-file-name ".kerl-root" (locate-dominating-file)))
  (let ((installs
         (--filter
          (file-directory-p
           (expand-file-name it erlh-kerl-installs-dir))
          (directory-files
           erlh-kerl-installs-dir  nil
           erlh-kerl-installs-pattern))))
    (expand-file-name 
     (cond 
       ((< (length installs) 2)
        (car installs))
       ((> (length installs) 1)
        (read-file-name
         "erlang base dir: " erlh-kerl-installs-dir
         nil t nil
         (lambda (candidate) (string-match-p erlh-kerl-installs-pattern candidate))))
       (t (error "you must have an erlang installation.")))
     erlh-kerl-installs-dir)))

;; maybe we can use this directly off an `erlang-mode-hook'
(defun erlh-hack-env (&optional kerl-root)
  "Hack the erl environment with kerl.

This is like flet for the erlang environment in Emacs.  It
injects the kerl environment into the erlang tools by replacing
the erlang tools with a BASH shell script that sources the kerl
environment script and then runs the specified erlang tool.  It
symlink bombs your current directory with links to that shell
script for each erlang tool, but they are at least all
namespaced."
  (interactive (list (erlh/find-kerl-root)))
  (unless kerl-root
    (setq kerl-root (erlh/find-kerl-root)))
  (make-variable-buffer-local 'erlang-root-dir)
  (setq erlang-root-dir kerl-root)
  ;; Make the script we will use for exec-ing all erlang tools
  (unless (file-exists-p (expand-file-name ".erl"))
    (erlh/make-script kerl-root))
  (make-variable-buffer-local 'inferior-erlang-machine)
  (--map
   (let* ((prog-name (symbol-value it))
          (script-name
           (concat ".erl-" (if (file-name-absolute-p prog-name)
                               (file-name-base prog-name) 
                               prog-name))))
     (or (file-exists-p script-name)
         (make-symbolic-link ".erl" script-name))
     (set it (expand-file-name script-name)))
   (list 'inferior-erlang-machine))
  ;; Now make the manuals work
  (erlang-man-init)
  (local-set-key (kbd "C-#") 'erlang-man-function))

;; automatically install? I think so... at least for now.
(add-hook 'erlang-mode-hook 'erlh-hack-env)

(provide 'erlh)

;;; erlh.el ends here
