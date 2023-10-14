;;; bfmt.el --- A formatter frontend that applies changes in batch -*- lexical-binding: t -*-

;; Copyright (C) 2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: files processes tools
;; URL: https://github.com/akirak/bfmt.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; FIXME

;;; Code:

(defconst bfmt-formatter-output-buffer "*bfmt output*")

(defcustom bfmt-formatter-function #'bfmt-run-formatter-command
  "Function used to format files in a project.

The function takes as an argument a list of file names to be
formatted and should run a formatter in sync."
  :type 'function)

(defcustom bfmt-formatter-command '("nix" "fmt" "--")
  "Formatter command used in `bfmt-run-formatter-command'.

If the value is a function, it takes as argument a list of file
names and should return a formatter command as a list of strings.

If it is a list of strings, a list of file names will be appended
to the list, and the entire list will be used as a formatter
command."
  :type '(choice function
                 (repeat :tag "Command" string)
                 (const nil)))

(defcustom bfmt-root-location nil
  "Function or file name used to determine the root of the project.

If it is a function, it should take the file name or the
directory of the buffer as the argument and returns a directory.
The returned directory can be either abbreviated or unabbreviated
but should be consistent across all files under the same root.

Alternatively, it can be a file name which indicates the root of the project.

If the value is nil, `bfmt-enqueue-this-file' and `bfmt-apply' do nothing."
  :type '(choice function
                 (string :tag "File name")
                 (const nil)))

(defcustom bfmt-check-git-diff t
  "When non-nil, exclude files that have not changed since HEAD."
  :type 'boolean)

(defcustom bfmt-initial-project-map-size 30
  "Initial size of the hash table `bfmt-per-root-queues'."
  :type 'number)

(defvar bfmt-per-root-queues
  (make-hash-table :test #'equal :size bfmt-initial-project-map-size))

;;;###autoload
(define-minor-mode bfmt-mode
  "Minor mode in which files are queued for formatting on save."
  :lighter " Bfmt"
  (if bfmt-mode
      (add-hook 'after-save-hook #'bfmt-enqueue-this-file nil t)
    (remove-hook 'after-save-hook #'bfmt-enqueue-this-file t)))

;;;###autoload
(define-globalized-minor-mode bfmt-global-mode bfmt-mode
  (lambda ()
    (when (and buffer-file-name
               (not (buffer-base-buffer)))
      (bfmt-mode t))))

(defun bfmt--find-root (file)
  (cl-typecase bfmt-root-location
    (function (funcall bfmt-root-location file))
    (string (locate-dominating-file file bfmt-root-location))))

;;;###autoload
(defun bfmt-apply ()
  "Apply formatter to files under the root directory."
  (interactive)
  (when-let (root (bfmt--find-root default-directory))
    (when-let (queue (gethash root bfmt-per-root-queues))
      (let* ((default-directory root)
             (files (cl-remove-duplicates queue :test #'equal))
             (files (if bfmt-check-git-diff
                        (bfmt-exclude-unchanged-files files)
                      files)))
        (when files
          (funcall bfmt-formatter-function files)))
      (puthash root nil bfmt-per-root-queues))))

(defun bfmt-exclude-unchanged-files (files)
  (when-let (files (thread-last
                     (apply #'process-lines "git" "status" "--porcelain" "--" files)
                     (cl-remove-if (lambda (s)
                                     (string-match-p (rx bol any " ") s)))
                     (mapcar (lambda (s) (substring s 3)))))
    (mapcar `(lambda (file)
               (expand-file-name file ,(vc-git-root default-directory)))
            files)))

;;;###autoload
(defun bfmt-enqueue-this-file ()
  "Enqueue the buffer file to the formatting queue."
  (when bfmt-root-location
    (when-let* ((file (buffer-file-name))
                (root (bfmt--find-root default-directory)))
      (let ((queue (gethash root bfmt-per-root-queues)))
        (unless (and queue (equal (car queue) file))
          (puthash root
                   (cons file queue)
                   bfmt-per-root-queues))))))

(defun bfmt-run-formatter-command (files)
  (when-let (buffer (get-buffer bfmt-formatter-output-buffer))
    (kill-buffer buffer))
  (with-current-buffer (generate-new-buffer bfmt-formatter-output-buffer)
    (pcase-exhaustive (bfmt-formatter-command files)
      (`(,cmd . ,args)
       (unless (zerop (apply #'call-process cmd
                             nil t nil
                             args))
         (pop-to-buffer bfmt-formatter-output-buffer)
         (user-error "nix fmt finished with non-zero exit code")))
      (`nil))))

(defun bfmt--formatter-command (files)
  (cl-typecase bfmt-formatter-command
    (function
     (funcall bfmt-formatter-command files))
    (list
     (assert (seq-every-p #'stringp bfmt-formatter-command))
     (append bfmt-formatter-command files))))

(provide 'bfmt)
;;; bfmt.el ends here
