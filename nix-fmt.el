;;; nix-fmt.el --- A frontend to the Nix formatter -*- lexical-binding: t -*-

;; Copyright (C) 2023 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: files processes tools
;; URL: https://github.com/akirak/nix-fmt.el

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

(defconst nix-fmt-formatter-output-buffer "*nix-fmt output*")

(defcustom nix-fmt-root-location nil
  "Function or file name used to determine the root of the project.

If it is a function, it should take the file name or the
directory of the buffer as the argument and returns a directory.
The returned directory can be either abbreviated or unabbreviated
but should be consistent across all files under the same root.

Alternatively, it can be a file name which indicates the root of the project.

If the value is nil, `nix-fmt-enqueue-this-file' and `nix-fmt-apply' do nothing."
  :type '(choice function
                 (string :tag "File name")
                 (const nil)))

(defcustom nix-fmt-file-regexp "\\.nix\\'"
  "Regular expression to match files that should be formatted."
  :type 'regexp)

(defcustom nix-fmt-check-git-diff t
  "When non-nil, exclude files that have not changed since HEAD."
  :type 'boolean)

(defcustom nix-fmt-initial-project-map-size 30
  "Initial size of the hash table `nix-fmt-per-root-queues'."
  :type 'number)

(defvar nix-fmt-per-root-queues
  (make-hash-table :test #'equal :size nix-fmt-initial-project-map-size))

(defvar nix-fmt-root-statuses
  (make-hash-table :test #'equal :size nix-fmt-initial-project-map-size))

(defvar nix-fmt-program-statuses
  (make-hash-table :test #'equal :size (* 2 nix-fmt-initial-project-map-size)))

(defvar nix-fmt-nix-system nil)

(defun nix-fmt-nix-system ()
  (or nix-fmt-nix-system
      (setq nix-fmt-nix-system
            (with-temp-buffer
              (unless (zerop (call-process "nix" nil (list t nil) nil
                                           "eval" "--expr" "builtins.currentSystem"
                                           "--impure" "--raw"))
                (error "Failed to retrieve the Nix system"))
              (buffer-string)))))

(defun nix-fmt--formatter (dir)
  (when-let (dir (locate-dominating-file (or dir default-directory)
                                         "flake.nix"))
    (if (file-exists-p (expand-file-name "flake.lock" dir))
        (when-let*
            ((default-directory dir)
             (installable (format ".#formatter.%s" (nix-fmt-nix-system)))
             (result (with-temp-buffer
                       (when (zerop (call-process "nix" nil (list t nil) nil
                                                  "build" installable
                                                  "--print-out-paths"
                                                  "--no-link"
                                                  "--no-write-lock-file"
                                                  "--accept-flake-config"))
                         (string-chop-newline (buffer-string)))))
             (program (with-temp-buffer
                        (when (zerop (call-process "nix" nil (list t nil) nil
                                                   "eval"
                                                   (concat installable ".meta.mainProgram")
                                                   "--raw"))
                          (string-chop-newline (buffer-string))))))
          (expand-file-name (concat "bin/" program) result))
      (message "flake.lock doesn't exist")
      nil)))

;;;###autoload
(define-minor-mode nix-fmt-mode
  "Minor mode in which files are queued for formatting on save."
  :lighter " Nix-Fmt"
  (if nix-fmt-mode
      (add-hook 'after-save-hook #'nix-fmt-enqueue-this-file nil t)
    (remove-hook 'after-save-hook #'nix-fmt-enqueue-this-file t)))

;;;###autoload
(define-globalized-minor-mode nix-fmt-global-mode nix-fmt-mode
  (lambda ()
    (when (and buffer-file-name
               (not (buffer-base-buffer)))
      (nix-fmt-mode t))))

(defun nix-fmt--find-root (file)
  (cl-typecase nix-fmt-root-location
    (function (funcall nix-fmt-root-location file))
    (string (locate-dominating-file file nix-fmt-root-location))))

;;;###autoload
(defun nix-fmt-apply (&optional dir)
  "Apply formatter to files under the root directory."
  (interactive)
  (when-let (root (nix-fmt--find-root (or dir default-directory)))
    (when-let (queue (gethash root nix-fmt-per-root-queues))
      (let* ((default-directory root)
             (files (cl-remove-if-not (lambda (filename)
                                        (string-match-p nix-fmt-file-regexp filename))
                                      (cl-remove-duplicates queue :test #'equal)))
             (files (if nix-fmt-check-git-diff
                        (nix-fmt-exclude-unchanged-files files)
                      files)))
        (when files
          (nix-fmt--run-formatter files)))
      (remhash root nix-fmt-per-root-queues))))

;;;###autoload
(defun nix-fmt-apply-all ()
  (interactive)
  (dolist (dir (map-keys nix-fmt--find-root))
    (nix-fmt-apply dir)))

(defun nix-fmt--run-formatter (files)
  (when-let (buffer (get-buffer nix-fmt-formatter-output-buffer))
    (kill-buffer buffer))
  (with-current-buffer (generate-new-buffer nix-fmt-formatter-output-buffer)
    (unless (zerop (apply #'call-process "nix"
                          nil t nil
                          "fmt" "--" files))
      (pop-to-buffer nix-fmt-formatter-output-buffer)
      (user-error "nix fmt finished with non-zero exit code"))))

(defun nix-fmt-exclude-unchanged-files (files)
  (when-let (files (thread-last
                     (apply #'process-lines "git" "status" "--porcelain" "--" files)
                     (cl-remove-if (lambda (s)
                                     (string-match-p (rx bol any " ") s)))
                     (mapcar (lambda (s) (substring s 3)))))
    (mapcar `(lambda (file)
               (expand-file-name file ,(vc-git-root default-directory)))
            files)))

;;;###autoload
(defun nix-fmt-enqueue-this-file ()
  "Enqueue the buffer file to the formatting queue."
  (when nix-fmt-root-location
    (when-let* ((file (buffer-file-name))
                (root (nix-fmt--find-root default-directory)))
      (let ((queue (gethash root nix-fmt-per-root-queues)))
        (unless (and queue (equal (car queue) file))
          (puthash root
                   (cons file queue)
                   nix-fmt-per-root-queues))))))

(provide 'nix-fmt)
;;; nix-fmt.el ends here
