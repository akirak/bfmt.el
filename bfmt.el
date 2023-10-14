;;; bfmt.el --- A formatter frontend that applies changes in batch -*- lexical-binding: t -*-

(defconst bfmt-formatter-output-buffer "*bfmt output*")

(defcustom bmt-formatter-function #'bfmt-nix-fmt
  "Function used to format files in a project."
  :type 'function)

(defcustom bfmt-root-function #'bfmt-find-treefmt-nix
  "Function used to determine the root of the project.

The function should take the file name or the directory of the
buffer as the argument and returns a directory. The returned
directory can be either abbreviated or unabbreviated but should
be consistent across all files under the same root."
  :type 'function)

(defcustom bfmt-check-git-diff t
  "When non-nil, exclude files that have not changed since HEAD."
  :type 'boolean)

(defcustom bfmt-initial-project-map-size 30
  "Initial size of the hash table `bfmt-per-root-queues'."
  :type 'number)

(defvar bfmt-per-root-queues
  (make-hash-table :test #'equal :size bfmt-initial-project-map-size))

;;;###autoload
(defun bfmt-apply ()
  "Apply formatter to files under the root directory."
  (when-let (root (funcall bfmt-root-function default-directory))
    (when-let (queue (gethash root bfmt-per-root-queues))
      (let* ((default-directory root)
             (files (cl-remove-duplicates queue :test #'equal))
             (files (if bfmt-check-git-diff
                        (bfmt-exclude-unchanged-files files)
                      files)))
        (when files
          (funcall bmt-formatter-function files)))
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
  (when-let* ((file (buffer-file-name))
              (root (funcall bfmt-root-function file)))
    (let ((queue (gethash root bfmt-per-root-queues)))
      (unless (and queue (equal (car queue) file))
        (puthash root
                 (cons file queue)
                 bfmt-per-root-queues)))))

;;;; Specific formatters

(defun bfmt-find-treefmt-nix (file)
  (locate-dominating-file file "treefmt.nix"))

(defun bfmt-nix-fmt (files)
  "Run \"nix fmt\" on FILES."
  (when-let (buffer (get-buffer bfmt-formatter-output-buffer))
    (kill-buffer buffer))
  (with-current-buffer (generate-new-buffer bfmt-formatter-output-buffer)
    (unless (zerop (apply #'call-process "nix" nil t nil
                          "fmt" files))
      (pop-to-buffer bfmt-formatter-output-buffer)
      (user-error "nix fmt finished with non-zero exit code"))))

(provide 'bfmt)
;;; bfmt.el ends here
