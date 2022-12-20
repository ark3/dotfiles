(use-package blacken)

(defun my/google-java-format-buffer ()
  "Use google-java-format to format the current buffer."
  (interactive)
  (let ((cursor (point))
        (temp-buffer (generate-new-buffer " *google-java-format-temp*"))
        (stderr-file (make-temp-file "google-java-format")))
    (unwind-protect
        (let ((status (call-process-region
                       (point-min) (point-max)
                       (executable-find "google-java-format")
                       nil (list temp-buffer stderr-file) t
                       "-"))
              (stderr
               (with-temp-buffer
                 (insert-file-contents stderr-file)
                 (when (> (point-max) (point-min))
                   (insert ": "))
                 (buffer-substring-no-properties
                  (point-min) (line-end-position)))))
          (cond
           ((stringp status)
            (error "google-java-format killed by signal %s%s" status stderr))
           ((not (zerop status))
            (error "google-java-format failed with code %d%s" status stderr))
           (t (message "google-java-format succeeded%s" stderr)
              (replace-buffer-contents temp-buffer))))
      ;;(goto-char cursor)
      (delete-file stderr-file)
      (when (buffer-name temp-buffer) (kill-buffer temp-buffer)))))

(defun my/clang-format-buffer ()
  "Use clang-format to format the current buffer."
  (interactive)
  (let ((cursor (point))
        (temp-buffer (generate-new-buffer " *clang-format-temp*"))
        (stderr-file (make-temp-file "clang-format")))
    (unwind-protect
        (let ((status (call-process-region
                       (point-min) (point-max)
                       (executable-find "clang-format")
                       nil (list temp-buffer stderr-file) t
                       "--style=file"))
              (stderr
               (with-temp-buffer
                 (insert-file-contents stderr-file)
                 (when (> (point-max) (point-min))
                   (insert ": "))
                 (buffer-substring-no-properties
                  (point-min) (line-end-position)))))
          (cond
           ((stringp status)
            (error "clang-format killed by signal %s%s" status stderr))
           ((not (zerop status))
            (error "clang-format failed with code %d%s" status stderr))
           (t (message "clang-format succeeded%s" stderr)
              (replace-buffer-contents temp-buffer))))
      ;;(goto-char cursor)
      (delete-file stderr-file)
      (when (buffer-name temp-buffer) (kill-buffer temp-buffer)))))
