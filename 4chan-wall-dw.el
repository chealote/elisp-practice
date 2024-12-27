;; references for writing this:
;; https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
;; http://xahlee.info/emacs/emacs/elisp_read_file_content.html

(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun list-images-from-file (file)
  (re-seq "\\/\\/i\\.4cdn\\.org/wg/[0-9]+\\.\\(jpg\\|png\\)" (get-string-from-file file)))

(defun get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun format-4chan-img-url (partial-url)
  (concat "https:" partial-url))

(defun download-format-file (url output-folder-path)
  (let ((new-url (format-4chan-img-url url))
	(output-file (concat output-folder-path (nth 4 (split-string url "/")))))
    (url-copy-file new-url output-file t)
    output-file))

(defun check-output-path (path)
  (if (not (string= (substring path (1- (length path))) "/"))
      (setq path (concat path "/")))
  (if (not (file-directory-p path))
      (progn
	(message "folder doesn't exists, creating it...")
	(make-directory path)))
  path)

(defun download-or-use-local-file (html-page-file)
    (if (not (file-exists-p html-page-file))
	(url-copy-file (read-string "4chan url: ") html-page-file)
      (let ((user-response
	     (read-string (concat "4chan url (blank to use file " html-page-file "): "))))
	(if (not (string= user-response ""))
	    (url-copy-file user-response html-page-file t))))
    html-page-file)

(defun set-as-wallpaper (filepath)
  (shell-command (concat "feh --bg-fill " filepath)))

(let (
      (image-list
       (list-images-from-file (download-or-use-local-file "/tmp/index.html")))
      (save-wallpaper-path
       (check-output-path "~/Pictures/Wallpapers/")))
  (let ((random-image (nth (random (length image-list)) image-list)))
    (set-as-wallpaper (download-format-file random-image save-wallpaper-path))))
