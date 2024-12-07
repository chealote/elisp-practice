;; found in https://emacs.stackexchange.com/questions/7148/get-all-regexp-matches-in-buffer-as-a-list
(defun re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

;; found in http://xahlee.info/emacs/emacs/elisp_read_file_content.html
(defun get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

(defun download-format-file (url output-folder-path)
  (let (
	(new-url (concat "https:" url))
	(output-file (concat output-folder-path (nth 4 (split-string url "/"))))
	)
    (if new-url
	(url-copy-file new-url output-file t)
      (message (concat "url is nil:" url)))))

(defun list-images-from-file (file)
  (re-seq "\\/\\/i\\.4cdn\\.org/wg/[0-9]+\\.\\(jpg\\|png\\)" (get-string-from-file file)))

(defun download-images-from-4chan (url output-folder)
  (let (
	(tmp-file "/tmp/test.html")
	)
    (url-copy-file url tmp-file t)
    (mapcar (lambda (file)
	      (download-format-file file output-folder))
	    (list-images-from-file tmp-file))))

;; replace this url with a valid url thread
(download-images-from-4chan
 "https://boards.4chan.org/wg/thread/..."
 "~/Pictures/Wallpapers")
