;; -*- lexical-binding: t -*-
(require 'json)
(require 'url)
(require 'cl-lib)
(setq lexical-binding t)

(defun hnr--http-get (url callback)
  (let ((url-request-method "GET"))
    (url-retrieve url
		  (lambda (state)
		    (switch-to-buffer (current-buffer))
		    (goto-char (point-min))
		    (when (not (string-match "200 OK" (buffer-string)))
		      (kill-buffer)
		      (error "Could not connect to server"))
		    (goto-char (point-min))
		    (search-forward "\n\n")
		    (let ((res (buffer-substring-no-properties
				(point)
				(point-max))))
		      (kill-buffer)
		      (funcall callback (decode-coding-string
					 (encode-coding-string res 'utf-8) 'utf-8)))))))

(defvar hnr--items-str "")
(defvar hnr--items '())
(defvar hnr--item 0)
(defvar hnr--max-items 5)
(defvar hnr--keymap (make-sparse-keymap))
(defvar hnr--max-item 0)
(defvar hnr--read-item 0)

(defvar hnr-max-items 10)
(defvar hnr-auto-mark-as-read nil)

(defgroup hnr nil
  "hackernews emacs client"
  :group 'external
  :prefix "hnr-")

(defface hnr-primary-face
  '((t (:foreground "SteelBlue3" :weight bold)))
  "Face used for news title"
  :group 'hnr)

(defface hnr-secondary-face
  '((t (:foreground "DimGrey")))
  "Face used for additional description"
  :group 'hnr)

(defface hnr-link-face
  '((t (:foreground "DimGrey" :underline t)))
  "Face used for links"
  :group 'hnr)

(defface hnr-link-hover-face
  '((t (:foreground "gold" :underline t)))
  "Face used for highlighted links"
  :group 'hnr)

(defface hnr-points-face
  '((t (:foreground "sienna1")))
  "Face used for points section"
  :group 'hnr)

(defface hnr-points-selected-face
  '((t (:foreground "gold")))
  "Face used for highlighted points section"
  :group 'hnr)

(defvar hnr-cache-file "~/.emacs.d/.hnr-cache")

(defun hnr--store-cache (id)
  (write-region (number-to-string id) nil hnr-cache-file nil))

(defun hnr--get-cache ()
  (with-temp-buffer
    (if (file-exists-p hnr-cache-file)
	(progn (insert-file-contents hnr-cache-file)
	       (string-to-number (buffer-string)))
      0)))

(defun hnr--switch-to-buffer ()
  (switch-to-buffer (get-buffer-create "Hacker News Reader")))

(defmacro enable-write (&rest forms)
  `(let ((buffer-read-only nil))
     (progn .,forms)))

(defun hnr-quit ()
  (interactive)
  (let ((buff (get-buffer "Hacker News Reader")))
    (when buff
      (kill-buffer buff))))

(defun hnr--button (url-address label)
  (enable-write
   (insert-button (or label "[no link]")
		  'follow-link t
		  'action (lambda (x) (browse-url (button-get x 'url)))
		  'url url-address
		  'face 'hnr-link-face
		  'mouse-face 'hnr-link-hover-face)))

(defun hnr--insert-secondary (text)
  (enable-write
   (insert (propertize text 'face 'hnr-secondary-face))))

(defun hnr--insert-primary (text)
  (enable-write
   (insert (propertize text 'face 'hnr-primary-face))))

(defun hnr--fetch-item ()
  (hnr--http-get (format "https://hacker-news.firebaseio.com/v0/item/%s.json" (elt hnr--items hnr--item)) 'hnr--process-get-item))

(defvar hnr--load-more-point 0)

(defun hnr-load-more (&optional x)
  (interactive)
  (when (< hnr--item (- (length hnr--items) 1))
    (goto-char (point-max))
    (forward-line -1)
    (end-of-line)
    (enable-write
     (delete-region (point) (point-max)))
    (setq hnr--load-more-point (point))
    (setq hnr--item (+ 1 hnr--item))
    (setq hnr--max-items (+ hnr--max-items hnr-max-items))
    (hnr--fetch-item)))

(defun hnr--process-get-item (res)
  (hnr--switch-to-buffer)
  (let* ((item-json (json-read-from-string res))
	 (item-id (cdr (assoc 'id item-json)))
	 (item-type (cdr (assoc 'type item-json)))
	 (item-descendants (cdr (assoc 'descendants item-json)))
	 (item-url (cdr (assoc 'url item-json)))
	 (item-by (cdr (assoc 'by item-json)))
	 (item-kids (cdr (assoc 'kids item-json)))
	 (item-time (cdr (assoc 'time item-json)))
	 (item-id-url (format "https://news.ycombinator.com/item?id=%d" item-id))
	 (item-final-url (if (or (not item-url) (string= "" item-url)) item-id-url item-url))
	 (buffer-read-only nil))
    (setq hnr--max-item (max hnr--max-item item-id))
    (insert (propertize (format "%s %-4d " (make-string 1 9650) (cdr (assoc 'score item-json)))
			'id item-final-url
			'root item-id
			'kids item-kids
			'face 'hnr-points-face))
    (hnr--insert-primary (cdr (assoc 'title item-json)))
    (newline)
    (beginning-of-line)
    (insert (make-string 7 32))
    (hnr--button (format "https://news.ycombinator.com/user?id=%s" item-by) item-by)
    (hnr--insert-secondary " | ")
    (hnr--insert-secondary (format "%s" (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time item-time))))
    (when (and (or (string= "story" item-type)
		   (string= "pool" item-type))
	       (> item-descendants 0))
      (hnr--insert-secondary " | ")
      (hnr--button item-id-url (format "%d Comments" item-descendants)))
    (newline)
    (beginning-of-line)
    (insert (make-string 7 32))
    (hnr--button item-final-url item-final-url)
    (newline 2)
    (beginning-of-line))
  (if (< hnr--item (- (length hnr--items) 1))
      (if (< hnr--item hnr--max-items)
	  (progn (setq hnr--item (+ 1 hnr--item))
		 (hnr--fetch-item))
	(enable-write
	 (newline)
	 (insert-button "Mark all as read"
			'follow-link t
			'face 'hnr-link-face
			'mouse-face 'hnr-link-hover-face
			'action 'hnr-mark-all-as-read)
	 (insert "      ")
	 (insert-button "More..."
			'follow-link t
			'face 'hnr-link-face
			'mouse-face 'hnr-link-hover-face
			'action 'hnr-load-more))
	(goto-char hnr--load-more-point)
	(hnr--move t))
    (enable-write
     (insert "End of Feed")
     (insert "      ")
     (insert-button "Mark all as read"
		    'follow-link t
		    'face 'hnr-link-face
		    'mouse-face 'hnr-link-hover-face
		    'action 'hnr-mark-all-as-read)
     (goto-char hnr--load-more-point)
     (hnr--move t))
    (when hnr-auto-mark-as-read
      (hnr-mark-all-as-read))))

(defun hnr--filter-read (list)
  (cl-remove-if (lambda (x) (<= x hnr--read-item)) list))

(defun hnr--process-topstories (res)
  (setq hnr--items-str res)
  (setq hnr--items (hnr--filter-read (json-read-from-string res)))
  (setq hnr--item 0)
  (setq hnr--max-items hnr-max-items)
  (if (> (length hnr--items) 0)
      (hnr--fetch-item)
    (enable-write (insert "No new items"))))

(when hnr--keymap
  (define-key hnr--keymap (kbd "q") 'hnr-quit)
  (define-key hnr--keymap (kbd "SPC") 'hnr-load-more)
  (define-key hnr--keymap (kbd "n") 'hnr-next)
  (define-key hnr--keymap (kbd "p") 'hnr-previous)
  (define-key hnr--keymap (kbd "<down>") 'hnr-next)
  (define-key hnr--keymap (kbd "<up>") 'hnr-previous)
  (define-key hnr--keymap (kbd "a") 'hnr-mark-all-as-read)
  (define-key hnr--keymap (kbd "RET") 'hnr-open-selected))

(defvar hnr--selected-item "")
(defvar hnr--selected-item-kids '())
(defvar hnr--prv-selected-point 0)
(defvar hnr--selected-item-id 0)

(defun hnr--move (forward)
  (if forward
      (re-search-forward (make-string 1 9650) nil t)
    (beginning-of-line)
    (re-search-backward (make-string 1 9650) nil t))
  (unless (string= hnr--selected-item "")
    (save-excursion
      (goto-char hnr--prv-selected-point)
      (beginning-of-line)
      (enable-write
       (put-text-property (point) (+ (point) 7) 'face 'hnr-points-face))))
  (setq hnr--selected-item (get-char-property (point) 'id))
  (setq hnr--selected-item-kids (get-char-property (point) 'kids))
  (setq hnr--selected-item-id (get-char-property (point) 'root))
  (beginning-of-line)
  (setq hnr--prv-selected-point (point))
  (enable-write
   (put-text-property (point) (+ (point) 7) 'face 'hnr-points-selected-face))
  (forward-char 7))

(defun hnr-next ()
  (interactive)
  (hnr--move t))

(defun hnr-previous ()
  (interactive)
  (hnr--move nil))

(defun hnr-open-selected ()
  (interactive)
  (unless (string= "" hnr--selected-item)
    (browse-url hnr--selected-item)))

(defun hnr-mark-all-as-read (&optional x)
  (interactive)
  (hnr--store-cache (max hnr--max-item hnr--read-item)))

(defun hnr ()
  (interactive)
  (setq hnr--read-item (hnr--get-cache))
  ;;(setq hnr--read-item 9373303)
  (hnr--switch-to-buffer)
  (enable-write
   (delete-region (point-min) (point-max)))
  (setq hnr--load-more-point 0)
  (use-local-map hnr--keymap)
  (setq buffer-read-only t)
  (enable-write
   (newline))
  (hnr--http-get "https://hacker-news.firebaseio.com/v0/topstories.json" 'hnr--process-topstories))

(provide 'hnr)
