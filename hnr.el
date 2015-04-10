;; -*- lexical-binding: t -*-
(require 'json)
(require 'url)
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

(defvar hnr-max-items 5)

(defgroup hnr nil
  "hackernews emacs client"
  :group 'external
  :prefix "hnr-")

(defface hnr-primary-face
  '((t (:foreground "White" :weight bold)))
  "Face used for news title"
  :group 'hnr)

(defface hnr-secondary-face
  '((t (:foreground "DimGrey")))
  "Face used for additional description"
  :group 'hnr)

(defface hnr-link-face
  '((t (:foreground "CornflowerBlue")))
  "Face used for links"
  :group 'hnr)

(defface hnr-link-hover-face
  '((t (:foreground "CornflowerBlue" :underline t)))
  "Face used for highlighted links"
  :group 'hnr)

(defface hnr-points-face
  '((t (:foreground "gray")))
  "Face used for points section"
  :group 'hnr)

(defface hnr-points-selected-face
  '((t (:foreground "red")))
  "Face used for highlighted points section"
  :group 'hnr)

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
   (insert-button label
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

(defun hnr-load-more (&optional x)
  (interactive)
  (end-of-buffer)
  (forward-line -1)
  (end-of-line)
  (enable-write
   (delete-region (point) (point-max)))
  (setq hnr--item (+ 1 hnr--item))
  (setq hnr--max-items (+ hnr--max-items hnr-max-items))
  (hnr--fetch-item))

(defun hnr--process-get-item (res)
  (hnr--switch-to-buffer)
  (let* ((item-json (json-read-from-string res))
	 (item-type (cdr (assoc 'type item-json)))
	 (item-descendants (cdr (assoc 'descendants item-json)))
	 (item-url (cdr (assoc 'url item-json)))
	 (item-by (cdr (assoc 'by item-json)))
	 (buffer-read-only nil))
    (insert (propertize (format "%s %-4d " (make-string 1 9650) (cdr (assoc 'score item-json)))
			'id item-url
			'face 'hnr-points-face))
    (hnr--insert-primary (cdr (assoc 'title item-json)))
    (newline)
    (beginning-of-line)
    (insert (make-string 7 32))
    (hnr--button (format "https://news.ycombinator.com/user?id=%s" item-by) item-by)
    (hnr--insert-secondary " | ")
    (hnr--insert-secondary (format "%s" (format-time-string "%Y-%m-%d %H:%M:%S" (seconds-to-time (cdr (assoc 'time item-json))))))
    (when (and (or (string= "story" item-type)
		   (string= "pool" item-type))
	       (> item-descendants 0))
      (hnr--insert-secondary (format " | %d Comments" item-descendants))
      (hnr--button (format "https://news.ycombinator.com/item?id=%d" (elt (cdr (assoc 'kids item-json)) 0))
		   (format "..." item-descendants)))
    (newline)
    (beginning-of-line)
    (insert (make-string 7 32))
    (hnr--button item-url item-url))
  (enable-write
   (newline 2)
   (beginning-of-line))
  (if (< hnr--item (min hnr--max-items (length hnr--items)))
      (progn (setq hnr--item (+ 1 hnr--item))
	     (hnr--fetch-item))
    (enable-write
     (newline)
     (insert-button "More..."
		    'follow-link t
		    'face 'hnr-link-face
		    'mouse-face 'hnr-link-hover-face
		    'action 'hnr-load-more))))

(defun hnr--process-topstories (res)
  (setq hnr--items-str res)
  (setq hnr--items (json-read-from-string res))
  (setq hnr--item 0)
  (setq hnr--max-items hnr-max-items)
  (hnr--fetch-item))

(when hnr--keymap
  (define-key hnr--keymap (kbd "q") 'hnr-quit)
  (define-key hnr--keymap (kbd "SPC") 'hnr-load-more)
  (define-key hnr--keymap (kbd "n") 'hnr-next)
  (define-key hnr--keymap (kbd "p") 'hnr-previous)
  (define-key hnr--keymap (kbd "RET") 'hnr-open-selected))

(defvar hnr--selected-item "")
(defvar hnr--prv-selected-point (point))

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
    (browse-url (format "https://news.ycombinator.com/item?id=%d" hnr--selected-item))))

(defun hnr ()
  (hnr--switch-to-buffer)
  (enable-write
   (delete-region (point-min) (point-max)))
  (use-local-map hnr--keymap)
  (setq buffer-read-only t)
  (enable-write
   (newline))
  (hnr--http-get "https://hacker-news.firebaseio.com/v0/topstories.json" 'hnr--process-topstories)
  ;;(hnr--http-get "https://hacker-news.firebaseio.com/v0/newstories.json" 'hnr--process-topstories)
  )

(hnr)
