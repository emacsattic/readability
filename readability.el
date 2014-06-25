;;; readability.el --- Read articles on Emacs via Readability -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2014 by Shingo Fukuyama

;; Version: 1.0.1
;; Author: Shingo Fukuyama - http://fukuyama.co
;; URL: https://github.com/ShingoFukuyama/emacs-readability
;; Created: Jun 24 2014
;; Keywords: readability oauth
;; Package-Requires: ((oauth "1.04") (ov "1.0") (emacs "24.3"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;;; Commentary:

;;; Readability API:
;; https://www.readability.com/developers/api

;;; Code:

(require 'cl-lib)
(require 'oauth)
(require 'ov)
(require 'shr)
(require 'json)

;; oauth-hmac-sha1-param-reverse has been nil in some environments
(unless oauth-hmac-sha1-param-reverse
  (if (require 'sasl nil t)
      (setq oauth-nonce-function #'sasl-unique-id)
    (setq oauth-nonce-function #'oauth-internal-make-nonce)))

(defgroup readability nil
  "Group for readability.el"
  :prefix "readability-" :group 'applications)

(defcustom readability-file-location (concat user-emacs-directory "readability-token")
  "File path to store token"
  :group 'readability
  :type 'string)

(defvar readability-parameters nil
  "you can specify more parameters:
https://www.readability.com/developers/api/reader#idm301959944144")
(setq readability-parameters
      '(("archive"  . nil)  ;; "0", "1"
        ("favorite" . nil)  ;; "0", "1"
        ("order"    . nil)  ;; "-date_added", "date_added", "-date_updated", "date_updated"
        ("page"     . nil)  ;; "1" ~
        ("per_page" . "50") ;; "1" ~ "50"
        ("domain"   . nil)  ;; string
        ("tags"     . nil)  ;; string
        ))

(defvar readability-font-list)
(setq readability-font-list
      '("Default"
        "Georgia"
        "Arial"
        "Verdana"))

(defvar readability-url-base      "https://www.readability.com")
(defvar readability-url-authorize (format "%s/api/rest/v1/oauth/authorize/"     readability-url-base))
(defvar readability-url-request   (format "%s/api/rest/v1/oauth/request_token/" readability-url-base))
(defvar readability-url-access    (format "%s/api/rest/v1/oauth/access_token/"  readability-url-base))
(defvar readability-key           "foko")
(defvar readability-secret        "38YYwcbMJBh5K4rHxcaXKGgXAQZUYHKs")
(defvar readability-access-token  nil)

(defvar readability-icon-face-on
  '(:foreground "#ee0" :family "FontAwesome" :height 1.2))
(defvar readability-icon-face-off
  '(:family "FontAwesome" :height 1.2))

(defvar readability-map-common
  (let (($map (make-sparse-keymap)))
    (define-key $map (kbd "n") (lambda () (interactive) (call-interactively 'next-line)))
    (define-key $map (kbd "j") (lambda () (interactive) (call-interactively 'next-line)))
    (define-key $map (kbd "p") (lambda () (interactive) (call-interactively 'previous-line)))
    (define-key $map (kbd "k") (lambda () (interactive) (call-interactively 'previous-line)))
    (define-key $map (kbd "f") (lambda () (interactive) (call-interactively 'forward-char)))
    (define-key $map (kbd "l") (lambda () (interactive) (call-interactively 'forward-char)))
    (define-key $map (kbd "b") (lambda () (interactive) (call-interactively 'backward-char)))
    (define-key $map (kbd "h") (lambda () (interactive) (call-interactively 'backward-char)))
    $map))

(defun readability--init ()
  "Get an access token from the token file. If it doesn't exist or fail to read from it,
start oauth authorization via your default browser."
  ;; Prevent to login with w3m or something for authorization
  (let ((browse-url-browser-function 'browse-url-default-browser))
    (when (file-exists-p readability-file-location)
      (with-temp-buffer
        (insert-file-contents readability-file-location)
        (when (re-search-forward "\\`\\([^:]+\\):\\([^\n]+\\)\\'" nil t)
          (setq readability-access-token
                (make-oauth-access-token
                 :consumer-key readability-key
                 :consumer-secret readability-secret
                 :auth-t (make-oauth-t
                          :token (match-string 1)
                          :token-secret (match-string 2)))))))
    (unless readability-access-token
      (setq readability-access-token
            (oauth-authorize-app readability-key
                                 readability-secret
                                 readability-url-request
                                 readability-url-access
                                 readability-url-authorize))
      (when readability-access-token
        (with-temp-file readability-file-location
          (erase-buffer)
          (let (($token (oauth-access-token-auth-t readability-access-token)))
            (insert (format "%s:%s"
                            (oauth-t-token $token)
                            (oauth-t-token-secret $token)))))))))

(defun readability--decode-json-string ($string)
  (with-temp-buffer
    (insert $string)
    (goto-char (point-min))
    (while (re-search-forward "\&\\(\#[^;]+;\\)" nil t)
      (let (($beg (match-beginning 0))
            ($end (match-end 0))
            ($hex (match-string 1)))
        (delete-region $beg $end)
        (insert (format "%c" (read $hex)))))
    (buffer-string)))

(defun readability-delete-token-and-file ()
  (interactive)
  (setq readability-access-token nil)
  (if (file-exists-p readability-file-location)
      (delete-file readability-file-location)
    (error (format "Token file couldn't find: %s" readability-file-location)))
  (message "Successfully delete token and token file"))

(defun readability--check-authentication ()
  (unless readability-access-token
    (readability--init)))

(defun readability--json-buffer-serialize ()
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward "\r" nil t)
      (replace-match "")))
  (delete-region (point-min) (save-excursion (re-search-forward "\n\n")))
  (json-read-from-string (buffer-substring-no-properties (point-min) (point-max))))

(defun readability--params-to-string ($params)
  (let (($string "?"))
    (mapc (lambda ($x)
            (if (cdr $x)
                (setq $string (format "%s%s=%s&" $string (car $x) (cdr $x)))))
          $params)
    (substring $string 0 -1)))

(defun readability--get-articles ()
  (readability--check-authentication)
  (let ($raw)
    (with-current-buffer (oauth-url-retrieve
                          readability-access-token
                          (concat readability-url-base
                                  "/api/rest/v1/bookmarks"
                                  (readability--params-to-string readability-parameters)))
      (setq $raw (readability--json-buffer-serialize)))
    $raw))

(defun readability--open-article ($article-id)
  (readability--check-authentication)
  (let ($raw)
    (with-current-buffer (oauth-url-retrieve
                          readability-access-token
                          (concat readability-url-base (format "/api/rest/v1/articles/%s" $article-id)))
      (setq $raw (readability--json-buffer-serialize)))
    ;; Display article buffer with shr format
    (let (($window (selected-window))
          ($buffer (get-buffer-create (format "Readability-%s" $article-id)))
          ($h1 (format "<h1>%s</h1>" (readability--decode-json-string (assoc-default 'title $raw))))
          ($body (readability--decode-json-string (assoc-default 'content $raw)))
          ($default-font))
      (with-current-buffer $buffer
        (read-only-mode 0)
        (ov-clear)
        (erase-buffer)
        (shr-insert-document
         (with-temp-buffer
           (insert $h1 $body)
           (libxml-parse-html-region (point-min) (point-max))))
        (goto-char (point-min))
        (read-only-mode 1)
        (set (make-local-variable 'readability-font-list) readability-font-list)
        (if (member "Default" readability-font-list)
            (setf (car (member "Default" readability-font-list))
                  (format "%s" (font-get (face-attribute 'default :font) :family))))
        (setq $default-font (pop readability-font-list))
        (setq readability-font-list (append readability-font-list `(,$default-font)))
        (ov-keymap
         (ov-set (ov (point-min) (point-max)) 'face '(:height 1.0) 'rdb-entire t)
         "+" (lambda () (interactive)
               (let* (($ov (car (ov-in 'rdb-entire)))
                      ($attr (cl-copy-list (ov-val $ov 'face)))
                      ($height (/ (round (+ (plist-get $attr :height) 0.1) 0.1) 10.0)))
                 (ov-set $ov 'face (plist-put $attr :height $height))))
         "-" (lambda () (interactive)
               (let* (($ov (car (ov-in 'rdb-entire)))
                      ($attr (cl-copy-list (ov-val $ov 'face)))
                      ($height (/ (round (- (plist-get $attr :height) 0.1) 0.1) 10.0)))
                 (when (> $height 0.1)
                   (ov-set $ov 'face (plist-put $attr :height $height)))))
         "F" (lambda () (interactive)
               (if (> (length readability-font-list) 0)
                   (let* (($ov (car (ov-in 'rdb-entire)))
                          ($attr (cl-copy-list (ov-val $ov 'face)))
                          ($font (pop readability-font-list)))
                     (setq readability-font-list (append readability-font-list `(,$font)))
                     (ov-set $ov 'face (plist-put $attr :family $font))))))
        (set-window-buffer $window $buffer)
        (use-local-map readability-map-common)))))

(defun readability--toggle-favorite-at ($bookmark-id $ov)
  (let (($fav (ov-val $ov 'rdb-fav)))
    (oauth-post-url readability-access-token
                    (format "%s/api/rest/v1/bookmarks/%s"
                            readability-url-base
                            $bookmark-id)
                    `(("favorite" . ,(if $fav "0" "1"))))
    ;; toggle icon's color
    (ov-set $ov 'face (if $fav
                          readability-icon-face-off
                        readability-icon-face-on))))

(defun readability--toggle-archive-at ($bookmark-id $ov)
  (let (($archive (ov-val $ov 'rdb-fav)))
    (oauth-post-url readability-access-token
                    (format "%s/api/rest/v1/bookmarks/%s"
                            readability-url-base
                            $bookmark-id)
                    `(("archive" . ,(if $archive "0" "1"))))
    ;; toggle icon's color
    (ov-set $ov 'face (if $archive
                          readability-icon-face-off
                        readability-icon-face-on))))

;;;###autoload
(defun readability-get-reading-list ()
  "Get a reading list and draw it on a buffer"
  (interactive)
  (readability--check-authentication)
  (with-current-buffer (get-buffer-create "Readability")
    (read-only-mode 0)
    (ov-clear)
    (erase-buffer)
    (let (($articles (readability--get-articles))
          ($fn-open-in-other-window
           (lambda () (interactive)
             (let (($id (ov-val (ov-at) 'rdb-article-id)))
               (other-window 1)
               (readability--open-article $id)))))
      (mapc (lambda ($x)
              (let* (($article  (assoc-default 'article  $x))
                     ($favorite (assoc-default 'favorite $x))
                     ($archive  (assoc-default 'archive  $x))
                     ($bookmark-id (assoc-default 'id  $x))
                     ($article-id  (assoc-default 'id $article)))
                (ov-keymap
                 (ov-set (ov-insert "\uf005")
                         'face (if (equal $favorite :json-false)
                                   readability-icon-face-off
                                 readability-icon-face-on)
                         'rdb-bookmark-id $bookmark-id
                         'rdb-fav (if (equal $favorite :json-false) nil t))
                 "RET" (lambda () (interactive)
                         (let* (($ov (ov-at))
                                ($id (ov-val $ov 'rdb-bookmark-id)))
                           (readability--toggle-favorite-at $id $ov))))
                (ov-keymap
                 (ov-set (ov-insert "\uf187")
                         'face (if (equal $archive :json-false)
                                   readability-icon-face-off
                                 readability-icon-face-on)
                         'rdb-bookmark-id $bookmark-id
                         'rdb-fav (if (equal $favorite :json-false) nil t))
                 "RET" (lambda () (interactive)
                         (let* (($ov (ov-at))
                                ($id (ov-val $ov 'rdb-bookmark-id)))
                           (readability--toggle-archive-at $id $ov))))
                (insert " ")
                (ov-keymap
                 (ov-set (ov-insert (assoc-default 'title $article))
                         'face '(:underline t)
                         'rdb-article-id $article-id)
                 "RET" (lambda () (interactive)
                         (readability--open-article (ov-val (ov-at) 'rdb-article-id)))
                 "o"   $fn-open-in-other-window
                 "O"   $fn-open-in-other-window
                 "C-o" (lambda () (interactive)
                         (let (($id (ov-val (ov-at) 'rdb-article-id))
                               ($window (selected-window)))
                           (other-window 1)
                           (readability--open-article $id)
                           (select-window $window))))
                (insert "\n")))
            (assoc-default 'bookmarks $articles)))
    (goto-char (point-min))
    (forward-char 3)
    (switch-to-buffer (current-buffer))
    (read-only-mode 1)
    (use-local-map readability-map-common)))


(provide 'readability)
;;; readability.el ends here
