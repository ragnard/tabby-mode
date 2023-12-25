;;; -*- lexical-binding: t; -*-
;;; tabby-mode.el --- Minor mode for the Tabby AI coding assistant.

;;; Commentary:

;; Author: Ragnar Dahlén

;;; Code

(defgroup tabby nil
  "Minor mode for using the Tabby AI coding assistant"
  :link '(url-link "htps://tabby.tabbyml.com")
  :group 'programming)

(defcustom tabby-api-url "https://localhost:8080/"
  "URL to Tabby API."
  :type 'string
  :group 'tabby)

(defun tabby--completions-url ()
  (format "%s/v1/completions" (string-remove-suffix "/" tabby-api-url)))

(defun tabby--completions-request (lang prefix suffix)
  "TODO."
  (json-encode
   `((language . ,lang)
     (segments . ((prefix . ,prefix)
                  (suffix . ,suffix))))))

(defun tabby--get-completions (buffer lang prefix suffix callback)
  "TODO."
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/json")))
        (url-request-data (tabby--completions-request lang prefix suffix)))
    (url-retrieve (tabby--completions-url)
                  (lambda (status)
                    (goto-char url-http-end-of-headers)
                    (let ((response (json-read)))
                      (funcall callback buffer response))))))

(defun tabby--handle-completion-response (buffer response)
  "TODO."
  (let* ((choices (mapcar (lambda (c)
                            (alist-get 'text c))
                          (alist-get 'choices response)))
         (text (completing-read "Tabby: " choices)))
    (when text
      (with-current-buffer buffer
        (insert text)))))

(defun tabby-complete ()
  "Ask Tabby for completion suggestions on the text around point."
  (interactive)
  (let ((prefix (buffer-substring (point-min) (point)))
        (suffix (when (< (point) (point-max))
                  (buffer-substring (+ (point) 1) (point-max))))
        (lang "go"))
    (tabby--get-completions (current-buffer) lang prefix suffix 'tabby--handle-completion-response)))

(define-minor-mode tabby-mode
  "A mode for Tabby")

(provide 'tabby-mode)
