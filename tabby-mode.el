;;; -*- lexical-binding: t; -*-
;;; tabby-mode.el --- Minor mode for the Tabby AI coding assistant.

;;; Commentary:

;; Author: Ragnar Dahlén

;;; Code

(defgroup tabby nil
  "Minor mode for using the Tabby AI coding assistant"
  :link '(url-link "htps://tabby.tabbyml.com")
  :group 'programming)

(defcustom tabby-api-url nil
  "URL to Tabby API."
  :type 'string
  :group 'tabby)

(defcustom tabby-completion-function 'completing-read
  "Function to use when selection a completion"
  :type 'symbol
  :group 'tabby)

(defcustom tabby-mode-language-alist
  '((c-mode . "c")
    (c++-mode . "cpp")
    (go-mode . "go")
    (java-mode . "java")
    (javascript-mode . "javascript")
    (kotlin-mode . "kotlin")
    (python-mode . "python")
    (ruby-mode . "ruby")
    (rust-mode . "rust")
    (typescript-mode . "typescript")
    (yaml-mode . "yaml"))
  "Mapping from major mode to Tabby language identifier."
  :type '(alist :key-type symbol :value-type string)
  :group 'tabby)

(defun tabby--completions-url ()
  "Return the API url for completions."
  (format "%s/v1/completions" (string-remove-suffix "/" tabby-api-url)))

(defun tabby--completions-request (lang prefix suffix)
  "Build a completions request for LANG with PREFIX and SUFFIX."
  `((language . ,lang)
    (segments . ((prefix . ,prefix)
                 (suffix . ,suffix)))))

(defun tabby--get-completions (buffer lang prefix suffix callback)
  "Send a completions request to the Tabby API."
  (let* ((request (tabby--completions-request lang prefix suffix))
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/json")))
         (url-request-data (json-encode request)))
    (url-retrieve (tabby--completions-url)
                  (lambda (status)
                    (goto-char url-http-end-of-headers)
                    (let ((response (json-read)))
                      (funcall callback buffer response))))))

(defun tabby--handle-completion-response (buffer response)
  "Handle a completions RESPONSE for a BUFFER."
  (let* ((choices (mapcar (lambda (c)
                            (alist-get 'text c))
                          (alist-get 'choices response)))
         (text (funcall tabby-completion-function "Tabby: " choices)))
    (when text
      (with-current-buffer buffer
        (insert text)))))

(defun tabby--determine-language ()
  "Determine the language identifier for the current buffer.
See https://code.visualstudio.com/docs/languages/identifiers."
  (alist-get major-mode tabby-mode-language-alist))

(defun tabby-complete ()
  "Ask Tabby for completion suggestions on the text around point."
  (interactive)
  (when (not tabby-api-url)
    (error "Please configure the URL for your Tabby server. See customizable variable `tabby-api-url`."))
  (let* ((lang (tabby--determine-language))
         (prefix (buffer-substring (point-min) (point)))
         (suffix (when (< (point) (point-max))
                   (buffer-substring (+ (point) 1) (point-max)))))
    (if lang
        (tabby--get-completions (current-buffer) lang prefix suffix 'tabby--handle-completion-response)
      (message "Unable to determine language for current buffer."))))

(define-minor-mode tabby-mode
  "A mode for Tabby")

(provide 'tabby-mode)
