;;; Generate the user guide .html file from .org source

(require 'cl-seq)
(require 'org)
(print (emacs-version))

;; Generates the HTML version of the user guide


(defun my-export (path desc backend &rest ignore)	; later versions of emacs add an info argument?
  (let* ((protocol (symbol-name (if (eq backend 'html) 'http backend)))
	 (url (concat protocol ":" path))
	 (label (or desc url)))
    (format "<a target=\"_blank\" href=\"%s\">%s</a>" url label)))

(nconc (cl-find "https" org-link-parameters :key #'car :test 'equal)
       '(:export my-export))

(nconc (cl-find "http" org-link-parameters :key #'car :test 'equal)
       '(:export my-export))


(defun gen-file (f)
  (princ (format "Generating %s" f))
  (terpri)
  (with-current-buffer
      (find-file f)
    (org-html-export-to-html)))

;;; TOD be smarter or use org publishing mechanisms https://orgmode.org/manual/Publishing.html
(gen-file "doc/user-guide.org")
(gen-file "doc/general.org")
(gen-file "doc/web-ui.org")
(gen-file "doc/cli.org")
(gen-file "doc/release3.org")

