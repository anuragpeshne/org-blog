;;; ox-article.el --- Article Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Author: Anurag Peshne <anurag.peshne AT gmail DOT com>
;; Keywords: org, blog, article

;;; Commentary:
;;
;; This library implements a simple article back-end, derived from HTML back-end.
;; Used for exporting org file as article in blog.

;;; Code:
(require 'ox-html)

(org-export-define-derived-backend 'blog-html 'html
                                   :translate-alist '((template . blog-html-template)))

(defun blog-html-template (contents info)
  (let* ((org-html--build-head
         (lambda (info) (concat
                         (org-html--build-head info)
                         "<link rel=\"canonical\" href=\"https://blog.example.com/dresses/green-dresses-are-awesome\" />"
                         ))))
    (org-html-template contents info)))

;;; End-user functions
;;;###autoload
(defun org-blog-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a blog article."
  (interactive)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    ;; export to html use article
    (org-export-to-file
        'blog-html file subtreep visible-only body-only ext-plist)))

(provide 'ox-article)

;; Local variables:
;; generated-autoload-file:
;; End:

;;; ox-article.el ends here
