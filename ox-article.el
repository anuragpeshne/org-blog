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
  (concat
   "<!DOCTYPE html>\n"
   (format "<html lang=\"%s\">\n" (plist-get info :language))
   "<head>\n"
   (format "<meta charset=\"%s\">\n"
           (coding-system-get org-html-coding-system 'mime-charset))
   (format "<title>%s</title>\n"
           (org-export-data (or (plist-get info :title) "") info))
   (format "<meta name=\"author\" content=\"%s\">\n"
           (org-export-data (plist-get info :author) info))
   "</head>\n"
   "<body>\n"
   (format "<h1 class=\"title\">%s</h1>\n"
           (org-export-data (or (plist-get info :title) "") info))
   contents
   "</body>\n"
   "</html>\n"))

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
