;;; ox-article.el --- Article Back-End for Org Export Engine -*- lexical-binding: t; -*-

;; Author: Anurag Peshne <anurag DOT peshne AT gmail DOT com>
;; Keywords: org, blog, article

;;; Commentary:
;;
;; This library implements a simple article back-end, derived from HTML back-end.
;; Used for exporting org file as article in blog.

;;; Code:
(require 'ox-html)
(require 'cl-lib)

(org-export-define-derived-backend 'blog-html 'html
                                   :translate-alist '((template . blog-html-template)))

(defun blog-html-template (contents info)
  (let* ((orig-org-html--build-head (symbol-function 'org-html--build-head))
        (site-url "https://anuragpeshne.github.io")
        (location-file-pair (split-string (plist-get info :input-file) "org-files" ))
        (location (car location-file-pair))
        (html-file-path (replace-regexp-in-string
                         "\.org"
                         "\.html"
                         (car (cdr location-file-pair))))
        (contents (concat
                   contents
                   (if (string-match "essays" html-file-path)
                       (blog-html-get-disqus-comment-code (concat
                                                           site-url
                                                           html-file-path)
                                                          html-file-path))
                   "")))
    (cl-letf (((symbol-function 'org-html--build-head)
               (lambda (info)
                 (concat
                  (funcall orig-org-html--build-head info)
                  (format "<link rel=\"canonical\" href=\"%s\" />\n"
                          (concat site-url html-file-path))))))
      (org-html-template contents info))))

(defun blog-html-get-disqus-comment-code (page-url page-identifier)
  (concat
   "<div id=\"disqus_thread\"></div>
<script>
    var disqus_config = function () {\n"
   (format "this.page.url = \"%s\";\n" page-url)
   (format "this.page.identifier = \"%s\";\n" page-identifier)
   "};
    (function() {  // DON'T EDIT BELOW THIS LINE
        var d = document, s = d.createElement('script');

        s.src = '//anuragpeshne.disqus.com/embed.js';

        s.setAttribute('data-timestamp', +new Date());
        (d.head || d.body).appendChild(s);
    })();
</script>
<noscript>Please enable JavaScript to view the "
   "<a href=\"https://disqus.com/?ref_noscript\" rel=\"nofollow\">"
   "comments powered by Disqus.</a></noscript>"))

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
