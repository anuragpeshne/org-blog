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
  :translate-alist '((template . blog-html-template)
                     (src-block . codefy-src-block)))

(defun codefy-src-block (src-block contents info)
  (let* ((pre-string (org-html-src-block src-block contents info))
            (codefied-opening-string (replace-regexp-in-string
                                      "^<pre class=\"src src-"
                                      "<pre><code class=\"src language-"
                                      pre-string))
            (codefied-opening-string (replace-regexp-in-string
                                      "</pre>"
                                      "</code></pre>"
                                      codefied-opening-string)))
    codefied-opening-string))

(setq org-html-htmlize-output-type 'nil)
(setq org-html-link-home  "https://anuragpeshne.github.io")
(setq org-html-doctype "html5")
(defun blog-html-template (contents info)
  (let* ((orig-org-html--build-head (symbol-function 'org-html--build-head))
        (location-file-pair (split-string (plist-get info :input-file) "org-files" ))
        (location (car location-file-pair))
        (html-file-path (replace-regexp-in-string
                         "\.org"
                         "\.html"
                         (car (cdr location-file-pair))))
        (contents (concat
                   contents
                   (if (string-match "essays" html-file-path)
                       (creative-commons-license-code))
                   (if (string-match "essays" html-file-path)
                       (blog-html-get-disqus-comment-code (concat
                                                           org-html-link-home
                                                           html-file-path)
                                                          html-file-path))
                   "")))
    (cl-letf (((symbol-function 'org-html--build-head)
               (lambda (info)
                 (concat
                  (funcall orig-org-html--build-head info)
                  (format "<link rel=\"canonical\" href=\"%s\" />\n"
                          (concat org-html-link-home html-file-path))
                  (get-prismjs-headers)))))
      (org-html-template contents info))))

(defun get-prismjs-headers ()
  (concat
   "<link href=\"/css/prism.css\" rel=\"stylesheet\" />"
   "<script src=\"/js/prism.js\"></script>"))

(defun creative-commons-license-code ()
  (concat
   "<a rel=\"license\" href=\"http://creativecommons.org/licenses/by/4.0/\">"
   "<img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by/4.0/80x15.png\" />"
   "</a>"))

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

;; End:

;;; ox-article.el ends here
