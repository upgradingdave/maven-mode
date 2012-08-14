(require 'json)

(defun mvn-search-maven-org (search-term)
  "Search http://search.maven.org for an artifact"
  (interactive "MSearch: ")
  (let* ((json (mvn-get-search-results search-term))
         (first-result (elt (mvn-get-list-from-search-results json) 0)))
    (insert (cdr (mvn-get-coords first-result)))))

(defun mvn-get-search-results (search-term)
"Make search request, parse out response headers and return json"
  (let* ((buffer 
          (url-retrieve-synchronously 
           (concat "http://search.maven.org/solrsearch/select?q=" search-term "&rows=20&wt=json")))
         (json (concat "{" (with-current-buffer buffer
                             (goto-char (point-min))
                             (substring (buffer-string) (- (search-forward "{") 1))))))
    (kill-buffer buffer)
    json))

(defun mvn-get-list-from-search-results (json)
  "Takes json from search and returns list of artifacts"
  (let ((json-object-type 'alist))
    (cdr (assoc 'docs (assoc 'response (json-read-from-string json))))))

(defun mvn-get-coords (alist)
  "Given a alist representing a maven artifact, return coords"
  (assoc 'id alist))

(defvar maven-mode-map (make-sparse-keymap)
  "maven-mode keymap")

(define-key maven-mode-map 
  (kbd "C-c m s") 'mvn-search-maven-org)

;; (define-key maven-mode-map 
;;   (kbd "C-c m m") 'mvn-simple-command)

;; (defun mvn-simple-command (in)
;;   (interactive "MIn: ")
;;   (message "%s" in))

(define-minor-mode maven-mode
  "Maven Mode"
  nil
  " mvn"
  maven-mode-map)

(provide 'maven-mode)


