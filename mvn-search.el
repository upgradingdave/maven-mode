;;;; mvn-search.el --- do searches against search.maven.org

(defvar mvn-search-maven-org-url "http://search.maven.org")
(defvar mvn-results-buffer "*mvn-results*")

;;;; Getters
;; The Search functions below convert json to vectors. Use these
;; getters to extract info from the vectors

(defun mvn-coords-from-alist (alist)
  "Given a alist representing a maven artifact, return coords"
  (cdr (assoc 'id alist)))

(defun mvn-artifactId-from-alist (alist)
  "Given a alist representing a maven artifact, return artifactId"
  (cdr (assoc 'a alist)))

(defun mvn-groupId-from-alist (alist)
  "Given a alist representing a maven artifact, return groupId"
  (cdr (assoc 'g alist)))

(defun mvn-latest-version-from-alist (alist)
  "Given a alist representing a maven artifact, return version"
  (cdr (assoc 'latestVersion alist)))

(defun mvn-version-from-alist (alist)
  "Given a alist representing a maven artifact, return version"
  (cdr (assoc 'v alist)))

;;;; Search functions

(defun mvn-search-get-json (url)
  "Make a request, get response, strip response headers, pass back json"
  (let* ((buffer 
          (url-retrieve-synchronously url))
         (json (concat "{" (with-current-buffer buffer
                             (goto-char (point-min))
                             (substring (buffer-string) (- (search-forward "{") 1))))))
    (kill-buffer buffer)
    json))

(defun mvn-search-by-term (search-term)
  "Do search against search.maven.org for SEARCH-TERM"
  (mvn-search-get-json 
   (url-generic-parse-url 
    (concat mvn-search-maven-org-url "/solrsearch/select?q=\"" 
            search-term "\"&rows=20&wt=json"))))

(defun mvn-search-for-versions (groupId artifactId)
  "Given GROUPID and ARTIFACTID, return list of all available versions"
  (mvn-search-get-json 
   (url-generic-parse-url
    (concat mvn-search-maven-org-url "/solrsearch/select?q=g:\"" groupId 
            "\"+AND+a:\"" artifactId "\"&core=gav&rows=20&wt=json"))))

(defun mvn-search-json-to-vector (json)
  "Convert json into a vector. Use getters to extract info from vector"
  (let ((json-object-type 'alist))
    (cdr (assoc 'docs (assoc 'response (json-read-from-string json))))))

(defun mvn-search-completing-groupIds (search-term)
  "Search for artifact by search term"
  (interactive "MSearch: ")
  (let ((groupIds 
         (mapcar (lambda (x) 
                   (concat 
                    (mvn-groupId-from-alist x) ":"
                    (mvn-artifactId-from-alist x)))
                 (mvn-search-json-to-vector (mvn-search-by-term search-term)))))
    (completing-read "Choose artifact:" groupIds nil t search-term)))

(defun mvn-search-completing-versions (coord)
  "Search for all versions of coord <groupId:artifactId>"
  (interactive "MSearch: ")
  (let* ((artifactId (cadr (split-string coord ":")))
         (groupId (car (split-string coord ":")))
         (versions 
          (mapcar (lambda (x) 
                    (concat 
                     (mvn-groupId-from-alist x) ":"
                     (mvn-artifactId-from-alist x) ":"
                     (mvn-version-from-alist x) ))
                  (mvn-search-json-to-vector (mvn-search-for-versions groupId artifactId)))))
    (completing-read "Choose Version: " versions nil t coord)))


(provide 'mvn-search)
