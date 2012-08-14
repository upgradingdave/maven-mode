(require 'json)

(defvar mvn-search-maven-org-url "http://search.maven.org")
(defvar mvn-results-buffer "*mvn-results*")

;;;; commands 

(defun mvn-search (search-term)
  "Search for artifact by search term"
  (interactive "MSearch: ")
  (mvn-setup-buffer)
  (mapcar (lambda (x) 
            (insert (concat 
                     (mvn-groupId-from-alist x) ":"
                     (mvn-artifactId-from-alist x) ":"
                     (mvn-latest-version-from-alist x) "\n")))
          (mvn-json-to-vector (mvn-get-search-json search-term))))

(defun mvn-search-versions (coord)
  "Search for coord (groupId:artifactId) for complete list of versions"
  (interactive "MGroupId:ArtifactId: ")
  (let ((artifactId (cadr (split-string coord ":")))
        (groupId (car (split-string coord ":"))))
    (mvn-setup-buffer)
    (mapcar (lambda (x) 
              (insert (concat 
                       (mvn-groupId-from-alist x) ":"
                       (mvn-artifactId-from-alist x) ":"
                       (mvn-version-from-alist x) "\n")))
            (mvn-json-to-vector (mvn-get-version-json groupId artifactId)))))

;;; buffer management

(defun mvn-setup-buffer ()
  (switch-to-buffer (get-buffer-create mvn-results-buffer))
  (maven-mode 1)
  (erase-buffer))

;;;; Get json results 

(defun mvn-get-search-json (search-term)
  "Get json with list of results"
  (mvn-get-json 
   (concat mvn-search-maven-org-url "/solrsearch/select?q=%22" search-term "%22&rows=20&wt=json")))

(defun mvn-get-version-json (groupId artifactId)
  "Get json with details for groupId"
  (mvn-get-json 
   (concat mvn-search-maven-org-url "/solrsearch/select?q=g:%22" groupId 
           "%22+AND+a:%22" artifactId "%22&core=gav&rows=20&wt=json")))

(defun mvn-get-json (url)
  "Make a request, get response, strip response headers, pass back json"
  (let* ((buffer 
          (url-retrieve-synchronously url))
         (json (concat "{" (with-current-buffer buffer
                             (goto-char (point-min))
                             (substring (buffer-string) (- (search-forward "{") 1))))))
    (kill-buffer buffer)
    json))

(defun mvn-json-to-vector (json)
  "Takes json from search and returns list of artifacts"
  (let ((json-object-type 'alist))
    (cdr (assoc 'docs (assoc 'response (json-read-from-string json))))))

;;;; Getters

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

;;;; KeyMap

(defvar maven-mode-map (make-sparse-keymap)
  "maven-mode keymap")

(define-key maven-mode-map 
  (kbd "C-c m s") 'mvn-search)

(define-key maven-mode-map 
  (kbd "C-c m d") 'mvn-search-versions)

(define-minor-mode maven-mode
  "Maven Mode"
  nil
  " mvn"
  maven-mode-map)

(provide 'maven-mode)


