;;;; functions for traversing and parsing xml

(require 'xml)
(require 'xml-to-string)

(require 'mvn-region)

;;;; Parsing functions
;; The following functions assume args have been parsed by
;; xml-parse-region (or similar) method

(defun mvn-xml-parse-tag (tagname &optional start-from)
  "Select all text inside xml tag named TAGNAME including the tag
itself and parse it into s expressions. START-FROM can be used to
search from specific locaiton in buffer"
  (save-excursion 
    (if start-from
        (goto-char start-from))
    (let* ((start-tag (concat "<" tagname ">"))
           (end-tag (concat "</" tagname ">"))
           (start-exists (search-forward-regexp start-tag nil t))
           (end-exists (search-forward-regexp end-tag nil t)))
      (if (and start-exists end-exists)
          (let ((start (- start-exists (length start-tag)))
                (end end-exists))
            (xml-parse-region start end))))))

(defun mvn-xml-parse-node-text (node)
  "Return text inside node"
  (car (xml-node-children node)))

(defun mvn-xml-parse-first-child-node (node child-tag-name)
  "Return first child node that matches child-tag-name.
CHILD-TAG-NAME should be quoted like 'description"
  (car (xml-get-children node child-tag-name)))

(defun mvn-xml-parse-coord (dependency)
  "Returns coordinate string in the form
`groupId:artifactId:version'. DEPENDENCY is a lisp representation
of xml from one of the xml-parse methods"
  (let ((artifactId (mvn-xml-parse-node-text 
                     (mvn-xml-parse-first-child-node dependency 'artifactId)))
        (groupId (mvn-xml-parse-node-text (mvn-xml-parse-first-child-node dependency 'groupId)))
        (version (mvn-xml-parse-node-text (mvn-xml-parse-first-child-node dependency 'version))))
    (concat groupId ":" artifactId ":" version)))

(defun mvn-xml-parse-coords (dependencies)
  "Create list of coords found in dependencyManagement section
of a pom. POM is a file path string to the pom.xml file"
  (let* ((value))
    (dolist (dependency dependencies value)
      (setq value (cons (mvn-xml-parse-coord dependency)
                        value)))))

(defun mvn-xml-sort-parsed-dependencies (dependencies)
  "Alphabetically sort list of depdendencies by groupid, and then
artifactid. Test scope dependencies are put first. DEPENDENCIES
should be a list of parsed dependency xml nodes"
  (let* ((value))
    (sort dependencies 
          (lambda (a b)
            (let ((coord1 (mvn-xml-parse-coord a))
                  (scope1 (mvn-xml-parse-node-text (mvn-xml-parse-first-child-node a 'scope)))
                  (coord2 (mvn-xml-parse-coord b))
                  (scope2 (mvn-xml-parse-node-text (mvn-xml-parse-first-child-node b 'scope))))
              ;;if one scope is test but the other isn't, the one with
              ;;test scope is less than the other
              (cond
               ((and (string-equal "test" scope1) 
                     (not (string-equal scope1 scope2)))
                t)
               ((and (string-equal "test" scope2) 
                     (not (string-equal scope1 scope2)))
                nil)
               (t
                (string-lessp coord1 coord2))))))))

;;;; Extract xml tags 
;; The following are convenience functions for extracting bits of xml
;; out of pom.xml

(defun mvn-xml-parse-managed-dependencies ()
  "Get list of dependencies from dependencyManagement section.
Returns NIL if no dependencyManagement section is found."
  (save-excursion
    (goto-char (point-min))
    (let ((dependencies 
           (xml-get-children (car (mvn-xml-parse-tag "dependencyManagement")) 'dependencies)))
      (xml-get-children (car dependencies) 'dependency))))

(defun mvn-xml-parse-dependencies ()
  "Get list of dependencies. Note that this doesn't get
dependencies inside dependencyManagement section"
  (save-excursion
    (let ((contents (buffer-string)))
      (with-temp-buffer
        (insert contents)
        (goto-char (point-min))
        (mvn-region-kill-xml-tag-contents "dependencyManagement")
        (goto-char (point-min))
        (xml-get-children (car (mvn-xml-parse-tag "dependencies")) 'dependency)))))

(defun mvn-xml-parse-next-dependencies (&optional start-from)
  "Get the next list of dependencies found. Either
project->dependencies or
project-dependencyManagement->dependencies"
  (xml-get-children (car (mvn-xml-parse-tag "dependencies" start-from)) 'dependency))


(provide 'mvn-xml)
