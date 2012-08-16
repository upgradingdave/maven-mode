;;; maven-mode.el --- Minor Mode for making working with Maven bearable

;; Copyright (C) 2012 Dave Paroulek

;; Author: Dave Paroulek <upgradingdave@gmail.com>
;; Maintainer: Dave Paroulek <upgradingdave@gmail.com>
;; Version: 0.1.0
;; Keywords: maven, java
;; URL: http://github.com/upgradingdave/maven-mode

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'json)

(defvar mvn-search-maven-org-url "http://search.maven.org")
(defvar mvn-results-buffer "*mvn-results*")

;;;; commands 

(defun mvn-search-completing (search-term)
  "Do search, then choose groupId, then choose version"
  (interactive "MSearch: ")
  (mvn-insert-dependency-xml (mvn-search-completing-versions (mvn-search-completing-groupIds search-term))))

;;;; api 

(defun mvn-search-completing-groupIds (search-term)
  "Search for artifact by search term"
  (interactive "MSearch: ")
  (let ((groupIds 
         (mapcar (lambda (x) 
                   (concat 
                    (mvn-groupId-from-alist x) ":"
                    (mvn-artifactId-from-alist x)))
                 (mvn-json-to-vector (mvn-get-search-json search-term)))))
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
                  (mvn-json-to-vector (mvn-get-version-json groupId artifactId)))))
    (completing-read "Choose Version: " versions nil t coord)))

;; (defun mvn-test-completing (search-term)
;;   (interactive "MSearch: ")
;;   (insert
;;    (completing-read "Choose Version: "
;;     '("foo-1.0" "foo-2.0" "bar-2.0" "bar-3.0") nil t 
;; (completing-read "Choose Lib: " '("foo" "bar" "baz") nil t  search-term))))

;; (defun mvn-test-completing (search-term)
;;   (interactive "MSearch: ")
;;   (insert
;;    (completing-read "Choose Version: "
;;     '("foo-1.0" "foo-2.0" "bar-2.0" "bar-3.0") nil t search-term)))

(defun mvn-insert-dependency-xml (coord)
  "Generate the dependency xml to insert into pom for coord <groupId:artifactId:version>"
  (interactive "MGroupId:ArtifactId:Version: ")
  (let ((groupId (car (split-string coord ":")))
        (artifactId (cadr (split-string coord ":")))
        (version (caddr (split-string coord ":"))))
    (insert
     (message
      (concat "<dependency>\n" 
              "  <groupId>%s</groupId>\n"
              "  <artifactId>%s</artifactId>\n"
              "  <version>%s</version>\n"
              "</dependency>\n")
      groupId artifactId version))))

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
  (kbd "C-c m sd") 'mvn-search-completing)

(define-key maven-mode-map 
  (kbd "C-c m dx") 'mvn-search-completing)

(define-minor-mode maven-mode
  "Maven Mode"
  nil
  " mvn"
  maven-mode-map)

(provide 'maven-mode)


