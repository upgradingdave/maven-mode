;;; mvn-mode.el --- Minor Mode for making working with Maven bearable

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

;; See README

;;; Code:

(require 'json)
(require 'xml-to-string "util/xml-to-string/xml-to-string.el")

(require 'mvn-search)
(require 'mvn-xml)

;;;; Setup

(defun mvn-enable-mode ()
  "Enable mvn-mode if current buffer is pom.xml"
  (if (string-match "pom\\.xml" (buffer-file-name))
      (mvn-mode t)))

(add-hook 'find-file-hook 'mvn-enable-mode)

;;;; Commands

(defun mvn-search-completing (search-term)
  "Do search, then choose groupId, then choose version"
  (interactive "MSearch: ")
  (mvn-insert-dependency-xml (mvn-search-completing-versions 
                              (mvn-search-completing-groupIds search-term))))

(defun mvn-insert-dependency-xml (coord)
  "Generate the dependency xml to insert into pom for coord <groupId:artifactId:version>"
  (interactive "MGroupId:ArtifactId:Version: ")
  (let ((groupId (car (split-string coord ":")))
        (artifactId (cadr (split-string coord ":")))
        (version (caddr (split-string coord ":")))
        (start (point)))
    (insert
     (message
      (concat "<dependency>\n" 
              "  <groupId>%s</groupId>\n"
              "  <artifactId>%s</artifactId>\n"
              "  <version>%s</version>\n"
              "</dependency>\n")
      groupId artifactId version))
    (indent-region start (point))))

(defun mvn-sort-all-dependencies ()
  (interactive)
  "Sort dependencies (and dependencyManagement) alphabetcially
by groupid and then by artifactid. Test dependencies are put at
the top of the list."
  (message "Sorting dependencies...")
  (goto-char (point-min))
  (let ((deps-pos (search-forward-regexp "<dependencies>"))
        (start-pos (point-min)))
    (while deps-pos
      (mvn-sort-next-dependencies start-pos deps-pos)
      (setq start-pos deps-pos)
      (setq deps-pos (search-forward-regexp "<dependencies>")))))

(defun mvn-sort-next-dependencies (start-pos deps-pos)
  (goto-char start-pos)
  (let ((dependencies (mvn-xml-parse-next-dependencies start-pos))
        (start (point))
        (result))
    (goto-char start-pos)
    (mvn-region-kill-xml-tag-contents "dependencies")
    (goto-char deps-pos)
    ;; sort, and then add new lines so it's formatted nicely
    (dolist (dependency (mvn-xml-sort-parsed-dependencies dependencies) result)
      (setq result (cons "\n" (cons dependency result ))))
    (insert (concat "\n" (xml-to-string-parse-child-node-list (nreverse result))))
    (indent-region (- (length "<dependencies>") start) 
                   (+ (length "</dependencies>") (point)))))

;;;; KeyMap

(defvar mvn-mode-map (make-sparse-keymap)
  "mvn-mode keymap")

(define-key mvn-mode-map 
  (kbd "C-c m ds") 'mvn-search-completing)

(define-key mvn-mode-map 
  (kbd "C-c m dx") 'mvn-insert-dependency-xml)

(define-key mvn-mode-map 
  (kbd "C-c m do") 'mvn-sort-all-dependencies)

(define-minor-mode mvn-mode
  "Maven Mode"
  nil
  " mvn"
  mvn-mode-map)

(provide 'mvn-mode)
