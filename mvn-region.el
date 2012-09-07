;;;; mvn-region.el --- manipulating regions of text

(defun mvn-region-select-xml-tag (tagname)
  "Select all text inside xml tag named TAGNAME including the tag
itself. Operates on current buffer"
  (save-excursion 
    (let* ((start-tag (concat "<" tagname ">"))
           (start (- (search-forward-regexp start-tag) (length start-tag)))
           (end (search-forward-regexp (concat "</" tagname ">"))))
      (buffer-substring-no-properties start end))))

(defun mvn-region-kill-xml-tag-contents (tagname)
  "Delete everything between opening and closing tagname"
  (save-excursion 
    (let* ((start-tag (concat "<" tagname ">"))
           (end-tag (concat "</" tagname ">"))
           (start-exists (search-forward-regexp start-tag nil t))
           (end-exists (search-forward-regexp end-tag nil t)))
      (if (and start-exists end-exists)
          (let ((start start-exists)
                (end (- end-exists (length end-tag))))
            (kill-region start end))))))

(provide 'mvn-region)
