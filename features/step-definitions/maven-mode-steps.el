;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have \"\\(.+\\)\"$"
       (lambda (something)
         ;; Do something
         ))

(When "^I have \"\\(.+\\)\"$"
      (lambda (something)
        ;; Do something
        ))

(Then "^maven-mode should be active$"
      (lambda ()
        (assert (eq maven-mode t))))

(And "^I have \"\\(.+\\)\"$"
     (lambda (something)
       ;; Do something
       ))

(But "^I should not have \"\\(.+\\)\"$"
     (lambda (something)
       ;; Do something
       ))
