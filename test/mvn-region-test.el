(ert-deftest mvn-test-region-kill-xml-tag-contents ()
  (should-not (mvn-region-kill-xml-tag-contents "dependencies")))
