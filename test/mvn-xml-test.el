(ert-deftest mvn-test-xml-parse-node-text ()
  (let ((groupNode '(groupId nil "org.springframework")))
    (should (string-equal "org.springframework" 
                          (mvn-xml-parse-node-text groupNode)))))

(ert-deftest mvn-test-xml-parse-first-child ()
  (let ((dependencyNode '(dependency nil "\n        "
                                     (groupId nil "org.springframework")
                                     "\n        "
                                     (artifactId nil "spring-context")
                                     "\n        "
                                     (version nil "${spring.version}")
                                     "\n      ")))
    (should (equal '(groupId nil "org.springframework") 
                   (mvn-xml-parse-first-child-node dependencyNode 'groupId)))))

(ert-deftest mvn-test-xml-parse-coord ()
  (let ((dependencyNode '(dependency nil "\n        "
                                     (groupId nil "org.springframework")
                                     "\n        "
                                     (artifactId nil "spring-context")
                                     "\n        "
                                     (version nil "${spring.version}")
                                     "\n      ")))
    (should (equal "org.springframework:spring-context:${spring.version}" 
                   (mvn-xml-parse-coord dependencyNode)))))

(ert-deftest mvn-test-parse-tag ()
  (with-temp-buffer
    (insert-file-contents "files/parent-pom.xml")
    (should (equal 17656 (point-max)))
    (goto-char (point-min))
    (let ((dependencies (mvn-xml-parse-tag "dependencies")))
      (should (equal (caar dependencies) 'dependencies))))
  (with-temp-buffer
    (should-not (mvn-xml-parse-tag "dependencies"))))

(ert-deftest mvn-test-xml-parse-managed-dependencies ()
  (with-temp-buffer
    (insert-file-contents "files/parent-pom.xml")
    (let ((dependencies (mvn-xml-parse-managed-dependencies)))
      (should (equal 26 (length dependencies)))))
  (with-temp-buffer
    (should-not (mvn-xml-parse-managed-dependencies))))

(ert-deftest mvn-test-xml-parse-dependencies ()
  (with-temp-buffer
    (insert-file-contents "files/parent-pom.xml")
    (let ((dependencies (mvn-xml-parse-dependencies)))
      (should (equal 4 (length dependencies)))))
  (with-temp-buffer
    (should-not (mvn-xml-parse-dependencies))))

(ert-deftest mvn-test-xml-parse-next-dependencies ()
  (with-temp-buffer
    (insert-file-contents "files/parent-pom.xml")
    (let* ((first-pos (search-forward-regexp "</dependencies>"))
           (dependencies (mvn-xml-parse-next-dependencies (point-min)))
           (dependencies2 (mvn-xml-parse-next-dependencies first-pos)))
      (should (equal 26 (length dependencies)))
      (should (equal 4 (length dependencies2))))))

(ert-deftest mvn-test-xml-parse-coords ()
  (with-temp-buffer
    (insert-file-contents "files/parent-pom.xml")
    (should (equal (mvn-xml-parse-coords 
                    (mvn-xml-parse-managed-dependencies))
                   '("jstl:jstl:${jstl.version}" "org.eclipse.jetty:jetty-servlet:${jetty.version}" "javax.servlet:servlet-api:${javax.servlet.version}" "org.springframework:spring-web:${spring.version}" "org.springframework:spring-core:${spring.version}" "org.springframework:spring-context:${spring.version}" "org.springframework:spring-beans:${spring.version}" "org.apache.maven:maven-plugin-tools-api:${maven-plugin-api.version}" "org.apache.maven:maven-artifact:${maven-plugin-api.version}" "org.apache.maven:maven-plugin-api:${maven-plugin-api.version}" "org.apache.maven:maven-project:${maven-plugin-api.version}" "org.jruby:jruby:${jruby.version}" "org.jruby:jruby-complete:${jruby.version}" "jline:jline:${jline.version}" "com.beust:jcommander:1.26" "com.google.code.gson:gson:${gson.version}" "commons-lang:commons-lang:${commons-lang.version}" "com.google.guava:guava:${quava.version}" "com.unboundid:unboundid-ldapsdk:${unboundid-ldapsdk.version}" "javax.mail:mail:${javax-mail.version}" "org.apache.derby:derby:${derby.version}" "org.slf4j:jcl-over-slf4j:${org.slf4j.version}" "org.slf4j:slf4j-log4j12:${org.slf4j.version}" "org.slf4j:slf4j-api:${org.slf4j.version}" "org.springframework:spring-test:${spring.version}" "junit:junit:${junit.version}")))))

(ert-deftest mvn-test-xml-sort-parsed-dependencies ()
  (with-temp-buffer
    (insert-file-contents "files/parent-pom.xml")
    (should (equal (subseq (mvn-xml-sort-parsed-dependencies 
                            (mvn-xml-parse-managed-dependencies))
                         0 4)
                   '((dependency nil "\n        "
                                 (groupId nil "junit")
                                 "\n        "
                                 (artifactId nil "junit")
                                 "\n        "
                                 (version nil "${junit.version}")
                                 "\n        "
                                 (scope nil "test")
                                 "\n      ")
                     (dependency nil "\n        "
                                 (groupId nil "org.springframework")
                                 "\n        "
                                 (artifactId nil "spring-test")
                                 "\n        "
                                 (version nil "${spring.version}")
                                 "\n        "
                                 (scope nil "test")
                                 "\n      ")
                     (dependency nil "\n        "
                                 (groupId nil "com.beust")
                                 "\n        "
                                 (artifactId nil "jcommander")
                                 "\n        "
                                 (version nil "1.26")
                                 "\n      ")
                     (dependency nil "\n        "
                                 (groupId nil "com.google.code.gson")
                                 "\n        "
                                 (artifactId nil "gson")
                                 "\n        "
                                 (version nil "${gson.version}")
                                 "\n      "))))))
  
(provide 'mvn-xml-test)
