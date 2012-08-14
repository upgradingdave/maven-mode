Feature: Search Maven Repository
  In order to easily write powerful, maintainable java programs
  As a maven user using emacs
  I want to easily search for and find dependencies from maven repo
  
  Scenario: Search search.maven.org for jsoup
    When I turn on maven-mode
    And I start an action chain 
    And I press "C-c m s"
    And I type "jsoup"
    And I execute the action chain
    Then I should see "org.jsoup:jsoup"
