Feature: Search Maven Repository
  In order to easily write powerful, maintainable java programs
  As a maven user using emacs
  I want to easily search for and find dependencies from maven repo
  
  Scenario: Search maven for an artifact
    When I turn on maven-mode
    And I start an action chain 
    And I press "C-c m s"
    And I type "jsoup"
    And I execute the action chain
    Then I should be in buffer "*mvn-results*"
    And I should see "org.jsoup:jsoup"

  Scenario: Retrieve versions of a group:artifact
    When I turn on maven-mode
    And I start an action chain
    And I press "C-c m d"
    And I type "org.slf4j:log4j-over-slf4j"
    And I execute the action chain
    Then I should be in buffer "*mvn-results*"
    And I should see "1.6.3"
