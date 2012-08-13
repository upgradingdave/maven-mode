Feature: Search Maven Repository
  In order to easily write powerful, maintainable java programs
  As a maven user using emacs
  I want to easily search for and find dependencies from maven repo
  
  Scenario: Prompt for search
    When I press "C-c m s"
    And I type "jsoup"
    Then I should see "jsoup:1.0-VERSION"

