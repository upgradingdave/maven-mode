Feature: Search Maven Repository
  In order to easily write powerful, maintainable java programs
  As a maven user using emacs
  I want to easily search for and find dependencies from maven repo
  
  Scenario: Generate and insert dependency xml snippet
    When I turn on mvn-mode
    And I start an action chain
    And I press "C-c m dx"
    And I type "log4j:log4j:1.1.3"
    And I execute the action chain
    Then I should see:
    """
    <dependency>
    <groupId>log4j</groupId>
    <artifactId>log4j</artifactId>
    <version>1.1.3</version>
    </dependency>
    """
  Scenario: Search maven with completing help
    Given I turn on mvn-mode
    When I start an action chain
    And I press "C-c m ds"
    And I type "log4j"
    And I press "RET"
    And I press "RET"
    And I type "1.2.17"
    And I press "RET"
    And I execute the action chain
    Then I should see:
    """
    <dependency>
    <groupId>log4j</groupId>
    <artifactId>log4j</artifactId>
    <version>1.2.17</version>
    </dependency>
    """

  Scenario: Enable maven minor mode when visiting pom.xml
    When I open temp file "pom.xml"
    Then mvn-mode should be active

