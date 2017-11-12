
Feature: Password check

  Scenario: Login attempt can be broken down into passwords
    Given the following passwords are registered in the system: mypass, word
    When I try to login with the password mypassword
    Then I should get the following passwords that can construct the password I entered: mypass, word
