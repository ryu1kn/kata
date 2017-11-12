
Feature: Password check

  Scenario Outline: Login attempt can be broken down into passwords
    Given the following passwords are registered in the system: <registered passwords>
    When I try to login with the password <login attempt>
    Then I should get the following passwords that can construct the password I entered: <passwords used>

    Examples:
      | login attempt | registered passwords | passwords used |
      | mypassword    | mypass, word         | mypass, word   |
      | password1     | password1, password2 | password1      |
