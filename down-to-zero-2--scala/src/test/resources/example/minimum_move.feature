
Feature: Find minimum move

  Scenario Outline: Find minimum move
    Given the number <input>
    Then I get <minimum move>
    Examples:
      | input | minimum move |
      |     0 |            0 |
      |     1 |            1 |
