
Feature: Build company hierarchical structure

  Scenario: Only one employee
    Given "1" is superior of "2"
    Then I see the structure: Pyramid(1,2)
