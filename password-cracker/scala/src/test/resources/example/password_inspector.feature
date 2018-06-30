
Feature: Password check

  Scenario Outline: Login attempt can be broken down into passwords
    Given the following passwords are registered in the system: <registered passwords>
    When I try to login with the password <login attempt>
    Then I should get the following passwords that can construct the password I entered: <passwords used>
    Examples:
      | login attempt              | registered passwords                | passwords used                           |
      | password                   | password                            | password                                 |
      | password1                  | password1, password2                | password1                                |
      | attempt                    | att, empt                           | att, empt                                |
      | attempt                    | empt, att                           | att, empt                                |
      | attempt                    | at, pt, tem                         | at, tem, pt                              |
      | pass                       | pa, s                               | pa, s, s                                 |
      | wedowhatwemustbecausewecan | because, can, do, must, we, what    | we, do, what, we, must, because, we, can |
      | abcd                       | ab, abcd, cd                        | ab, cd                                   |
      | zfzahm                     | ozkxyhkcst, xvglh, hpdnb, zfzahm    | zfzahm                                   |
      | gurwgrb                    | gurwgrb, maqz, holpkhqx, aowypvopum | gurwgrb                                  |

  Scenario Outline: Login attempt cannot be broken down into passwords
    Given the following passwords are registered in the system: <registered passwords>
    When I try to login with the password <login attempt>
    Then I should be unable to get passwords that can construct the password I entered
    Examples:
      | login attempt | registered passwords                                                      |
      | attempt       | password                                                                  |
      | helloworld    | hello, planet                                                             |
      | aaaaaaaaaab   | a, aa, aaa, aaaa, aaaaa, aaaaaa, aaaaaaa, aaaaaaaa, aaaaaaaaa, aaaaaaaaaa |
