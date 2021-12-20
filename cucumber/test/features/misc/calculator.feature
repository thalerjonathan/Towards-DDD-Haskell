# Copyright 2012 Marco Túlio Pimenta Gontijo <marcotmarcot@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Of course it needs calc!
@needs_calc
@nice_feature
Feature: Division
  In order to avoid silly mistakes
  Cashiers must be able to calculate a fraction

  Scenario: Regular numbers
    Given that I have entered 2 into the calculator
    # Comment! =)
    And that I have entered 3 into the calculator
    When I press divide
    Then the result should be 1.5 on the screen
