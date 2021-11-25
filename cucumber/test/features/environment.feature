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

Feature: Environment
  In order to use the enviroment variables
  Programmers must be able to write and read environment variables

  Scenario: Write then read
    Given that I start the test
    When I set the variable as "environment" into the environment
    # Comment! =)
    Then the variable should have "environment" on its content

  Scenario: E-mail variables
    When I set the  as e-mail marcotmarcot@gmail.com into the environment
    Then the  should have e-mail marcotmarcot@gmail.com on its content
