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

Feature: Don't match prefixes
  In order to write scenarios without worrying
  Programmers must be able to write steps that are prefix of others

  Scenario: Prefixes
    When I add 1
    And I add 1 2 times
    Then the result should be 3
