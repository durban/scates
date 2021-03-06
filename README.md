<!--

   Copyright 2016-2019 Daniel Urban and contributors listed in AUTHORS

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

--->

# scates

Experiments with statically checking state machines.

- [`IxFree`](src/main/scala/com/example/IxFree.scala) is a stack-safe indexed free monad implementation.
  - Examples from the Lambda Days 2019 presentation:
    - [free monad](src/test/scala/com/example/presentation/FreeExample.scala) version
    - [indexed state monad](src/test/scala/com/example/presentation/IxStateExample.scala) version
    - [indexed free monad](src/test/scala/com/example/presentation/IxFreeExample.scala) version
  - Presentation [slides](https://github.com/dilation/lambda-days/raw/master/state_machine_talk.pdf) (PDF)
- [`Sm`](src/main/scala/com/example/Sm.scala) is a more capable utility for working with FSMs, built on `IxFree`.
- [`St`](src/main/scala/com/example/St.scala) is an experiment with an alternative representation (currently it doesn't work).

## License

*scates* is open source software under the [Apache License v2.0](https://www.apache.org/licenses/LICENSE-2.0).
For details, see the [LICENSE.txt](LICENSE.txt), [NOTICE.txt](NOTICE.txt), and [AUTHORS](AUTHORS) files.
