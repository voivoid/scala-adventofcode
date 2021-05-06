# Advent of Code in Scala

Advent of Code ( https://adventofcode.com ) 2015-2020 solutions using pure-functional scala

Prerequisites
-----

* mill

Build
-----

    $ mill app.compile

Tests
-----

    $ mill problems.test       # quick unit-tests check
    $ mill -j 8 app.testAll    # test all problems

Solve Problem
-----

    $ mill app.problem 2015_01_1
    $ mill app.problem 2015_01_2

Sources
-----

Day | 2015 | 2016 | 2017 | 2018 | 2019 | 2020 |
--- | ---- | ---- | ---- | ---- | ---- | ---- |
01  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/01.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/01.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/01.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/01.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/01.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/01.scala) |
02  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/02.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/02.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/02.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/02.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/02.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/02.scala) |
03  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/03.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/03.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/03.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/03.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/03.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/03.scala) |
04  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/04.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/04.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/04.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/04.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/04.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/04.scala) |
05  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/05.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/05.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/05.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/05.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/05.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/05.scala) |
06  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/06.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/06.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/06.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/06.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/06.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/06.scala) |
07  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/07.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/07.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/07.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/07.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/07.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/07.scala) |
08  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/08.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/08.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/08.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/08.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/08.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/08.scala) |
09  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/09.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/09.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/09.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/09.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/09.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/09.scala) |
10  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/10.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/10.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/10.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/10.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/10.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/10.scala) |
11  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/11.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/11.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/11.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/11.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/11.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/11.scala) |
12  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/12.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/12.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/12.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/12.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/12.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/12.scala) |
13  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/13.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2016/13.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/13.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2018/13.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/13.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/13.scala) |
14  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/14.scala) |      | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/14.scala) |      | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2019/14.scala) | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2020/14.scala) |
15  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/15.scala) |      | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/15.scala) |      |      |      |
16  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/16.scala) |      | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/16.scala) |      |      |      |
17  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/17.scala) |      | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/17.scala) |      |      |      |
18  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/18.scala) |      | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/18.scala) |      |      |      |
19  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/19.scala) |      | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/19.scala) |      |      |      |
20  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/20.scala) |      | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/20.scala) |      |      |      |
21  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/21.scala) |      | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/21.scala) |      |      |      |
22  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/22.scala) |      | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2017/22.scala) |      |      |      |
23  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/23.scala) |      |      |      |      |      |
24  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/24.scala) |      |      |      |      |      |
25  | [✓](https://github.com/voivoid/scala-adventofcode/blob/master/problems/src/2015/25.scala) |      |      |      |      |      |
