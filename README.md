# Programming Principles, SNU 4190.210, 2017 Spring

- Instructor: Prof. [Chung-Kil Hur](http://sf.snu.ac.kr/gil.hur)
- TA: [Yoonseung Kim](http://sf.snu.ac.kr/yoonseung.kim/)
    + Office: Bldg 301 Rm 416
    + Email address: [pp201701@sf.snu.ac.kr](mailto:pp201701@sf.snu.ac.kr)

## Announcements

- 03/02: Lecture slide is uploaded [here](lecture.pdf).
- Lecture on March 7th will be a practice session. Click [this](https://github.com/snu-sf-class/pp201701/issues/1) for more information
- Old announcements are hidden (go to https://raw.githubusercontent.com/snu-sf-class/pp201701/master/README.md to check them)
<!--
- 03/16: [HW1](assignments/hw1) is out. (Due date: 3/24(FRI) 23:59) [Instruction for submission](HWInstruction.md)
- 03/27: [HW1 solution](assignments-sol/hw1) is available. You can check your score at the server. For questions, email TA.
- 03/30: [HW2](assignments/hw2) is out. (Due date: 4/7(FRI) 23:59)
- 04/06: Midterm Exam schedule: April 23, 2pm ~ (no time limit) [Instruction for midterm](MidInstruction.md)
- 04/07: [HW3](assignments/hw3) is out. (Due date: 4/14(FRI) 23:59)
- 04/11: [HW2 solution](assignments-sol/hw2) is available.
- 04/18: Class on Thursday(04/20) is a QnA session for midterm.
- 04/18: Midterm coverage: until 'Structural subtypes' [Instruction for midterm](MidInstruction.md)
- 04/18: [HW3 solution](assignments-sol/hw3) is available.
- 04/20: [Instruction for midterm](MidInstruction.md) updated. More description of problems added.
- 04/25: [HW3 solution](assignments-sol/hw3) is revised, thanks to a student's report.
- 04/25: Lectures canceled on these days: [4/27(Thu), 5/9(Tue), 5/16(Tue), 5/18(Thu)]
- 04/27: [HW4](assignments/hw4) is out. (Due date: 5/5(FRI) 23:59)
- 05/03: [HW4](assignments/hw4) has changed little: the type of IterDictImpl.empty is changed from IterDict[K,V] to IterDictImpl[K,V]. (This is just for clarification. Not necessary to apply this to your homework, if you are already done without problems.)
- 05/11: [HW5](assignments/hw5) is out. (Due date: 5/19(FRI) 23:59)
-->
- 05/11: Now [midterm result](http://147.46.219.145:8102) and [additional reduction](https://github.com/snu-sf-class/pp201701/issues/45) is valid. Claim until May 21 (Sun)
- 05/25: Final Exam schedule: June 16, 6:30pm ~ 11:30pm
- 05/29: [Project](project) is out. (~~Due date: 6/22(Thu) 23:59~~) Read the instruction [proj.pdf](project/proj.pdf) carefully.
- 06/01: Two lectures are left - 6/8(Thu) & 6/13(Tue). No class on 6/15. Final exam will cover materials taught until 6/8.
- 06/01: Two homework assignments left. ~~HW6(6/1 ~ 6/8) & HW7(6/8 ~ 6/15)~~
- 06/01: HW6 is out. (Due date: 6/8(Thu) 23:59)
- 06/06: Check new [HW3 score](https://github.com/snu-sf-class/pp201701/issues/52)! Send email to TA for questions/claims.
- 06/08: ~~Homework deadline changed: HW7(6/8 ~ 6/14(Wed))~~
- 06/08: Homework deadline changed: HW6(6/1 ~ 6/9(Fri))
- 06/08: [HW7](assignments/hw7) is out!
- 06/13: HW7 deadline postponed to 6/15(Thu) 23:59. For questions about HW7, visit TA in 14:00 ~ 16:00 of 6/14 (Wed), (Place: 301-416).
- 06/14: Fixed confusing comments in HW7/Test.scala. This doesn't alter the assignment.
- 06/14: [HW6 solution](assignments-sol/hw6) uploaded.
- 06/14: [Midterm Prob6 solution](assignments-sol/mid6) uploaded, by request. Not related to the Final exam.
- 06/16: [HW7 solution](assignments-sol/hw7) is uploaded. (Reminder: this is just one of the possible ways to solve this problem.)
- 06/21: Fixed a typo in the project document. (see [here](https://github.com/snu-sf-class/pp201701/issues/70)for more information.)
- 06/21: [`proj-lib2116.jar`](https://github.com/snu-sf-class/pp201701/tree/master/project/lib) for Scala 2.11.6 is uploaded. If you're using Scala 2.11.6, download the new file and change the name into `proj-lib.jar`
- 06/22: Project due date postponed to 06/25(Sun) 23:59.
- 06/26: Check your final score [here](http://sf.snu.ac.kr:8103)
- 06/26: Your final grade ~~will be~~ IS NOW individually announced on the [HW submission page](http://sf.snu.ac.kr:8101) ~~tomorrow~~. Check details from [here](https://github.com/snu-sf-class/pp201701/issues/83). The claim period is until next Tuesday.
- 06/29: Check the [project solution](project-sol/) and the [final exam solution](final-sol/).
- 06/29: Check the UPDATED grade on the [HW submission page](http://sf.snu.ac.kr:8101) 

##  Lecture slide updates (since 04/20)

|No. | Published     |
|----|------------	|
| 1 | 04/20     	|
| 2 | 04/25     	|
| 3 | 05/02     	|
| 4 | 05/07		|
| 5 | 05/24		|
| 6 | 05/25             |
| 7 | 06/03             |
| 8 | 06/08             |
| 9 | 06/13             |
| 10 | 06/15             |
| - | -             |

## Assignments
- Download skeleton code and replace `???` with your code
- No delayed submission
- The score is automatically uncovered right after the due date.
- Claims: within 2 weeks from the due date, please.

|No. | Published     | Due       	| Description                   	 	 	 	 	 	 	 	 	 	  	|
|----|------------	|------------	|----------------------	|
| 1 | 03/16     	|03/24 23:59    | Recursion                                                            	|
| 2 | 03/30     	|04/07 23:59    | Algebraic Data Types                                                            	|
| 3 | 04/07     	|04/14 23:59    | Polymorphism                                                            	|
| 4 | 04/27     	|05/05 23:59    | Abstract classes                                                            	|
| 5 | 05/11     	|05/19 23:59    | Trait Mixin                                                            	|
| 6 | 06/01     	|~~06/08~~ 06/09 23:59    | Typeclasses                                                            	|
| 7 | 06/08     	|~~06/14~~ 06/15 23:59    | Typeclass into OO                                                            	|
| prj | 05/29     	|~~06/22~~ 06/25 23:59    | Project                                                            	|
| - | -             | -             | - |

## Must Read

- *READ VERY CAREFULLY* this section.

### Questions

- Ask questions in the [GitHub repository issue tracker](https://github.com/snu-sf-class/pp201701/issues).
    + When you post a piece of source code, use ["fenced code blocks" feature](https://help.github.com/articles/creating-and-highlighting-code-blocks/)
      * Like this:
      ```
      def foo(x:Int) = x + 3
      foo(10)
      ```
    + Or, you can always use [GitHub Gist](https://gist.github.com/).
- Send email for *PERSONAL MATTERS ONLY*.

### Scala
- Latest version: [2.12.1](https://www.scala-lang.org/)
- Use IDEs supporting Scala.
    + IntelliJ IDEA
    + Eclipse (works only for Scala 2.11.8, which is older)
    + Emacs: [Ensime](https://github.com/ensime).

#### Honor Code: *DO NOT CHEAT*
