# Assignment 1: Programming Principles, SNU 4190.210

## Restrictions 

The first assignment restricts to use some syntax of Scala.

**WARNING: Please read the restrictions below carefully.** 

If you do not follow it, **your submission will not be graded.**

1. Do not use keyword `var`. Use `val` and `def` instead.
2. Do not use any library functions or data structures like `List`, `Array` `Range` (`1 to n`, `1 until n` ...), `fold`, `map`, `reduce` or etc... 
   You can only use tuples, `scala.annotation.tailrec`, and `scala.util.control.TailCalls._` from library.

Again, if you do not follow these rules, your score will be zero. 


## Grading 

Each exercise will be tested by two sets of inputs. 

The first set of inputs limits its range that a usual imperative program would not raise stack overflow error.

From the second set of inputs, the range will be extended to occur stack overflow. If you want to avoid it, use the proper method that you are learned in the class. 

For all three problems, 50% of the test cases will require tail call optimizations (ie, have large inputs) and the other 50% will not (ie, have small inputs).

So, we will get 50% of the score if you submit a correct program without tail call optimization.

## Q&A

If you have any questions, please submit an issue to [pp202002 issue tracker](https://github.com/snu-sf-class/pp202002/issues).

## Submissions

TBD





