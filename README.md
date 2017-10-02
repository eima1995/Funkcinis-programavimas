# Funkcinis-programavimas 

Functional programming task 1
Exercise 1. Similarly as it was done for the function exOr (see the Lecture 2 slides), deﬁne in Haskell two diﬀerent versions of a function nAnd :: Bool −> Bool −> Bool, which returns the result True in all cases except the one when both arguments are True. Moreover, deﬁne the third version of the function that encodes the truth table for this mathematical function. In other words, the literal values True and False should be used instead of both function arguments, resulting in four diﬀerent deﬁnition cases (equations).

Exercise 2. Import the module Test.QuickCheck by adding the command ”import Test.QuickCheck” at the beginning of your module (after the keyword where). After that, test that all the three versions of nAnd from the previous exercise are functionally identical by deﬁning the corresponding properties of the type ::Bool−> Bool −> Bool and running quickCheck prop nAnd??? in the interpreter. Think of one more property that any implementation of nAnd must satisfy (for example, that it should return True if any of the arguments is False) and test it.

Exercise 3. The pre-deﬁned function length returns the size of a string (or any sequence). Relying on application of this function, deﬁne your own function nDigits ::Integer−>Int that takes any integer number and returns the number of its digits. To distinguish the cases of negative and natural numbers, use guards in your function deﬁnition.

Exercise 4. Write a function nRoots::Float−>Float−>Float−>Int which returns the number of solutions for a quadratic equation a∗x2 + b∗x + c = 0.0, for the given real coeﬃcients a,b, and c. Reminder: the quadratic equation has • two real roots, if b2 > 4.0∗a∗c, • one real root, if b2 = 4.0∗a∗c, • no real roots, if b2 < 4.0∗a∗c, provided that a 6= 0.0 Please distinguish diﬀerent deﬁnition cases using guards. In the case when a = 0.0, your function must return an error (using the pre-deﬁned error function), e.g., error "the first argument should be non-zero!".

Exercise 5. Using your solution the last exercise, deﬁne the functions smallerRoot::Float−>Float−>Float−>Float and largerRoot::Float−>Float−>Float−>Float, which respectively return the smaller and larger root of a quadratic equation, for the given real coeﬃcients a,b, and c. Reminder: the formula for the roots of a quadratic equation is (−b)±p(b2 −4∗a∗c) 2∗a In the case of one root, both functions should return the same result. In the case of no roots, the corresponding error message has to be returned.

Exercise 6. Using the primitive recursion mechanism, write a function power2::Integer−>Integer that calculates the power of 2 for the given natural number n, i.e., 2n. When a negative number is supplied as the parameter value, the function must return 0.

Exercise 7. Write a function mult::Integer−>Integer−>Integer that recursively redeﬁnes the multiplication operation by using only addition, i.e., m∗n = m + m + ... + m n times, or m∗n = n + n + ... + n m times. Use either the ﬁrst or second function argument for implementing primitive recursion, i.e. deﬁning the base and recursive cases. Please make sure that the function cases are exhaustive, i.e., it is deﬁned also for the cases when any argument is negative.

Exercise 8. Deﬁne a recursive function prod::Integer−>Integer−>Integer that, for the given numbers m and n, multiplies all the numbers from the range m..n. In other words, the produced result must be equal to m∗(m + 1)∗...∗(n−1)∗n Note that, to follow the primitive recursion pattern, it is not necessary to choose a single parameter in order to deﬁne the base and recursive cases. Any expression on the parameter values can be used instead, provided that, according to the pattern, this expression is checked to be equal to 0 for the base case, and be greater than 0 for the recursive case. Moreover, the recursive call should make this expression smaller by 1. An error message must be returned for an invalid range, i.e., when m > n. Finally, redeﬁne the factorial function (from the Lecture 2 slides) as a special case of prod.

Functional programming task 2
Exercise 1.
Define a function
average ::  [Float] -> Float,
which for a given number list returns their average value. (If you need to
convert an integer number into a float, use function fromIntegral.)
Exercise 2.
Write a function
divides ::  Integer -> [Integer],
which for any integer number returns a list of its divisors. Create two versions
of such a function, one based on recursion and the other one on the list comprehension method (see Lecture 5).
Relying on your divides implementation, define a function, which checks whether a given non-negative integer is a prime number.
Exercise 3.
Write a function
prefix ::  String -> String -> Bool,
which for any two given strings checks whether the first one is a prefix (i.e.,
coincides with the beginning) of the other one. Relying on your prefix implementation, define a function substring,
checking whether one given string is a part of another one.
Exercise 4.
Define a function
permut ::  [Integer] -> [Integer] -> Bool,
which checks that the two given lists of integers are permutations of each
other.  A permutation means that the lists consist of the same elements,
occuring the same number of times.

Exercise 5.
Using the
list comprehension
method, define a function
capitalise ::  String -> String,which modifies a given string by filtering (leaving only letters) and then
changing the found letters into capital ones.
Exercise 6.
A shop basket can be defined as the data structure
[(String,Float)], storing a pairs of items and their prices. Write two functions:
•itemTotal ::  [(String,Float)] -> [(String,Float)],which merges repeating items, summing their prices accordingly,
•itemDiscount ::  String -> Integer -> [(String,Float)] ->[(String,Float)], which applies a discount (the second parameter,
ranging from 0% until 100%) to a given item (the first parameter) andcorrespondingly modifies the prices of this item in a given shop basket(the third parameter).
