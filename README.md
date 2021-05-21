Symbolic Differentiation

We use prefix notation

The expression format is (func arg1) or (op arg1 arg2) where op means operator,
func means function and arg1, arg2 are aguments to the operator or function.
For example (+ x 1) or (cos x)

The expressions will always have balanced parenthesis and with spaces between list items.

Expression operators, functions and arguments will all be lowercase.

Expressions are single variable expressions using x as the variable.

Expressions can have nested arguments at any depth for example (+ (* 1 x) (* 2 (+ x 1)))

Examples of prefix notation in this format:

(+ x 2)        // prefix notation version of x+2

(* (+ x 3) 5)  // same as 5 * (x + 3)

(cos (+ x 1))  // same as cos(x+1)

(^ x 2)        // same as x^2 meaning x raised to power of 2
The operators and functions you are required to implement are + - * / ^ cos sin tan exp ln where ^ means raised to power of. exp is the exponential function (same as e^x) and ln is the natural logarithm (base e).

Example of input values and their derivatives:

(* 1 x) => 1

(^ x 3) => (* 3 (^ x 2))

(cos x) => (* -1 (sin x))
In addition to returning the derivative your solution must also do some simplifications of the result
but only what is specified below.

The returned expression should not have unecessary 0 or 1 factors.
For example it should not return (* 1 (+ x 1)) but simply the term (+ x 1) similarly it should
not return (* 0 (+ x 1)) instead it should return just 0

Results with two constant values such as for example (+ 2 2) should be evaluated and returned as a
single value 4

Any argument raised to the zero power should return 1 and if raised to 1 should return the same
value or variable. For example (^ x 0) should return 1 and (^ x 1) should return x

No simplifactions are expected for functions like cos, sin, exp, ln...
(but their arguments might require a simplification).

Check out the tests package to see what's possible