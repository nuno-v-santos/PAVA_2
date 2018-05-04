#lang racket
(provide add-active-token def-active-token process-string)

#|Define functions:

add-active-token, that takes a string describing an active token and a function that should be triggered
when that token is found. This last function takes a string and returns a string. The function
add-active-token stores the association between the token and the corresponding function.

process-string, that takes a string and returns a string and is responsible for triggering the active token
actions, transforming the string until no more active tokens are found. To this end, whenever an active
token is found, the associated function is called with the substring that starts immediately after the token,
and the result of that function replaces the substring that starts with the token.


Def macro:

def-active-token, that takes an active token, a parameter list, and a body and that expands into an
appropriate use of the function add-active-token.
|#