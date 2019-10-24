-module(bool).
-export([
    b_not/1,
    b_and/2,
    b_or/2,
    b_xor/2
]).

b_not(true) ->
    false;
b_not(false) ->
    true;
b_not(_) ->
    error.

b_and(true, true) ->
    true;
b_and(false, _) ->
    false;
b_and(_, false) ->
    false;
b_and(_, _) ->
    error.

b_or(true, _) ->
    true;
b_or(_, true) ->
    true;
b_or(false,false) ->
    false;
b_or(_, _) ->
    error.

b_xor(true, false) ->
    true;
b_xor(false, true) ->
    true;
b_xor(false, false) ->
    false;
b_xor(true, true) ->
    false;
b_xor(_, _) ->
    error.


