Overview
--------
estring is a set of erlang string functions

Installation
------------
    git clone git://github.com/dweldon/estring.git
    cd estring && make

Functions
---------
### begins_with
* begins_with(String, SubString)

returns true if String begins with SubString.

    estring:begins_with("fancy pants", "fancy").
    true

### ends_with
* ends_with(String, SubString)

returns true if String ends with SubString.

    estring:ends_with("fancy pants", "pants").
    true

### contains
* contains(String, SubString)

returns true if String contains SubString.

    estring:contains("los angeles", "angel").
    true

### edit_distance
* edit_distance(String1, String2)
* edit_distance(String1, String2, CaseInsensitive)

returns the damerau-levenshtein edit distance between String1 and String2.
edit_distance/2 will call edit_distance/3 with CaseInsensitive == false.

    estring:edit_distance("cars", "bats").
    2
    estring:edit_distance("receive", "RECIEVE", true).
    1

### similarity
* similarity(Source, Target)
* similarity(Source, Target, CaseInsensitive)
* similarity(Source, Target, CaseInsensitive, LowerLimit)

returns a score representing how similar Source is to Target based on the edit
distance and the length of Target. similarity/2 will call similarity/3 with
CaseInsensitive == false. similarity/4 is a short circuit performance
enhancement. a simple heuristic is used to estimate the upper bound for
similarity between the Source and Target strings. if the estimate exceeds
LowerLimit, then {error, limit_reached} is returned immediately. otherwise
{ok, Score} or {error, limit_reached} is returned based on a call to
similarity/3.

    estring:similarity("linux", "ilnuxx").
    0.8
    estring:similarity("linux", "Linux", true).
    1.0
    estring:similarity("linux", "microsoft", false, 0.5).
    {error,limit_reached}

### strip
* strip(String)

returns a string, where leading and trailing whitespace (" ",\n\t\f\r) has been
removed. note that string:strip/1 only removes spaces.

    estring:strip("\t  clean me   \r\n").               
    "clean me"

### strip_split
* strip_split(String, SeparatorString)

helper function - less verbose way of calling:
`re:split(estring:strip(String), SeparatorString, [{return, list}])`

    estring:strip_split("\tfirst,second,third\n", ",").
    ["first","second","third"]

### squeeze
* squeeze(String)
* squeeze(String, Char)

returns a string where runs of the same character are replaced by Char.
Char can be a string or an integer, e.g. "e" or $e. squeeze/1 will call
squeeze/2 with " " for Char.

    estring:squeeze("i need   a  squeeze!").
    "i need a squeeze!"
    estring:squeeze("the cow says moooo", "o").
    "the cow says mo"

### is_integer
* is_integer(String)

returns true if String is is a string representation of an integer.

    estring:is_integer("35").
    true
    estring:is_integer("35.4").
    false

### format
* format(Format, Data)

helper function - less verbose way of calling:
`lists:flatten(io_lib:format(Format, Data))`

    estring:format("~w bottles of ~s on the wall", [99, "beer"]).
    "99 bottles of beer on the wall"

### random
* random(N)

returns a random alphanumeric string of length N.

    estring:random(32).
    "LzahJub1KOMS0U66mdXHtHyMMXIdxv1t"
