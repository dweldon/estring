Overview
--------
estring is a string manipulation library for erlang

Installation
------------
    git clone git://github.com/dweldon/estring.git
    cd estring && make

Interface
---------
The following examples give an overview of the estring interface. Please see the
complete documentation by running `make doc`.

### Matching
    > estring:begins_with("fancy pants", "fancy").
    true

    > estring:contains("los angeles", "angel").
    true

    > estring:ends_with("fancy pants", "shoes").
    false

### Comparison
    > estring:edit_distance("theater", "theatre").
    1

    > estring:similarity("speling mistaeks", "spelling mistakes").
    0.8823529411764706

### IO Helpers
    > estring:strip_split(" first<@>second<@>third \r\n", "<@>").
    ["first","second","third"]

    > estring:format("~w bottles of ~s on the wall", [99, "beer"]).
    "99 bottles of beer on the wall"

### Miscellaneous
    > estring:is_integer("35").
    true

    > estring:squeeze("I  have    too  many   spaces!").
    "I have too many spaces!"

    > estring:rot13("All your base are belong to us!").
    "Nyy lbhe onfr ner orybat gb hf!"

    > estring:random(20).
    "B0EYJOkOGkC6FjTcZqlR"
