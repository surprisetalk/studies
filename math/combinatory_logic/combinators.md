
TODO: define (.)

blackbird : (a -> b) -> (c -> d -> a) -> c -> d -> b
blackbird = (.) (.) (.)

TODO: write this more interesting
flip3 : (a -> b -> c -> d) -> c -> a -> b -> d
flip3 = (curry <<< flip) << uncurry
