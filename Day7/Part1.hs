import Day7

jackCompare :: Hand -> Hand -> Ordering
jackCompare h1@(Hand cs1) h2@(Hand cs2) = (category h1, cs1) `compare` (category h2, cs2)

main = run jackCompare
