data Ord = GT | LT | EQ;

:ord: Int -> Int -> Ord;
ord x y = if x > y then GT else if x == y then EQ else LT;
