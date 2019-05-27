data Nat = Z | S Nat;

:lt: Nat -> Nat -> Bool;
lt Z Z = F ;
lt Z (S x) = T ;
lt (S x) (S y) = lt x y;
