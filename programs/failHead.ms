data Nat = Z | S Nat;
data ListNat = Nil | Cons Nat ListNat;
data MaybeNat = Nothing | Just Nat;

:head: ListNat -> MaybeNat;
head Nil = Z ;
head (Cons hd tl) = Just hd;
