data List a = Nil | Cons a (List a);
data Entry k v = Entry k v;
data Map k v = Map (List (Entry k v));

:f: a -> Int -> List a;
f x 0 = Nil;
f x n = Cons x (f x (n .- 1));

:makeEntry: k -> v -> Entry k v;
makeEntry k v = Entry k v;

:makeMap: List k -> List v -> Map k v;
makeMap Nil Nil = Map Nil;
makeMap (Cons k ks) (Cons v vs) = Cons (makeEntry k v) (makeMap ks vs);
