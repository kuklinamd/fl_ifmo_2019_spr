data EitherBI = Right Bool | Left Int | Middle;
:main: Int;
main = let x = Middle in case (x) of {| Right n -> n | Left n -> F | Middle -> T};
