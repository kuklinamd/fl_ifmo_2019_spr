data EitherBI = Right Bool | Left Int | Middle;
:main: Bool;
main = let x = Middle
   in case (x) of {
		| Right n -> n
		| Left n -> F
		| Middle -> 10
    };
