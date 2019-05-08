data EitherBI = Left Bool | Right Int | Middle;

:f: EitherBI -> Int;
f (Left b) = if b then -1 else 1;
f (Right n) = n + 2;
f Middle = 42;

:g: Int -> EitherBI;
g x = if x == 0
      then Left F
      else if x > 10 
           then Right 50
           else Middle;

:main: Int;
main = let x = Middle
	   in case (g (f x)) of {
			| (Right n) -> n 
			| (Left b) -> 0
			| Middle -> -1
		};
