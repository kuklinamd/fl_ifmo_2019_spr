data EitherBI = Right Bool -- If everything okey
			  | Left Int   -- If everything bad
 			  | Middle;    -- Sometimes we don't care

-- :succ: Int -> Int
-- succ x = x + 1

:main: Int;	-- Return type
main = let x = Middle in -- Let it be!
  case (x) of {
	| Right n -> n
	| Left n -> F
	| Middle -> T
 -- | Just x -> F
  };
