:g: Int -> Int;
g y = let x = 10 in x * y;

:f: Int;
f = let y = (let x = 10 in g x) in y * 2;
