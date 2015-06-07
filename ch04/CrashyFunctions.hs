-- this example is bad because it uses length, which walks the whole list. O(N)
-- if xs is really long this function will struggle, and if xs is infinite, then
-- it'll loop forever.
dumbSafeHead :: [a] -> Maybe a
dumbSafeHead xs = if length xs > 0
                  then Just (head xs)
                  else Nothing

-- this is a safer example. it uses null, which is O(1) and thus safe even if
-- xs is infinite
smartSafeHead :: [a] -> Maybe a
smartSafeHead xs = if not (null xs)
                   then Just (head xs)
                   else Nothing

-- alternatively, we can use pattern matching to write our own head
otherSafeHead :: [a] -> Maybe a
otherSafeHead (x:_) = Just x
otherSafeHead [] = Nothing


main = putStrLn "library file does nothing"
