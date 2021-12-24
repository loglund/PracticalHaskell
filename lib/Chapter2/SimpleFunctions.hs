module Chapter2.SimpleFunctions where

firstOrEmpty :: [[Char]] -> [Char]
firstOrEmpty lst = if not (null lst) then head lst else "empty"

(+++) :: [a] -> [a] -> [a]
lst1 +++ lst2 =
  if null lst1 -- check emptyness
    then lst2 -- base case
    else head lst1 : (tail lst1 +++ lst2) --recursive call with rest of the list

reverse2 :: [a] -> [a]
reverse2 list =
  if null list
    then []
    else reverse2 (tail list) +++ [head list]

maxmin :: Ord b => [b] -> (b, b)
maxmin list =
  let h = head list
   in if null (tail list)
        then (h, h)
        else
          ( if h > t_max
              then h
              else t_max,
            if h < t_min
              then h
              else t_min
          )
  where
    t = maxmin (tail list)
    t_max = fst t
    t_min = snd t
