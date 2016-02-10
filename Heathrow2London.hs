data Road = A Int | B Int | C Int deriving (Show)
type Path = ([Road], Int)
type Section = (Int, Int, Int)
--type RoadSystem = [Section]
-- 1. Only get the shortest path result
-- 1.1 Recusive
calcQuickestPathResult :: (Int, Int) -> [Section]  -> Int
calcQuickestPathResult (ra, rb) [] = min ra rb
calcQuickestPathResult (ra, rb) ((a, b, c):xs) = calcQuickestPathResult (sa, sb) xs
                     where sa = min (ra + a) (rb + b + c)
                           sb = min (rb + b) (ra + a + c)

calcResult :: [Section] -> Int
calcResult s = calcQuickestPathResult (0, 0) s


-- 1.2 Fold
step ::  (Int, Int) -> Section -> (Int, Int)
step (ra, rb) (a, b, c) = (min (ra + a) (rb + b + c), min (rb + b) (ra + a + c))

calcResult' :: [Section] -> Int
calcResult' sections = min ra rb where (ra, rb) = foldl step (0, 0) sections

-- 2 Calculate the Path
step' :: (Path, Path) -> Section -> (Path, Path)
step' ((aroads, ar), (broads, br)) (a, b, c) = (sar, sbr)
      where da = ar + a
            db = br + b
            la = db + c
            lb = da + c
            sar = if da < la then ((aroads ++ [A a]), da)
                             else ((broads ++ [B b, C c]), la)
            sbr = if db < lb then ((broads ++ [B b]), db)
                              else ((aroads ++ [A a, C c]), lb)

calcPath :: [Section] -> Path
calcPath sections = if snd apath < snd bpath then apath else bpath
         where (apath, bpath) = foldl step' (([], 0), ([], 0)) sections

main = do
   let sections = [(50, 10, 30),(5, 90, 20),(40, 2, 25),(10, 8, 0)]
   let r = calcResult' sections
   putStrLn $ "Just get the quickest path result, result is " ++ show r

   let x = calcPath sections
   putStrLn $ "Get the whole path, path is " ++ show x
