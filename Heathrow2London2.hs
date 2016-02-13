import Control.Monad.Writer
data Road = A Int | B Int | C Int deriving (Show)
type Section = (Int, Int, Int)

astep :: Int -> Int  -> Section -> Writer [Road] Int
astep ar br (a, b, c) = writer (sar, roads)
      where da = ar + a
            db = br + b
            la = db + c
            (sar, roads) = if da < la then (da, [A a])
                                    else (la, [B b, C c])

bstep :: Int -> Int  -> Section -> Writer [Road] Int
bstep ar br (a, b, c) = writer (sbr, roads)
      where da = ar + a
            db = br + b
            lb = da + c
            (sbr, roads) = if db < lb then (db, [B b])
                                    else (lb, [A a, C c])


calcPath :: Int -> Int -> [Section] -> Writer [Road] Int
calcPath a b [] = if a < b then return a else return b
calcPath a b (x:xs) = do
            aw <- astep a b x
            bw <- bstep a b x
-- !!! This is the problem as calcPath takes two monads, then the [Road] will do mappend on not only aw but also bw
            calcPath aw bw xs

main = do
   let sections = [(50, 10, 30),(5, 90, 20),(40, 2, 25),(10, 8, 0)]
   let x = calcPath 0 0 sections
   putStrLn $ "Get the whole path, path is " ++ show x
