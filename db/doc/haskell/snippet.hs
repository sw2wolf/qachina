
import Data.List

mean xs = tot / len
  where
    (tot, len) = foldl' fun (0, 0) xs
    fun (!tot, !len) x = (tot+x, len+1)

