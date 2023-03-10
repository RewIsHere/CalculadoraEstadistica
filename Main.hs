import Data.List
import System.IO

-- Función que calcula la media
media :: [Double] -> Double
media xs = sum xs / fromIntegral (length xs)

-- Función que calcula la medianaa
mediana :: [Double] -> Double
mediana xs =
  let sorted = sort xs
      n = length sorted
   in if odd n
        then sorted !! (n `div` 2)
        else (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2

-- Función que calcula la moda
moda :: [Double] -> [Double]
moda xs =
  let grouped = group $ sort xs
      freqs = map length grouped
      maxFreq = maximum freqs
   in map fst $ filter (\(_, f) -> f == maxFreq) $ zip (map head grouped) freqs

-- Función que calcula la desviación estándar
-- stddev :: [Double] -> Double
-- stddev xs =
-- let m = media xs
--  variance = media $ map (\x -> (x - m) ^ 2) xs
-- in sqrt variance

-- Función principal que recibe la entrada del usuario y muestra los resultados
main :: IO ()
main = do
  putStrLn "Ingrese una lista de numeros separados por espacios:"
  input <- getLine
  let nums = map read $ words input :: [Double]
  putStrLn $ "Media: " ++ show (media nums)
  putStrLn $ "mediana: " ++ show (mediana nums)
  putStrLn $ "Moda: " ++ show (moda nums)

-- putStrLn $ "Desviación estándar: " ++ show (stddev nums)