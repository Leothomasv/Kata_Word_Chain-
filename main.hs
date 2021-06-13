module Main where

import Data.Char
import Data.List
import System.IO
import System.Exit

main :: IO ()
main = do
  file <- openFile "wordlist.txt" ReadMode
  hSetEncoding file latin1
  contents <- hGetContents file
  let arrWord = lines contents

  putStrLn "Ingrese primer palabra: "
  pal1 <- getLine
  putStrLn "Ingrese segunda palabra: "
  pal2 <- getLine

  let tamano = compareSize pal1 pal2
  if tamano
    then print "Same size"
    else if not tamano
      then exitWith (ExitFailure 43)
      else print "Nada se mando"

  --creo el arreglo ordenado con las palabras del tamano correcto
  let arregloFiltrado = init (ordenarArreglo arrWord (length pal2))

  print "Word Chain"
  print (buscarPalabras arregloFiltrado pal1 pal2 0 arregloFiltrado)

compareSize :: String -> String -> Bool
compareSize x y = length x == length y

ordenarArreglo :: [String] -> Int -> [String]
ordenarArreglo (x : xs) tam = do
  if not (null xs) && length x == tam
    then x : ordenarArreglo xs tam
    else
      if not (null xs) && length x /= tam
        then ordenarArreglo xs tam
        else return []

cantidadSimilares :: String -> String -> [Ordering]
cantidadSimilares = zipWith compare

--arreglar estas ->
--transformar de tipo ordering a tipo int
cantidadIguales :: [Ordering] -> [Int]
cantidadIguales (x : xs) = do
  if x == EQ
    then 1 : cantidadIguales xs
    else
      if x /= EQ
        then 0 : cantidadIguales xs
        else return 0
cantidadIguales _ = []

--buscar las palabras para formar la lista
buscarPalabras :: [String] -> String -> String -> Int -> [String] -> [String]
buscarPalabras (x : xs) primera segunda n arreglo = do

  let cant1 = sum (cantidadIguales (cantidadSimilares primera x))
  let cant2 = sum (cantidadIguales (cantidadSimilares segunda x))

  if not (null xs) && (cant1 == (length primera - 1)) && (cant2 == n)
    then if x == segunda then return x else x : buscarPalabras xs x segunda (n + 1) arreglo
    else buscarPalabras xs primera segunda n arreglo

buscarPalabras _ primera segunda n arreglo = buscarPalabras arreglo primera segunda n arreglo
