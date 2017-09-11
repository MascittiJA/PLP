import Diccionario
import Data.Maybe
import Arbol23
import Test.HUnit
import Data.Char

--Este módulo sirve para utilizar el diccionario sin acceder a su estructura interna. Pueden agregar otras funciones o casos de prueba.

{- Función a implementar. -}


búsquedaDelTesoro::Eq a=>a->(a->Bool)->Diccionario a a->Maybe a 
búsquedaDelTesoro pista esTesoro dicc = 
    if (esTesoro pista) then 
      Just pista 
    else
      case (obtener pista dicc) of 
        Just z -> búsquedaDelTesoro z esTesoro dicc
        Nothing -> Nothing

{-
type Estr clave valor = Arbol23 (clave,valor) clave
data Diccionario clave valor = Dicc {cmp :: Comp clave, estructura :: Maybe (Estr clave valor)}

iterate :: (a -> a) -> a -> [a]
example: iterate (2*) 1 = [1,2,4,8,16,32,64...

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b 
(>>=) :: [a] -> (a -> [b]) -> [b]

ejemplo (>>=):
[x*2 | x<-[1..10], odd x] es lo mismo que:
[1..10] >>= (\x -> if odd x then [x*2] else [])

obtener::Eq clave=>clave->Diccionario clave valor->Maybe valor
-}

{- Diccionarios de prueba: -}

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8" ~: testsEj8,
  "ejercicio9" ~: testsEj9,
  "ejercicio10" ~: testsEj10
  ]

testsEj2 = test [
  True ~=? (esHoja (Hoja 28)),
  False ~=? (esHoja arbolito1),
  False ~=? (esHoja arbolito2),
  False ~=? (esHoja arbolito3),
  False ~=? (esHoja arbolito4),
  [0,1,2,3,4,5,6,7] ~=? internos arbolito1,
  "abcdefghi" ~=? hojas arbolito1,
  [True,False,True] ~=? internos arbolito2,
  [-1,0,-2,4] ~=? (hojas arbolito2),
  [1,2,3,2,3,4,3,4,5,4] ~=? take 10 (hojas arbolito3),
  "plgrapnde" ~=? (internos arbolito4),
  [5,2,0,1,12,-3,4,9,20,7] ~=? (hojas arbolito4)
  ]

testsEj3 = test [
  [0,1,-1,5] ~=? hojas (incrementarHojas arbolito2)
  ]

testsEj4 = test [
  [1,2,3,2,3,4,3,4,5,4,5,6,0,0,0,0,0] ~=? hojas (truncar 0 6 arbolito3)
  ]

testsEj5 = test [
  22 ~=? evaluar (truncar 0 6 arbolito3)
  ]

testsEj6 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
--  [] ~=? claves (vacio (<))
  ]

testsEj7 = test [
  [1] ~=? claves (definir 1 111 (vacio (<))),
  [1,2] ~=? claves (definir 2 222 (definir 1 111 (vacio (<))))
  ]

testsEj8 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
--   (Just 222)  ~=? obtener (definir 2 222 (definir 1 111 (vacio (<))))
  ]
  
testsEj9 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]
  
testsEj10 = test [
  Just "alfajor" ~=? búsquedaDelTesoro "inicio" ((=='a').head) dicc2
  ]
