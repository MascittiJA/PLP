module Diccionario (Diccionario, vacio, definir, definirVarias, obtener, claves,cmp,estructura) where

import Data.Char
import Data.Maybe
import Data.List
import Arbol23

{- Definiciones de tipos. -}

type Comp clave = clave->clave->Bool
type Estr clave valor = Arbol23 (clave,valor) clave

data Diccionario clave valor = Dicc {cmp :: Comp clave, estructura :: Maybe (Estr clave valor)}
--El comparador es por menor.

{- Funciones provistas por la cátedra. -}

--Inserta un nuevo par clave, valor en una estructura que ya tiene al menos un dato.
insertar::clave->valor->Comp clave->Estr clave valor-> Estr clave valor
insertar c v comp a23 = interceptar (insertarYPropagar c v comp a23) id (\s1 (c1, s2)->Dos c1 s1 s2)

--Maneja el caso de que la segunda componente sea Nothing.
interceptar ::(a,Maybe b)->(a->c)->(a->b->c)->c
interceptar (x,y) f1 f2 = case y of
                   Nothing -> f1 x
                   Just z -> f2 x z

{- Inserta una clave con su valor correspondiente. Si se actualiza el índice, el cambio se propaga hacia arriba
   para mantener balanceado el árbol.
   Usamos recursión explícita porque este tipo de recursión no es estructural (no se aplica a todos los hijos). -}
insertarYPropagar::clave->valor->Comp clave->Estr clave valor-> (Estr clave valor, Maybe (clave, Estr clave valor))
insertarYPropagar c v comp a23 = let rec = insertarYPropagar c v comp in case a23 of
    --Si es hoja, elegimos la máxima de las claves y propagamos el balanceo hacia arriba.
    Hoja (ch,vh) -> if comp c ch 
                then (Hoja (c,v), Just (ch, Hoja (ch,vh)))
                else (Hoja (ch, vh), Just (c, Hoja (c,v)))
    {- Si el actual es Nodo-Dos, o se queda en forma Nodo-Dos o se transforma en 
       Nodo-Tres; no puede ocurrir que haya propagación hacia arriba (retornamos Nothing). -}
    Dos c1 a1 a2 -> (if comp c c1
                then 
                -- La clave c va del lado izquierdo.
                    interceptar (rec a1) 
                        (\s1 -> Dos c1 s1 a2)
                        (\s1 (c3, s2) -> Tres c3 c1 s1 s2 a2)
                else 
                -- La clave c va del lado derecho.
                    interceptar (rec a2) 
                        (\s1 -> Dos c1 a1 s1)
                        (\s1 (c3, s2) -> Tres c1 c3 a1 s1 s2), Nothing)
    {- Nodo-tres sólo propaga si de abajo propagan, los tres casos son muy similares
       Sólo cambia en que árbol se inserta. -}
    Tres c1 c2 a1 a2 a3 -> if comp c c1
                then 
                    -- La clave debe ir en el primer árbol.
                    interceptar (rec a1) 
                        (\s1 -> (Tres c1 c2 s1 a2 a3, Nothing))
                        (\s1 (c3, s2) -> (Dos c3 s1 s2, Just(c1, Dos c2 a2 a3)))
                else if comp c c2
                then 
                    -- La clave debe ir en el árbol del medio.
                    interceptar (rec a2) 
                        (\s1 -> (Tres c1 c2 a1 s1 a3, Nothing))
                        (\s1 (c3, s2) -> (Dos c1 a1 s1, Just(c3, Dos c2 s2 a3)))
                else 
                    --La clave debe ir en el último árbol.
                    interceptar (rec a3) 
                        (\s1 -> (Tres c1 c2 a1 a2 s1, Nothing))
                        (\s1 (c3, s2) -> (Dos c1 a1 a2, Just(c2, Dos c3 s1 s2)))

--Se asume que la lista no tiene claves repetidas.
definirVarias::[(clave,valor)]->Diccionario clave valor->Diccionario clave valor
definirVarias = (flip.foldr.uncurry) definir

{- Funciones a implementar. -}
{-
  Creo un diccionario vacío
-}
vacio::Comp clave -> Diccionario clave valor
vacio comparador = Dicc comparador Nothing

definir::clave->valor->Diccionario clave valor->Diccionario clave valor
{- Al definir un (clave, valor), creo un diccionario donde el segundo parametro de tipo Maybe 
siempre será de tipo Just.-}
definir clave valor dict = Dicc comparador (Just (rec))
{- Lo que contenga como parámetro el Just dependerá de si la estructura del diccionario
pasado como parámetro es Nothing, o si es Just z con z un árbol -}
   where    rec = case (estructura dict) of 
                        {-En ese caso, se necesitará una Hoja-}
                        Just z -> (insertar clave valor comparador z)
                        {-En ese caso, se hará uso de la función insertar-}
                        Nothing -> Hoja((clave, valor))
            comparador = cmp dict

{-
  Obtengo el valor asociado a una clave en el diccionario. Si la estructura del diccionario es un arbol, busco dentro del
  mismo el valor asociado a dicha calve, si es Nothing no hay nada para buscar
-}
obtener::Eq clave=>clave->Diccionario clave valor->Maybe valor
obtener clave dict = case (estructura dict) of
            Just z -> obtenerClave clave (cmp dict) z   
            Nothing -> Nothing

obtenerClave::Eq clave=>clave->Comp clave->Estr clave valor->Maybe valor
obtenerClave clave comparador a23 = 
    foldA23 fHoja fDos fTres a23
            where fHoja = (\(c,v) -> if clave == c then Just v else Nothing)
                  fDos = (\c a1 a2 -> if comparador clave c then a1 else a2) 
                  fTres = (\c1 c2 a1 a2 a3 -> if comparador clave c1 then a1 else 
                                                (if comparador clave c2 then a2 else a3)) 

    {-
    -- Si llegue a una hoja me fijo si tiene la clave que busco, si no devuelvo Nothing
    Hoja (c,v) -> if clave == c 
                    then Just v
                    else Nothing
    -- Si es arbol Dos me fijo por que rama seguir 
    --    Si la comparacion da con c sigo buscando del lado izquierdo
    Dos c a1 a2 -> if comparador clave c
                        then obtenerClave clave comparador a1
                        else obtenerClave clave comparador a2
    -- Si es arbol Tres me fijo por que rama seguir 
    --    Si la comparacion da con c1 sigo buscando por el medio
    Tres c1 c2 a1 a2 a3 -> if comparador clave c1
                        then obtenerClave clave comparador a1
                        else if comparador clave c2
                            then obtenerClave clave comparador a2
                            else obtenerClave clave comparador a3
                            -}

claves::Diccionario clave valor->[clave]
claves dict = case (estructura dict) of
                Just z -> (map (\(c,v) -> c) (hojas z))
                Nothing -> []

{- Diccionarios de prueba: -}

dicc1::Diccionario Int String
dicc1 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (<))

dicc2::Diccionario String String
dicc2 = definirVarias [("inicio","casa"),("auto","flores"),("calle","auto"),("casa","escalera"),("ropero","alfajor"),("escalera","ropero")] (vacio (<))

dicc3::Diccionario Int String
dicc3 = definirVarias [(0,"Hola"),(-10,"Chau"),(15,"Felicidades"),(2,"etc."),(9,"a")] (vacio (\x y->x `mod` 5 < y `mod` 5))

