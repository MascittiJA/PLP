module Arbol23 where

data Arbol23 a b = Hoja a | Dos b (Arbol23 a b) (Arbol23 a b) | Tres b b (Arbol23 a b) (Arbol23 a b) (Arbol23 a b)

{- Funciones para mostrar el árbol. -}

instance (Show a, Show b) => Show (Arbol23 a b) where
    show = ("\n" ++) . (padTree 0 0 False)

padlength = 5    
    
padTree:: (Show a, Show b) => Int -> Int -> Bool -> (Arbol23 a b)-> String
padTree nivel acum doPad t = case t of 
                                  (Hoja x) -> initialPad ++ stuff x
                                  (Dos x i d) -> initialPad ++ stuff x ++ 
                                                 pad padlength ++ rec x False i ++ "\n" ++
                                                 rec x True d ++ "\n"
                                  (Tres x y i m d) -> initialPad ++ stuff x ++ --(' ':tail (stuff y)) ++
                                                      pad padlength ++ rec x False i ++ "\n" ++
                                                      pad levelPad ++ stuff y ++ pad padlength ++ rec x False m ++ "\n" ++
                                                      rec x True d ++ "\n" 
  where l = length . stuff
        levelPad = (padlength*nivel + acum)
        initialPad = (if doPad then pad levelPad else "")
        rec x = padTree (nivel+1) (acum+l x)
            
stuff:: Show a => a -> String
stuff x = if n > l then pad (n-l) ++ s else s
  where s = show x
        l = length s
        n = padlength

pad:: Int -> String
pad i = replicate i ' '

{- Funciones pedidas. -}
{-
 En el foldA23 buscamos desarmar recursivamente la estructura de estos tipos de arborles
 Para eso definimos el comportamiento para cada uno de sus constructores
-}
foldA23::(a -> r) -> ( b -> r -> r -> r ) -> ( b -> b -> r -> r -> r -> r ) -> Arbol23 a b -> r
foldA23 fH fD fT arbol = case arbol of
                              Hoja a -> fH a
                              Dos b a1 a2 -> fD b (rec a1) (rec a2)
                              Tres b1 b2 a1 a2 a3 -> fT b1 b2 (rec a1) (rec a2) (rec a3)
    where rec = foldA23 fH fD fT


--Lista en preorden de los internos del árbol.
{-
 Para listar los nodos internos del arbol aprovechamos el comportamiento recursivo del foldA23
 definiendo las funciones necesarias para cada uno de los constructores.
 Como queremos solo los nodos internos, las hojas no tienen relevancia y cuando se trata de un
 nodo con 2 o 3 hijos, primero va el nodo y luego sus hijos de izquierda a derecha.
-}
internos::Arbol23 a b->[b]
internos = foldA23 fHoja fDos fTres 
            where fHoja = (\_ -> []) 
                  fDos = (\x a1 a2 -> [x] ++ a1 ++ a2) 
                  fTres = (\x z a1 a2 a3 -> [x,z] ++ a1 ++ a2 ++ a3)

--Lista las hojas de izquierda a derecha.
{-
 Nuevamente aprovechamos la estructura recursiva del foldA23, pero ahora solo para listar las
 hojas del mismo.
-}
hojas::Arbol23 a b->[a]
hojas = foldA23 fHoja fDos fTres 
            where fHoja = (\x -> [x]) 
                  fDos = (\_ a1 a2 -> a1 ++ a2) 
                  fTres = (\_ _ a1 a2 a3 -> a1 ++ a2 ++ a3)

                  
{-
 Funcion simple que devuelve TRUE solo si es un árbol con una única hoja
-}
esHoja::Arbol23 a b->Bool
esHoja (Hoja _) = True
esHoja (Dos _ _ _) = False
esHoja (Tres _ _ _ _ _) = False

-- recorrer el arbol e ir armando uno aplicando las funciones
{-
 Utilizamos la estructura recursiva del foldA23 y definimos como es la aplicacion de las funciones
 para cada constructor del arbol.
 Tenemos una funcion para las hojas y otra para los nodos, que se aplican respectivamente en 
 cada constructor
-}
mapA23::(a->c)->(b->d)->Arbol23 a b->Arbol23 c d
mapA23 fHojas fNodos = foldA23 fHojasRec fNodosDobles fNodosTriples
      where fHojasRec =  \hoja -> Hoja (fHojas hoja)
            fNodosDobles = \x a1 a2 ->  Dos (fNodos x) a1 a2
            fNodosTriples = \x z a1 a2 a3 -> Tres (fNodos x) (fNodos z) a1 a2 a3 

--Ejemplo de uso de mapA23.
--Incrementa en 1 el valor de las hojas.
{-
 Aprovechando el comportamiento de mapA23. Solo es necesario definir dos funciones, y como queremos
 incrementar en 1 solo a las hojas, vamos a utilizar la funcion (+1) para las hojas y la identidad
 para el resto de los nodos del árbol
-}
incrementarHojas::Num a =>Arbol23 a b->Arbol23 a b
incrementarHojas = mapA23 (+1) id

-- Fold para naturales
foldNat::b -> (b -> b) -> Integer -> b
foldNat x f n = case n of 0 -> x
                          _ -> f (rec (n-1))
                            where rec = foldNat x f

--Trunca el árbol hasta un determinado nivel. Cuando llega a 0, reemplaza el resto del árbol por una hoja con el valor indicado.
--Funciona para árboles infinitos.
{-
 Utilizando foldNat y una funcion que recursivamente se mete en la estructura del arbol, cuando llega al valor indicado como 
 parámetro de entrada se devuelve un arbol Hoja con el otro parámetro de la función, que pasa a ser parte de nuestro nuevo arbol sin
 seguir recorriendo el resto.
-}
truncar::a -> Integer -> Arbol23 a b -> Arbol23 a b
truncar valor = foldNat fBase fRecu
  where fBase = (const (Hoja valor))
        fRecu =  (\f x -> (case x of
                              Hoja c -> (Hoja c)
                              Dos b a1 a2 -> Dos b (f a1) (f a2)
                              Tres b1 b2 a1 a2 a3 -> Tres b1 b2 (f a1) (f a2) (f a3)))

--Evalúa las funciones tomando los valores de los hijos como argumentos.
--En el caso de que haya 3 hijos, asocia a izquierda.
{-
  Evalua las funciones que están definidas en los nodos internos del arbol, utilizando los respectivos hijos
  de ese nodo como parámetros de la funcion.
-}
evaluar::Arbol23 a (a->a->a)->a
evaluar = foldA23 fHoja fDos fTres
          where fHoja = \x -> x
                fDos = \func a1 a2 -> func a1 a2
                fTres = \func1 func2 a1 a2 a3 -> func2 (func1 a1 a2) a3

--Ejemplo:
--evaluar (truncar 0 6 arbolito3) = 22 = (1*2-3)+(2*3-4)+(3*4-5)+(4*5-6)

{- Árboles de ejemplo. -}
arbolito1::Arbol23 Char Int
arbolito1 = Tres 0 1
        (Dos 2 (Hoja 'a') (Hoja 'b'))
        (Tres 3 4 (Hoja 'c') (Hoja 'd') (Dos 5 (Hoja 'e') (Hoja 'f')))
        (Dos 6 (Hoja 'g') (Dos 7 (Hoja 'h') (Hoja 'i')))

arbolito2::Arbol23 Int Bool
arbolito2 = Dos True (Hoja (-1)) (Tres False True (Hoja 0) (Hoja (-2)) (Hoja 4))

arbolito3::Arbol23 Int (Int->Int->Int)
arbolito3 = Dos (+) (Tres (*) (-) (Hoja 1) (Hoja 2) (Hoja 3)) (incrementarHojas arbolito3)

arbolito4::Arbol23 Int Char
arbolito4 = Dos 'p' (Dos 'l' (Dos 'g' (Hoja 5) (Hoja 2)) (Tres 'r' 'a' (Hoja 0)(Hoja 1)(Hoja 12))) 
                    (Dos 'p' (Tres 'n' 'd' (Hoja (-3))(Hoja 4)(Hoja 9)) (Dos 'e' (Hoja 20)(Hoja 7)))
