import Test.HUnit
import Arbol23
import Diccionario

test1 = TestCase (assertEqual "esHoja de Hoja 28" True (esHoja (Hoja 28)))
test2 = TestCase (assertEqual "esHoja de arbolito1" False (esHoja arbolito1))
test3 = TestCase (assertEqual "esHoja de arbolito1" False (esHoja arbolito2))
test4 = TestCase (assertEqual "esHoja de arbolito1" False (esHoja arbolito3))
test5 = TestCase (assertEqual "esHoja de arbolito1" False (esHoja arbolito4))

test6 = TestCase (assertEqual "hojas de arbolito1" "abcdefghi" (hojas arbolito1))
test7 = TestCase (assertEqual "hojas de arbolito2" [-1,0,-2,4] (hojas arbolito2))
test8 = TestCase (assertEqual "hojas de arbolito3" [1,2,3,2,3,4,3,4,5,0,0,0,0,0] (hojas (truncar 0 5 arbolito3)))
test9 = TestCase (assertEqual "hojas de arbolito4" [5,2,0,1,12,-3,4,9,20,7] (hojas arbolito4))
