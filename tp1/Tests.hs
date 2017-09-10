import Test.HUnit
import Arbol23
import Diccionario

test1 = TestCase (assertEqual "esHoja de Hoja 28" True (esHoja (Hoja 28)))
test2 = TestCase (assertEqual "esHoja de arbolito1" True (esHoja arbolito1))
