module Proceso (Procesador, AT(Nil,Tern), RoseTree(Rose), Trie(TrieNodo), foldAT, foldRose, foldTrie, procVacio, procId, procCola, procHijosRose, procHijosAT, procRaizTrie, procSubTries, unoxuno, sufijos, inorder, preorder, postorder, preorderRose, hojasRose, ramasRose, caminos, palabras, ifProc,(++!), (.!)) where

import Test.HUnit
import Data.Maybe (isNothing, isJust)


--Definiciones de tipos

type Procesador a b = a -> [b]


-- Árboles ternarios
data AT a = Nil | Tern a (AT a) (AT a) (AT a) deriving Eq
--E.g., at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
--Es es árbol ternario con 1 en la raíz, y con sus tres hijos 2, 3 y 4.

-- RoseTrees
data RoseTree a = Rose a [RoseTree a] deriving Eq
--E.g., rt = Rose 1 [Rose 2 [], Rose 3 [], Rose 4 [], Rose 5 []] 
--es el RoseTree con 1 en la raíz y 4 hijos (2, 3, 4 y 5)

-- Tries
data Trie a = TrieNodo (Maybe a) [(Char, Trie a)] deriving Eq
-- E.g., t = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]
-- es el Trie Bool de que tiene True en la raíz, tres hijos (a, b, y c), y, a su vez, b tiene como hijo a d.


-- Definiciones de Show

instance Show a => Show (RoseTree a) where
    show = showRoseTree 0
      where
        showRoseTree :: Show a => Int -> RoseTree a -> String
        showRoseTree indent (Rose value children) =
            replicate indent ' ' ++ show value ++ "\n" ++
            concatMap (showRoseTree (indent + 2)) children

instance Show a => Show (AT a) where
    show = showAT 0
      where
        showAT :: Show a => Int -> AT a -> String
        showAT _ Nil = replicate 2 ' ' ++ "Nil"
        showAT indent (Tern value left middle right) =
            replicate indent ' ' ++ show value ++ "\n" ++
            showSubtree (indent + 2) left ++
            showSubtree (indent + 2) middle ++
            showSubtree (indent + 2) right

        showSubtree :: Show a => Int -> AT a -> String
        showSubtree indent subtree =
            case subtree of
                Nil -> replicate indent ' ' ++ "Nil\n"
                _   -> showAT indent subtree

instance Show a => Show (Trie a) where
    show = showTrie ""
      where
        showTrie :: Show a => String -> Trie a -> String
        showTrie indent (TrieNodo maybeValue children) =
            let valueLine = case maybeValue of
                                Nothing -> indent ++ "<vacío>\n"
                                Just v  -> indent ++ "Valor: " ++ show v ++ "\n"
                childrenLines = concatMap (\(c, t) -> showTrie (indent ++ "  " ++ [c] ++ ": ") t) children
            in valueLine ++ childrenLines


--Ejercicio 1
procVacio :: Procesador a b
procVacio _ = []

procId :: Procesador a a
procId a = [a]

procCola :: Procesador [a] a
procCola [] = []
procCola (_:xs) = xs

procHijosRose :: Procesador (RoseTree a) (RoseTree a)
procHijosRose (Rose _ roseHijos) = roseHijos
-- Rose a [RoseTree a]

procHijosAT :: Procesador (AT a) (AT a)
procHijosAT Nil = []
procHijosAT (Tern _ izq medio der) = [izq, medio, der]
-- AT a puede ser Nil ó Tern a (AT a) (AT a) (AT a)

procRaizTrie :: Procesador (Trie a) (Maybe a)
procRaizTrie (TrieNodo a _) = [a]
-- Trie a es TrieNodo (Maybe a) [(Char, Trie a)] y te pide la parte del Maybe

procSubTries :: Procesador (Trie a) (Char, Trie a)
procSubTries (TrieNodo a hijos) = hijos
-- Lo mismo pero quiere los hijos == el Char y el siguiente Trie a

--Ejercicio 2


foldAT :: (a -> b -> b -> b -> b) -> b -> AT a -> b
foldAT fAt c Nil = c
foldAT fAt c (Tern w x y z) = fAt w (foldAT fAt c x) (foldAT fAt c y) (foldAT fAt c z)

foldRose :: (a -> [b] -> b) -> RoseTree a -> b
foldRose fRs (Rose n roseHijos) = fRs n (map rec roseHijos)
        where rec = foldRose fRs

foldTrie :: (Maybe a -> [(Char,b)] -> b) -> Trie a -> b
foldTrie fT (TrieNodo a as) = fT a (map (\(x,xs) -> (x, foldTrie fT xs)) as)


--Ejercicio 3
unoxuno :: Procesador [a] [a]
unoxuno = map (:[])

sufijos :: Procesador [a] [a]
sufijos = foldr (\x xs -> (x: head xs) : xs) [[]]


--Ejercicio 4
preorder :: Procesador (AT a) a
preorder = foldAT (\w x y z -> w : x ++ y ++ z) []

postorder :: Procesador (AT a) a
postorder = foldAT (\w x y z -> x ++ y ++ z ++ [w]) []

inorder :: Procesador (AT a) a
inorder = foldAT (\w x y z -> x ++ y ++ [w] ++ z) []

att = Tern 16
        (Tern 1 (Tern 9 Nil Nil Nil) (Tern 7 Nil Nil Nil) (Tern 2 Nil Nil Nil))
        (Tern 14 (Tern 0 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 6 Nil Nil Nil))
        (Tern 10 (Tern 8 Nil Nil Nil) (Tern 5 Nil Nil Nil) (Tern 4 Nil Nil Nil))

--Ejercicio 5

preorderRose :: Procesador (RoseTree a) a
preorderRose = foldRose (\x rec -> x : concat rec)

hojasRose :: Procesador (RoseTree a) a
hojasRose = foldRose (\x rec -> if null rec then [x] else concat rec)

ramasRose :: Procesador (RoseTree a) [a]
ramasRose = foldRose (\x rec -> if null rec then [[x]] else map (x:) (concat rec))


--Definición del árbol rosado (RoseTree)
atRoseTree :: RoseTree Int
atRoseTree = Rose 16 [ Rose 1 [ Rose 9 [] , Rose 7 [] , Rose 2 [] ] , Rose 14 [ Rose 0 [], Rose 3 [], Rose 6 []] , Rose 10 [ Rose 8 [], Rose 5 [], Rose 4 []]]


--Ejercicio 6

caminos :: Trie a -> [String]
caminos = foldTrie (\t tr -> "" : concatMap (\(c, cr) -> if null cr then [[c]] else map (c:) cr) tr)

--Ejercicio 7

palabras :: Trie a -> [String]
palabras = foldTrie (\t tr -> concatMap (\(c,cr) -> if null cr || isNothing t then [[c]] else map (c:) cr) tr)


--Ejercicio 8
-- 8.a)
ifProc :: (a->Bool) -> Procesador a b -> Procesador a b -> Procesador a b
ifProc f a b = (\ x -> if f x then a x else b x)

-- 8.b)
(++!) :: Procesador a b -> Procesador a b -> Procesador a b
(++!) a b = (\x -> (a x) ++ (b x))

-- 8.c)
(.!) :: Procesador b c -> Procesador a b -> Procesador a c
(.!) a b = \x -> concatMap a (b x)

--Ejercicio 9
-- Se recomienda poner la demostración en un documento aparte, por claridad y prolijidad, y, preferentemente, en algún formato de Markup o Latex, de forma de que su lectura no sea complicada.

-- Parametros para casos de test
at = Tern 1 (Tern 2 Nil Nil Nil) (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil)
atRes = Tern 2 (Tern 3 Nil Nil Nil) (Tern 4 Nil Nil Nil) (Tern 5 Nil Nil Nil) 
at2 = Tern 2 Nil Nil Nil
at3 = Tern 3 Nil Nil Nil
at4 = Tern 4 Nil Nil Nil
atEj1 = Tern 1 (at2) (at3) (at4)

atVacio = Nil

atNoHijos = Nil


rt = Rose 1 [Rose 2 [Rose 3 [], Rose 4 [Rose 5 [], Rose 6 [], Rose 7 []]]]
rtRes = Rose 2 [Rose 3 [Rose 4 [], Rose 5 [Rose 6 [], Rose 7 [], Rose 8 []]]]
rt1 = Rose 2 []
rt2 = Rose 3 [Rose 4 [Rose 5 []], Rose 8 []]

rt3 = Rose 6 [Rose 7 [], Rose 8 [Rose 9 [Rose 10 []], Rose 11 [Rose 12 []]]]

rt4 = Rose 12 [Rose 3 [], Rose 2 [Rose 7 [Rose 15 []], Rose 21 [Rose 6 []]]]

rt5 = Rose 6 [Rose 5 [Rose 76 [], Rose 1 [],Rose 4 []], Rose 8 [],Rose 3 [], Rose 18 [Rose 11 [], Rose 12 [],Rose 89 []],Rose 31 [], Rose 22 [],Rose 0 [Rose 19 [], Rose 3 [],Rose 5 []]]
rtEj1 = Rose 1 [rt1, rt2]

rtEj2 = Rose 1 [rt4,rt,rt2]

rtEj3 = Rose 1 [rt1,rt5,rt2,rt3]

roseNoHijos = Rose 1 []


trieNoHijos = TrieNodo (Just True) []
trieNoHijosI = TrieNodo (Just 1) []
trieNoHijosIRes = TrieNodo (Just 2) []
t1 = ('d', TrieNodo (Just True) [])
t2 = ('p', TrieNodo Nothing [('r', TrieNodo (Just True) [('e', TrieNodo Nothing [])])])
trieEj1 = TrieNodo (Just True) [t1,t2]
trieEj2 = TrieNodo (Just True) [('a', TrieNodo (Just True) []), ('b', TrieNodo Nothing [('a', TrieNodo (Just True) [('d', TrieNodo Nothing [])])]), ('c', TrieNodo (Just True) [])]

trieEj3 = TrieNodo (Just 1) [('h', TrieNodo (Just 2) [('o', TrieNodo (Just 3) [('l', TrieNodo Nothing [('a', TrieNodo (Just 5) [('p', TrieNodo Nothing [])])])])]),('m', TrieNodo (Just 6) [('u', TrieNodo (Just 7) [('n', TrieNodo (Just 8) [('d', TrieNodo Nothing [('o', TrieNodo (Just 10) [('w', TrieNodo Nothing [])])])])])])]

trieEj4 = TrieNodo (Just True) [('y', TrieNodo (Just True) []), ('n', TrieNodo Nothing [('o', TrieNodo (Just True) [('v', TrieNodo Nothing [])])]), ('t', TrieNodo (Just True) [('r', TrieNodo (Just True) [('i', TrieNodo Nothing [('e', TrieNodo (Just True) [('d', TrieNodo (Just True) [])])])])])]

trieEj5 = TrieNodo (Just 1) [('a', TrieNodo (Just 2) []), ('b', TrieNodo (Just 3) [('a', TrieNodo (Just 4) [('d', TrieNodo (Just 5) [])])]), ('c', TrieNodo (Just 6) [])]
trieEj5Res = TrieNodo (Just 2) [('a', TrieNodo (Just 3) []), ('b', TrieNodo (Just 4) [('a', TrieNodo (Just 5) [('d', TrieNodo (Just 6) [])])]), ('c', TrieNodo (Just 7) [])]

--Funcion auxuliar para Trie
sumaTrie :: Num a => Maybe a -> Maybe a
sumaTrie Nothing = Nothing
sumaTrie (Just x) = Just (x+1)


{-Tests-}

main :: IO Counts
main = do runTestTT allTests

allTests = test [ -- Reemplazar los tests de prueba por tests propios
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7,
  "ejercicio8a" ~: testsEj8a,
  "ejercicio8b" ~: testsEj8b,
  "ejercicio8c" ~: testsEj8c
  ]
testsEj1 = test [ -- Casos de test para el ejercicio 1
  procVacio []             -- Caso de test 1 - expresión a testear
    ~=? ([] :: [Int])
  ,
  procCola []             -- Caso de test 1 - expresión a testear
    ~=? ([] :: [Int])
  ,
  procCola [1]             -- Caso de test 1 - expresión a testear
    ~=? ([] :: [Int])
  ,
  procCola [1,2]             -- Caso de test 1 - expresión a testear
    ~=? ([2] :: [Int])
  ,
  procHijosRose rtEj1
      ~=? [rt1,rt2]
  ,
  procHijosRose roseNoHijos
      ~=? []
  ,
  procHijosAT atEj1     -- Caso de test 2 - expresión a testear
    ~=? [at2,at3,at4]                                                    -- Caso de test 2 - resultado esperado
  ,
  procHijosAT atNoHijos     -- Caso de test 2 - expresión a testear
    ~=? ([] :: [AT Int])
  ,
  procRaizTrie trieNoHijos
      ~=? [Just True]
  ,
  procRaizTrie trieEj1
      ~=? [Just True]
  ,
  procSubTries trieNoHijos
      ~=? []
  ,
  procSubTries trieEj1
      ~=? [t1,t2]
  ]

testsEj2 = test [ -- Casos de test para el ejercicio 2
  foldAT (\w x y z -> Tern (1 + w) x y z) Nil at
    ~=? atRes
  ,
  foldAT (\w x y z -> Tern (1 + w) x y z) Nil atNoHijos
    ~=? atNoHijos
  ,
  foldAT (\w x y z -> Tern w x y z) Nil at
    ~=? at
  ,
  foldAT (\w x y z -> Tern w x y z) Nil at2
    ~=? at2
  ,
  foldRose (\x rec -> Rose (1+x) rec) rt
    ~=? rtRes
  ,
  foldRose (\x rec -> Rose (1+x) rec) roseNoHijos
    ~=? roseNoHijos
  ,
  foldRose (\x rec -> Rose x rec) rt
    ~=? rt
  ,
  foldRose (\x rec -> Rose x rec) roseNoHijos
    ~=? roseNoHijos
  ,
  foldTrie (\t tr -> TrieNodo (sumaTrie t) tr) trieEj5
    ~=? trieEj5Res
  ,
  foldTrie (\t tr -> TrieNodo (sumaTrie t) tr) trieNoHijosI
    ~=? trieNoHijosIRes
  ,
  foldTrie (\t tr -> TrieNodo t tr) trieEj3
    ~=? trieEj3
  ,
  foldTrie (\t tr -> TrieNodo t tr) trieNoHijosI
    ~=? trieNoHijosI
  ]

testsEj3 = test [ -- Casos de test para el ejercicio 3
  unoxuno [3,1,4,1,5,9]
    ~=? [[3],[1],[4],[1],[5],[9]]
  ,
  unoxuno []
    ~=? ([] :: [[Int]])
  ,
  unoxuno "Amigo"
    ~=? ["A","m","i","g","o"]
  ,
  sufijos "Amigo"
    ~=? ["Amigo","migo","igo","go","o",""]
  ,
  sufijos ""
    ~=? [""]
  ,
  sufijos []
    ~=? ([[]] :: [[Int]])
  ,
  sufijos [1,2,3,4]
    ~=? [[1,2,3,4],[2,3,4],[3,4],[4],[]]

  ]

testsEj4 = test [ -- Casos de test para el ejercicio 4
  preorder att
    ~=? [16,1,9,7,2,14,0,3,6,10,8,5,4]
  ,
  preorder (Tern Nil (Nil) (Nil) (Nil))
    ~=? ([Nil] :: [AT Int])
  ,
  preorder at
    ~=? [1,2,3,4]
  ,
  postorder att
    ~=? [9,7,2,1,0,3,6,14,8,5,4,10,16]
  ,
  postorder (Tern Nil (Nil) (Nil) (Nil))
    ~=? ([Nil] :: [AT Int])
  ,
  postorder at
    ~=? [2,3,4,1]
  ,
  inorder att
    ~=? [9,7,1,2,0,3,14,6,16,8,5,10,4]
  ,
  inorder (Tern Nil (Nil) (Nil) (Nil))
    ~=? ([Nil] :: [AT Int])
  ,
  inorder at
    ~=? [2,3,1,4]
  ]


testsEj5 = test [ -- Casos de test para el ejercicio 5
  preorderRose roseNoHijos
    ~=? [1]
  ,
  preorderRose rtEj2
    ~=? [1,12,3,2,7,15,21,6,1,2,3,4,5,6,7,3,4,5,8]
  ,
  preorderRose rtEj3
    ~=? [1,2,6,5,76,1,4,8,3,18,11,12,89,31,22,0,19,3,5,3,4,5,8,6,7,8,9,10,11,12]
  ,
  hojasRose roseNoHijos
    ~=? [1]
  ,
  hojasRose rtEj2
    ~=? [3,15,6,3,5,6,7,5,8]
  ,
  hojasRose rtEj3
    ~=? [2,76,1,4,8,3,11,12,89,31,22,19,3,5,5,8,7,10,12]
  ,
  ramasRose roseNoHijos
    ~=? [[1]]
  ,
  ramasRose rtEj2
    ~=? [[1,12,3],[1,12,2,7,15],[1,12,2,21,6],
        [1,1,2,3],[1,1,2,4,5],[1,1,2,4,6],
        [1,1,2,4,7],[1,3,4,5],[1,3,8]]
  ,
  ramasRose rtEj3
    ~=? [[1,2],[1,6,5,76],[1,6,5,1],
        [1,6,5,4],[1,6,8],[1,6,3],
        [1,6,18,11],[1,6,18,12],[1,6,18,89],
        [1,6,31],[1,6,22],[1,6,0,19],
        [1,6,0,3],[1,6,0,5],[1,3,4,5],
        [1,3,8],[1,6,7],[1,6,8,9,10],[1,6,8,11,12]]
  ]

testsEj6 = test [ -- Casos de test para el ejercicio 6
  caminos trieEj1
    ~=? ["","a","b","ba","bad","c"]
  ,
  caminos trieEj3
    ~=? ["","h","ho","hol","hola","holap","m","mun","mund","mundo","mundow"]
  ,
  caminos trieEj4
    ~=? ["","y","n","no","nov","t","tr","tri","trie","tried"]
  ]

testsEj7 = test [ -- Casos de test para el ejercicio 7
  palabras trieEj1
    ~=? ["a","ba","c"]
  ,
  palabras trieEj3
    ~=? ["hola","mundo"]
  ,
  palabras trieEj4
    ~=? ["y","no","trie"]                                          -- Caso de test 1 - resultado esperado
  ]


esNilAT :: AT a -> Bool
esNilAT Nil = True
esNilAT _   = False


testsEj8a = test [ -- Casos de test para el ejercicio 7
  ifProc odd procId procVacio 2        -- Caso de test 1 - expresión a testear
    ~=? []
  ,                                          -- Caso de test 1 - resultado esperado
  ifProc even procId procVacio 2        -- Caso de test 1 - expresión a testear
    ~=? [2]
  ,
  ifProc esNilAT procId procVacio Nil
    ~=? ([Nil] :: [AT Int])
  ,
  ifProc (not . esNilAT) procId procVacio Nil
    ~=? ([] :: [AT Int])
  ,
  ifProc ( not . esNilAT) procId procVacio at
    ~=? procId at
  
  ]
testsEj8b = test [ -- Casos de test para el ejercicio 7
  (++!) postorder preorder att
    ~=? [9,7,2,1,0,3,6,14,8,5,4,10,16,16,1,9,7,2,14,0,3,6,10,8,5,4]
  ,
  (++!) postorder preorder Nil
    ~=? ([] :: [AT Int])
  ,
  (++!) (procCola . postorder) preorder att  -- Igual al primero pero sin la cabeza
    ~=? [7,2,1,0,3,6,14,8,5,4,10,16,16,1,9,7,2,14,0,3,6,10,8,5,4]
  ]

testsEj8c = test [ -- Casos de test para el ejercicio 7
  (.!) (\z -> [0..z]) (map (+1)) [1,3]
    ~=? [0,1,2,0,1,2,3,4]
  ,
  (.!) procId (map (+1)) []
    ~=? []
  ,
  (.!) (\z -> [z, z*2,z*3]) (map (^2)) [4]
    ~=? [16,32,48]

  ]
