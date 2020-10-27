module Practica1 where
--Ejercicio 1
type Variable = String
--Ejemplos
--Var "p0"
--Var "a"
--Var "variable"
--Var "EstaNoEsUnaVariable"

--Ejercicio 2
type Valuacion = [(Variable,Bool)]
--Ejemplos
--[("a",True)]
--[("b",false),("c",True)]
--[("a",True),("b",false),("c",True)]

--Ejercicio 3
variablesUsadas :: PL -> [Variable]
variablesUsadas phi = case phi of
    Bot -> []
    Top -> []
    Var x -> [x]
    Imp a b -> quitaDuplicados((variablesUsadas a)++(variablesUsadas b))
    Dis a b -> quitaDuplicados((variablesUsadas a)++(variablesUsadas b))
    Con a b -> quitaDuplicados((variablesUsadas a)++(variablesUsadas b))
    Nand a b -> quitaDuplicados((variablesUsadas a)++(variablesUsadas b))
    Nor a b -> quitaDuplicados((variablesUsadas a)++(variablesUsadas b))
    Neg a -> quitaDuplicados((variablesUsadas a))
-- Tests:
-- variablesUsadas Bot
-- variablesUsadas Top
-- variablesUsadas (Var "a")
-- variablesUsadas (Neg(Var "a"))
-- variablesUsadas (Imp (Var "a") (Var "b"))
-- variablesUsadas (Imp (Var "a") (Var "a"))
-- variablesUsadas (Dis (Var "a") (Var "b"))
-- variablesUsadas (Dis (Var "a") (Var "a"))
-- variablesUsadas (Con (Var "a") (Var "b"))
-- variablesUsadas (Con (Var "a") (Var "a"))
-- variablesUsadas (Nand (Var "a") (Var "b"))
-- variablesUsadas (Nand (Var "a") (Var "a"))

-- funcion que quita duplicados de una lista
-- esta funcion la ocupamos en variablesUsadas para prevenir duplicados en la lista
quitaDuplicados :: [Variable] -> [Variable]
quitaDuplicados [] = []
quitaDuplicados (x:xs) = [x]++quitaDuplicados (quitaElemento x xs)

-- funcion que quita un elemento de una lista
quitaElemento ::Variable -> [Variable] -> [Variable]
quitaElemento a [] = []
quitaElemento a (x:xs) = if a==x then quitaElemento a xs else [x]++quitaElemento a xs

--Ejercicio 4
data PL = Bot | Top | Var Variable | Imp PL PL | Dis PL PL | Con PL PL | Neg PL | Nand PL PL | Nor PL PL
instance Show PL where
 show(Bot)= "⊥"
 show(Top)= "⊤"
 show(Var a)= a
 show(Imp a b)= "("++show(a)++"→"++show(b)++")"
 show(Dis a b)= "("++show(a)++" ⋁ "++show(b)++")"
 show(Con a b)= "("++show(a)++" ⋀ "++show(b)++")"
 show(Neg a)= "¬"++show(a)
 show(Nand a b)="("++show(a)++"⊼"++show(b)++")"
 show(Nor a b)="("++show(a)++"⊽"++show(b)++")"

numConjunciones :: PL -> Int
numConjunciones phi = case phi of
    Bot -> 0
    Top -> 0
    Var a -> 0
    Neg a -> 0+numConjunciones(a)
    Con a b -> 1+numConjunciones(a)+numConjunciones(b)
    Dis a b -> 0+numConjunciones(a)+numConjunciones(b)
    Imp a b -> 0+numConjunciones(a)+numConjunciones(b)
    Nand a b -> 0+numConjunciones(a)+numConjunciones(b)
    Nor a b -> 0+numConjunciones(a)+numConjunciones(b)
-- Tests:
-- numConjunciones Bot
-- numConjunciones Top
-- numConjunciones (Var "a")
-- numConjunciones (Neg(Var "a"))
-- numConjunciones (Imp (Var "a") (Var "b"))
-- numConjunciones (Dis (Var "a") (Var "b"))
-- numConjunciones (Con (Var "a") (Var "b"))
-- numConjunciones (Nand (Var "a") (Var "b"))
-- numConjunciones (Con (Con (Var "a") (Var "b")) (Con (Var "c") (Var "d")))

--Ejercicio 5
quitaImp :: PL -> PL
quitaImp phi = case phi of
    Bot -> Bot
    Top -> Top
    Var a -> Var a
    Neg a -> Neg (quitaImp a)
    Con a b -> Con (quitaImp a) (quitaImp b)
    Dis a b -> Dis (quitaImp a) (quitaImp b)
    Imp a b -> Dis (Neg (quitaImp a)) (quitaImp b)
    Nand a b -> Nand (quitaImp a) (quitaImp b)
    Nor a b -> Nor (quitaImp a) (quitaImp b)
-- Tests:
-- quitaImp Bot
-- quitaImp Top
-- quitaImp (Var "a")
-- quitaImp (Neg(Var "a"))
-- quitaImp (Imp (Var "a") (Var "b"))
-- quitaImp (Dis (Var "a") (Var "b"))
-- quitaImp (Con (Var "a") (Var "b"))
-- quitaImp (Nand (Var "a") (Var "b"))
-- quitaImp (Imp (Imp (Var "a") (Var "b")) (Imp (Var "c") (Var "d")))

quitaAnd :: PL -> PL
quitaAnd phi = case phi of
    Bot -> Bot
    Top -> Top
    Var a -> Var a
    Neg a -> Neg (quitaAnd a)
    Con a b -> Dis (Neg (quitaAnd a)) (Neg (quitaAnd b))
    Dis a b -> Dis (quitaAnd a) (quitaAnd b)
    Imp a b -> Imp (quitaAnd a) (quitaAnd b)
    Nand a b -> Nand (quitaAnd a) (quitaAnd b)
    Nor a b -> Nor (quitaAnd a) (quitaAnd b)
-- Tests:
-- quitaAnd Bot
-- quitaAnd Top
-- quitaAnd (Var "a")
-- quitaAnd (Neg(Var "a"))
-- quitaAnd (Imp (Var "a") (Var "b"))
-- quitaAnd (Dis (Var "a") (Var "b"))
-- quitaAnd (Con (Var "a") (Var "b"))
-- quitaAnd (Nand (Var "a") (Var "b"))
-- quitaAnd (Con (Con (Var "a") (Var "b")) (Con (Var "c") (Var "d")))

quitaOr :: PL -> PL
quitaOr phi = case phi of
    Bot -> Bot
    Top -> Top
    Var a -> Var a
    Neg a -> Neg (quitaOr a)
    Con a b -> Con (quitaOr a) (quitaOr b)
    Dis a b -> Con (Neg (quitaAnd a)) (Neg (quitaAnd b))
    Imp a b -> Imp (quitaOr a) (quitaOr b)
    Nand a b -> Nand (quitaOr a) (quitaOr b)
    Nor a b -> Nor (quitaOr a) (quitaOr b)
-- Tests:
-- quitaOr Bot
-- quitaOr Top
-- quitaOr (Var "a")
-- quitaOr (Neg(Var "a"))
-- quitaOr (Imp (Var "a") (Var "b"))
-- quitaOr (Dis (Var "a") (Var "b"))
-- quitaOr (Con (Var "a") (Var "b"))
-- quitaOr (Nand (Var "a") (Var "b"))
-- quitaOr (Dis (Dis (Var "a") (Var "b")) (Dis (Var "c") (Var "d")))

--Ejercicio 6
lNand :: PL -> PL
lNand phi = case phi of
    Bot -> Bot
    Top -> Top
    Var a -> Var a
    Neg a -> Nand (lNand a) (lNand a)
    Con a b -> Nand (Nand (lNand a) (lNand b)) (Nand (lNand a) (lNand b))
    Dis a b -> Nand (Nand (lNand a) (lNand a)) (Nand (lNand b) (lNand b))
    Imp a b -> Nand (Nand (Nand (lNand a) (lNand a)) (Nand (lNand a) (lNand a))) (Nand (lNand b) (lNand b))
    Nand a b -> Nand (lNand a) (lNand b)
    Nor a b -> Nand (Nand (Nand (lNand a) (lNand a)) (Nand (lNand b) (lNand b))) (Nand (Nand (lNand a) (lNand a)) (Nand (lNand b) (lNand b)))
-- Tests:
-- lNand Bot
-- lNand Top
-- lNand (Var "a")
-- lNand (Neg(Var "a"))
-- lNand (Imp (Var "a") (Var "b"))
-- lNand (Dis (Var "a") (Var "b"))
-- lNand (Con (Var "a") (Var "b"))
-- lNand (Nand (Var "a") (Var "b"))
-- lNand (Nor (Var "a") (Var "b"))
-- lNand (Dis (Dis (Var "a") (Var "b")) (Dis (Var "c") (Var "d")))

lNor :: PL -> PL
lNor phi = case phi of
    Bot -> Bot
    Top -> Top
    Var a -> Var a
    Neg a -> Nor (lNor a) (lNor a)
    Con a b -> Nor (Nor (lNor a) (lNor a)) (Nor (lNor b) (lNor b))
    Dis a b -> Nor (Nor (lNor a) (lNor b)) (Nor (lNor a) (lNor b))
    Imp a b -> Nor (Nor (Nor (lNor a) (lNor a)) (lNor b)) (Nor (Nor (lNor a) (lNor a)) (lNor b))
    Nand a b -> Nor (Nor (Nor (lNor a) (lNor a)) (Nor (lNor b) (lNor b))) (Nor (Nor (lNor a) (lNor a)) (Nor (lNor b) (lNor b)))
    Nor a b -> Nor (lNor a) (lNor b)
--Tests:
-- lNor Bot
-- lNor Top
-- lNor (Var "a")
-- lNor (Neg(Var "a"))
-- lNor (Imp (Var "a") (Var "b"))
-- lNor (Dis (Var "a") (Var "b"))
-- lNor (Con (Var "a") (Var "b"))
-- lNor (Nand (Var "a") (Var "b"))
-- lNor (Nor (Var "a") (Var "b"))
-- lNor (Con (Dis (Var "a") (Var "b")) (Nand (Var "c") (Var "d")))

--Ejercicio 7
mSatisface :: Valuacion -> PL -> Bool
mSatisface ((var,val):sig) pl = case pl of
    Bot -> False
    Top -> True
    Var a -> if a==var then val else mSatisface sig (Var a)
    Neg a -> if mSatisface ((var,val):sig) a then False else True
    Con a b -> if (mSatisface ((var,val):sig) a && mSatisface ((var,val):sig) b) then True else False
    Dis a b -> if mSatisface ((var,val):sig) a then True else if mSatisface ((var,val):sig) b then True else False
    Imp a b -> if mSatisface ((var,val):sig) (Neg a) then True else if mSatisface ((var,val):sig) b then True else False
    Nand a b -> if (mSatisface ((var,val):sig) a && mSatisface ((var,val):sig) b) then False else True
    Nor a b -> if (mSatisface ((var,val):sig) a || mSatisface ((var,val):sig) b) then False else True
-- Tests
-- mSatisface [("a",False)] Bot
-- mSatisface [("a",False)] Top
-- mSatisface [("a",False)] (Var "a")
-- mSatisface [("a",True)] (Var "a")
-- mSatisface [("a",False)] (Neg(Var "a"))
-- mSatisface [("a",True)] (Neg(Var "a"))
-- mSatisface [("a",False),("b",False)] (Imp (Var "a") (Var "b"))
-- mSatisface [("a",False),("b",True)] (Imp (Var "a") (Var "b"))
-- mSatisface [("a",True),("b",False)] (Imp (Var "a") (Var "b"))
-- mSatisface [("a",True),("b",True)] (Imp (Var "a") (Var "b"))
-- mSatisface [("a",False),("b",True)] (Dis (Var "a") (Var "b"))
-- mSatisface [("a",False),("b",False)] (Dis (Var "a") (Var "b"))
-- mSatisface [("a",True),("b",True)] (Dis (Var "a") (Var "b"))
-- mSatisface [("a",True),("b",False)] (Dis (Var "a") (Var "b"))
-- mSatisface [("a",False),("b",False)] (Nand (Var "a") (Var "b"))
-- mSatisface [("a",False),("b",True)] (Nand (Var "a") (Var "b"))
-- mSatisface [("a",True),("b",False)] (Nand (Var "a") (Var "b"))
-- mSatisface [("a",True),("b",True)] (Nand (Var "a") (Var "b"))
-- mSatisface [("a",False),("b",False)] (Nor (Var "a") (Var "b"))
-- mSatisface [("a",False),("b",True)] (Nor (Var "a") (Var "b"))
-- mSatisface [("a",True),("b",False)] (Nor (Var "a") (Var "b"))
-- mSatisface [("a",True),("b",True)] (Nor (Var "a") (Var "b"))
-- mSatisface [("a",False),("b",False)] (Con (Var "a") (Var "b"))
-- mSatisface [("a",False),("b",True)] (Con (Var "a") (Var "b"))
-- mSatisface [("a",True),("b",False)] (Con (Var "a") (Var "b"))
-- mSatisface [("a",True),("b",True)] (Con (Var "a") (Var "b"))
-- mSatisface [("a",False),("b",False),("c",True),("d",False)] (Imp (Dis (Var "a") (Var "b")) (Nor (Var "c") (Var "d")))
-- mSatisface [("a",True),("b",False),("c",False),("d",False)] (Dis (Nand (Var "a") (Var "b")) (Con (Var "c") (Var "d")))