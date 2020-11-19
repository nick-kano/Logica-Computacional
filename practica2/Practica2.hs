module Practica2 where

type Variable = String

data PL = Bot | Top | Var Variable | Imp PL PL | Dis PL PL | Con PL PL | Neg PL | Nand PL PL | Nor PL PL deriving (Eq)
instance Show PL where
 show(Bot)= "⊥"
 show(Top)= "⊤"
 show(Var a)= a
 show(Imp a b)= "("++show(a)++"→"++show(b)++")"
 show(Dis a b)= "("++show(a)++" ⋁ "++show(b)++")"
 show(Con a b)= "("++show(a)++" ⋀ "++show(b)++")"
 show(Neg a)= "¬"++show(a)


-- Si phi en PL es una disyuncion de literales,
-- entonces disLit2ListLit transforma phi en una lista de literales.
--
---Ejemplo: disLit2ListLit (Var "x") ----> ["x"]
---         disLit2ListLit Bot -----> []
---         disLit2ListLit(Dis Bot (Var "x2")) ---->  [x2]
---         disLit2ListLit(Dis (Var "x") (Var "x2")) ----> [x,x2]
disLit2ListLit :: PL -> [PL]
disLit2ListLit phi = case phi of
    Bot -> []
    Var x -> [Var x]
    Neg (Var x) -> [Neg (Var x)]
    (Dis alpha beta) -> (disLit2ListLit alpha) ++ (disLit2ListLit beta)
    _-> error $"disLit2ListLit: phi no es una disyuncion de literales, phi="++(show phi)

-- Dado un literal l en PL, litComp calcula el literal complementario de l. (Recordando que una literal es una Variable o la negacion de una Variable)
---Ejemplo: litComp(Var "x") ----> ¬x
---         litComp(Neg(Var "x2")) ----> x2
litComp :: PL -> PL
litComp phi= case phi of
    Var x -> Neg (Var x)
    Neg (Var x) -> Var x
    _ -> error $ "litComp: phi no es literal, phi="++(show phi)


-- Dada una termino de PL, representada por una lista de literales ll,
-- terminoVAL determina si ll es una termino valida.
-- ll es valida sii ll tiene al menos dos literales complementarios.
-- Ejemplo:     terminoVal [Var "x" , Neg (Var "x")] ----> True
--              terminoVal [Bot,Var "x" , Neg (Var "x"), Top, Var "x3"] ----> True 
terminoVal :: [PL] -> Bool
terminoVal ll = case ll of
    [] -> False
    (l:ls) -> estaLista (litComp l) ll || terminoVal ls

    
--Nos dice si el elemento PL esta en una lista
-- Ejemplo: 
estaLista :: PL -> [PL] -> Bool
estaLista p [] = False
estaLista p (l:ls) = if (p==l) then True else estaLista p ls


-- Dada phi en DNF, representada como una lista de listas de literales lc,
-- termListTrue determina si todas las termino de lc son validas.
-- Es decir clauListTrue determina si todos los elementos de lc son termino validas.
--- Ejemplo:    dnf2LListLit(Var "x") 
dnf2LListLit :: PL -> [[PL]]
dnf2LListLit phi = case phi of
    Bot -> []
    Var x -> [[Var x]]
    Neg (Var x) -> [[Neg (Var x)]]
    (Con alpha beta) -> (dnf2LListLit alpha) ++ (dnf2LListLit beta)
    _ -> error $ "dnf2LListLit: phi no esta en DNF, phi="++(show phi)


--dnfEstaSAT :: PL -> Bool
--sdnfEstaSAT p = if terminoVal(dnf2LListLit (p)) then True else False

