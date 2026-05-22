--Probado con cabal 4.18.2.1
module Practica05 where

import Terminos

--funcion auxiliar para checar si una variable ocurre en un termino
ocurre :: Nombre -> Term -> Bool
ocurre x (Var y) = x == y
ocurre x (Fun _ args) = any (ocurre x) args

--Aplicar una sustitucion a un termino
apsubT :: Term -> Subst -> Term
apsubT (Var x) s = case lookup x s of
    Just t  -> t
    Nothing -> Var x
apsubT (Fun f args) s = Fun f (aplicarLista args s)

--Funcion auxiliar para aplicar la sustitucion a una lista de terminos
aplicarLista :: [Term] -> Subst -> [Term]
aplicarLista ts s = map (\t -> apsubT t s) ts

--Funcion que elimina los pares que son de la forma x=x
simpSus :: Subst -> Subst
simpSus = filter (\(x, t) -> t /= Var x)

--Funcion que calcula la composicion de dos sustituciones
compSus :: Subst -> Subst -> Subst
compSus s1 s2 = simpSus 
                ([(x, apsubT t s2) | (x, t) <- s1] ++ [(y, t') 
                    | (y, t') <- s2, notElem y domS1])
    where domS1 = map fst s1

--Funcion que devuelve un umg de dos terminos, si es que lo hay
unifica :: Term -> Term -> [Subst]
unifica t1 t2 | t1 == t2 = [[]]
unifica (Var x) t2
    | ocurre x t2 = []
    | otherwise = [[(x, t2)]]
unifica t1 (Var x)
    | ocurre x t1 = []
    | otherwise = [[(x, t1)]]
unifica (Fun f1 args1) (Fun f2 args2)
    | f1 == f2 && length args1 == length args2 = unificaListas args1 args2
    | otherwise = []

--Funcion que devuelve un unificador de dos términos funcionales, si es que lo hay
unificaListas :: [Term] -> [Term] -> [Subst]
unificaListas [] [] = [[]]
unificaListas (t1:ts1) (t2:ts2) = [ compSus s1 s2 | s1 <- unifica t1 t2,
        s2 <- unificaListas (aplicarLista ts1 s1) (aplicarLista ts2 s1) ]
unificaListas _ _ = []

--Funcion que devuelve un umg de una lista de termino, si es que lo hay
unificaConj :: [Term] -> [Subst]
unificaConj [] = [[]]
unificaConj [_] = [[]]
unificaConj (t1:t2:ts) = [ compSus s1 s2 | s1 <- unifica t1 t2, s2 <- unificaConj (aplicarLista (t2:ts) s1) ]
