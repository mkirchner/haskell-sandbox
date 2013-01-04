--
-- twobytwobytwo.hs
--
-- Copyright (C) 2012 Marc Kirchner
--
-- Solves a 2x2x2 Rubik's cube using a breadth-first search.
-- The code closely follows the rush hour problem solution in
-- [Bird, 2010, Chapter 18].
--
-- This is a slow solution: the length of the frontier in the
-- BFS can grow exponentially; if one is interested in a valid
-- solution only, a simple BFS->DFS switch (by redefining the
-- "otherwise" branch in bfsearch) may yield acceptable run times.
--

import Data.List

data Color = R | G | B | W | Y | O deriving (Eq, Show)
type Face = (Color, Color, Color, Color)
type State = (Face, Face, Face, Face, Face, Face)

data Move = Fwd | Rgt | Bck | Lft | Up | Dwn deriving (Eq, Show)
type Path = ([Move], State)
type Frontier = [Path]


move :: State -> Move -> State
move s m
    | m == Fwd = fwd s
    | m == Rgt = rgt s
    | m == Bck = bck s
    | m == Lft = lft s
    | m == Up = up s
    | m == Dwn = dwn s

-- return all moves except the inverse move o
moves :: Path -> [Move]
allmoves = [Fwd, Rgt, Bck, Lft, Up, Dwn]
moves ([], s) = allmoves
moves ((m:ms), s)
    | m == Fwd = allmoves \\ [Bck]
    | m == Rgt = allmoves \\ [Lft]
    | m == Bck = allmoves \\ [Fwd]
    | m == Lft = allmoves \\ [Rgt]
    | m == Up = allmoves \\ [Dwn]
    | m == Dwn = allmoves \\ [Up]
    | otherwise = error "Bad error."
    where allmoves = [Fwd, Rgt, Bck, Lft, Up, Dwn]

cube :: State
cube = ( (R, R, R, R),  (G, G, G, G),  (B, B, B, B),  (W, W, W, W),  (Y, Y, Y, Y),  (O, O, O, O) )

solved :: State -> Bool
solved ( (a, b, c, d),  (e, f, g, h),  (i, j, k, l),  (m, n, o, p),  (q, r, s, t),  (u, v, w, x) ) = 
    all (==a) [b,c,d] && all (==e) [f,g,h] && all (==m) [n,o,p] && all (==q) [r,s,t] && all (==u) [v,w,x]

-- movements
lft :: State -> State
lft ( (a, b, c, d),  (e, f, g, h),  (i, j, k, l),  (m, n, o, p),  (q, r, s, t),  (u, v, w, x) ) = 
  ( (q, b, s, d),  (e, f, g, h),  (i, v, k, x),  (o, m, p, n),  (l, r, j, t),  (u, c, w, a) )

rgt :: State -> State
rgt = lft . lft . lft

fwd :: State -> State
fwd ( (a, b, c, d),  (e, f, g, h),  (i, j, k, l),  (m, n, o, p),  (q, r, s, t),  (u, v, w, x) ) = 
    ( (c, a, d, b),  (s, f, t, h),  (i, j, k, l),  (m, u, o, v),  (q, r, p, n),  (g, e, w, x) )

bck :: State -> State
bck = fwd . fwd . fwd

up :: State -> State
up ( (a, b, c, d),  (e, f, g, h),  (i, j, k, l),  (m, n, o, p),  (q, r, s, t),  (u, v, w, x) ) = 
    ( (e, f, c, d),  (i, j, g, h),  (m, n, k, l),  (a, b, o, p),  (s, q, t, r),  (u, v, w, x) )

dwn :: State -> State
dwn = up . up . up

-- search
bfsearch :: [State] -> Frontier -> Maybe [Move]
bfsearch qs [] = Nothing
bfsearch qs (p@(ms,q):ps)
    | solved q = Just ms
    | q `elem` qs = bfsearch qs ps -- has already been explored
    | otherwise = bfsearch (q:qs) (ps ++ succs p)
    where
        succs :: Path -> [Path]
        succs p@(ms, q) = [(ms ++ [m], move q m) | m <- moves p]

solvetwobytwo :: State -> Maybe [Move]
solvetwobytwo s = bfsearch [] [([],s)]
