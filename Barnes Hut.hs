-- CS300-SP17 Assignment 2: Barnes Hut Simulation
-- Deadline: 24 Feb 9pm
-- Submission: via LMS only
-- Mujahid Khan ; 18100245
import System.Environment
import Data.List
import Graphics.Rendering.OpenGL hiding (($=))
import Graphics.UI.GLUT
import Control.Applicative
import Data.IORef
import Debug.Trace
import Data.Function (on)
--
-- PART 1: You are given many input files in the inputs directory and given 
-- code can read and parse the input files. You need to run "cabal update 
-- && cabal install glut" on your system to work on this assignment. Then
-- run "./ghc BH.hs && ./BH 1 < inputs/planets.txt" to run 1 iteration ofHN
-- the algorithm and print updated positions. Replace 1 with any number to
-- run more iteration. You may also run it without an argument and it will
-- display the simulation using OpenGL in a window.
--
-- In the first part, you are to write the updateBody function to correctly 
-- update body after 1 unit of time has passed. You have to correctly 
-- update the position of a body by calculating the effect of all other
-- bodies on it. The placeholder implementation just moves the body left
-- without doing any physics. Read http://www.cs.princeton.edu/courses/
-- archive/fall03/cs126/assignments/nbody.html for help with physics. Try
-- simplyfying equations on paper before implementing them. You can compare
-- answers with the given binary solution.
--
-- Make helper functions as needed
--helper function calculate F
g = 6.67e-11
type Vec2 = (Double, Double)
data Body = Body Vec2 Vec2 Double (Color3 Double)
manDist :: (Double, Double) -> (Double, Double) -> Double
manDist (posx, posy) (px, py) = sqrt ((px-posx)^2 + (py-posy)^2)

gravForce :: Body -> Body -> (Double, Double) -> (Double, Double)
gravForce (Body (posx, posy) v1 m1 c) (Body (px, py) v2 m2 clr) (accx, accy) | (manDist (posx, posy) (px, py)) == 0 =
    (accx, accy)
gravForce (Body (posx, posy) v m c) (Body (px, py) vel mass clr) (accx, accy) =
 let r = (manDist (posx, posy) (px, py))
     f = (g*m*mass) / (r^2)
     fx = (f*(posx-px)) / r
     fy = (f*(posy-py)) / r
 in (fx+accx, fy+accy)


updateBody :: (Foldable f) => f Body -> Body -> Body
updateBody listOfBodies (Body (posx, posy) (vx,vy) mass clr) =
    let (fx, fy) = foldr (\ (Body (px, py) v m c) (accx, accy) -> gravForce (Body (posx, posy) (vx,vy) mass clr) (Body (px, py) v m c) (accx, accy)) (0, 0) listOfBodies
        (ax,ay) = (-fx / mass, -fy / mass)
    in Body (ax+vx+posx,ay+vy+posy) (vx+ax, vy+ay) mass clr
-- updateBody _ (Body (posx,posy) vel mass clr) = 
    -- Body (posx-100000000,posy) vel mass clr

-- PART 2: We will make a Quadtree to represent our universe. See 
-- http://www.cs.princeton.edu/courses/archive/fall03/cs126/assignments/
-- barnes-hut.html for help on this tree. The QT structure has the the
-- length of one side of quadrant) for internal nodes. The makeQT function
-- has arguments: center, length of quadrant side, a function to find
-- coordinates of an element of the tree (e.g. extract position from a Body
-- object), a function to summarize a list of nodes (e.g. to calculate a
-- Body with position center of gravity and total mass), and the list of
-- nodes to put in tree.
--
-- Note that inserting all nodes at once is much easier than inserting one
-- by one. Think of creating the root node (given all nodes), and then
-- divide the nodes into quadrants and let recursive calls create the
-- appropriate trees. In this part you do not have to give a correct
-- implementation of the summary function
--
-- Now make QT member of Foldable typeclass. See what function you are
-- required to implement. Once this is done, change the tick function below 
-- to create the tree and then pass it to updateBody function instead of a 
-- list of bodies. No change to updateBody function should be needed since
-- it works with any Foldable.
centerOfMass :: Vec2 -> Vec2 -> Double -> Double -> (Vec2, Double)
centerOfMass (x1,y1) (x2,y2) m1 m2 =
 let c1 = (x1*m1) + (x2*m2)
     c2 = (y1*m1) + (y2*m2)
     m = m1 + m2
     x = c1 / m
     y = c2 / m
 in ((x,y), m)

--tupleAdder :: (Vec2, Double) -> (Vec2, Double) -> (Vec2, Double)
--tupleAdder ((x1,y1), m1) ((x2,y2), m2) = ((x1+x2, y1+y2), m1+m2)

summarize :: [Body] -> Body
summarize listOfBodies  = 
    let ((com1, com2), m) = foldr (\(Body (posx, posy) _ m _) ((px, py), totalMass) -> centerOfMass (posx, px) (posy, py) m totalMass ) ((0,0), 0) listOfBodies
    in Body (com1, com2) (0,0) m (Color3 0 0 0)


getPos :: Body -> Vec2
getPos (Body pos _ _ _) = pos
--getPos ((Body pos _ _ _):bs) = [pos] ++ getPos bs

--calculateQuadrant :: Vec2 -> Double -> Vec2 -> (Int, Vec2)
--calculateQuadrant (c1, c2) radius position 
--    | ( ((fst position) < c1+radius) && ( (fst position) > c1) ) && ( ( (snd position) < c2+radius) && ((snd position) > c2) ) = (2, position)
--    | ( ((fst position) > c1-radius) && ( (fst position) < c1) ) && ( ( (snd position) < c2+radius) && ((snd position) > c2) ) = (1, position)
--    | ( ((fst position) > c1-radius) && ( (fst position) < c1) ) && ( ( (snd position) > c2-radius) && ((snd position) < c2) ) = (3, position)
--    | ( ((fst position) < c1+radius) && ( (fst position) > c1) ) && ( ( (snd position) > c2-radius) && ((snd position) < c2) ) = (4, position)

----calculateQuadrantS :: Vec2 -> Double -> [Vec2] -> [(Int, Vec2)]
----calculateQuadrantS c radius [position] = [calculateQuadrant c radius position]
----calculateQuadrantS c radius (p:ps) = [calculateQuadrant c radius p] ++ calculateQuadrantS c radius ps
--calculateQuadrantS :: Vec2 -> Double -> [Vec2] -> [(Int, Vec2)]
--calculateQuadrantS c radius [position] = [calculateQuadrant c radius position]
--calculateQuadrantS c radius (p:ps) = [calculateQuadrant c radius p] ++ calculateQuadrantS c radius ps
--     --c1 + r, c2 + r
--     --c1 + r, c2 - r
--     --c1 - r, c2 + r
--     --c1 - r, c2 - r
--calculateBodiesInQuadrant1 :: (Int, Vec2) -> [Body] -> [(Int, Body)]
--calculateBodiesInQuadrant1 (i, (px, py)) [(Body (posx, posy) _ _ _)] = [(i, (Body (posx, posy) _ _ _))] 
--calculateBodiesInQuadrant1 (i, (px, py) ((Body pos _ _ _):bs) = [(i, Body pos v m c)] ++ calculateBodiesInQuadrant1 (i, (px,py)) bs


data QT a = Internal Double a (QT a,QT a,QT a,QT a) | Leaf a | Nil 
makeQT :: Vec2 -> Double -> (a->Vec2) -> ([a]->a) -> [a] -> (QT a)
makeQT (cx,cy) radius getPos summarize [] = Nil
makeQT (cx,cy) radius getPos summarize [body] = Leaf body
makeQT (cx,cy) radius getPos summarize bodies = 
 Internal radius (summarize bodies) (w, x, y, z)
 where w = makeQT (cx - (radius/2), cy + (radius/2)) (radius/2) getPos summarize (filter (\ b -> (fst (getPos b) < cx) && (snd (getPos b) >= cy) ) bodies) 
       x = makeQT (cx + (radius/2), cy + (radius/2)) (radius/2) getPos summarize (filter (\ b -> (fst (getPos b) >= cx) && (snd (getPos b) > cy) ) bodies) 
       y = makeQT (cx - (radius/2), cy - (radius/2)) (radius/2) getPos summarize (filter (\ b -> (fst (getPos b) <= cx) && (snd (getPos b) < cy) ) bodies) 
       z = makeQT (cx + (radius/2), cy - (radius/2)) (radius/2) getPos summarize (filter (\ b -> (fst (getPos b) >= cx) && (snd (getPos b) <= cy) ) bodies) 

-- This functions takes a set of bodies and returns an updated set of 
-- bodies after 1 unit of time has passed (dt=1)
instance Foldable QT where
    foldr f acc Nil  = acc
    foldr f acc (Leaf a) = f a acc
    foldr f acc (Internal r a (b,c,d,e)) = foldr f (foldr f (foldr f (foldr f acc b) c) d) e 




tick ::Double -> [Body] -> [Body]
tick radius bodies = 
    let q = makeQT (0,0) radius getPos summarize bodies
    in fmap (updateBody q) bodies

-- PART 3: Now we create another datatype that contains a quadtree and a 
-- function which given radius and a summarized body (containing center of
-- gravity and total mass) returns true if the summarized body is a good
-- enough approximation. Use 0.5 as threshold.
--
-- Make a correct summarize function to pass to makeQT above and then make
-- BH an instance of Foldable typeclass as well. However this instance
--
-- Make a correct summarize function to pass to makeQT above and then make
-- BH an instance of Foldable typeclass as well. However this instance
-- should use the internal node if the predicate function returns true and
-- recurse only if it returns false. Make sure to recurse over a BH type
-- variable. If your implementation is correct, you will be as fast as the
-- provided binary BH2 on large inputs like galaxy1.txt
data BH a = BH (Double -> a -> Bool) (QT a)

---------------------------------------------------------------------------
-- You don't need to study the code below to work on the assignment
---------------------------------------------------------------------------
main :: IO ()
main = do
    (_,args) <- getArgsAndInitialize
    stdin <- getContents
    uncurry (mainChoice args) (parseInput stdin)

mainChoice :: [String] -> Double -> [Body] -> IO ()
mainChoice (iter:_) r bodies = putStr $ applyNtimes r bodies (read iter)
mainChoice [] r bodies = do
    createWindow "Barnes Hut"
    windowSize $= Size 700 700
    bodiesRef <- newIORef bodies
    ortho2D (-r) r (-r) r
    displayCallback $= (display r bodiesRef)
    addTimerCallback 10 (timer r bodiesRef)
    mainLoop

applyNtimes :: Double -> [Body] -> Int -> String
applyNtimes r bodies n = (unlines.map show) (iterate (tick r) bodies !! n)

parseInput :: String -> (Double, [Body])
parseInput input = 
    let (cnt:r:bodies) = lines input
    in (read r, map read (take (read cnt) bodies))

dispBody :: Body -> IO ()
dispBody (Body (x,y) _ _ rgb) = color rgb >> vertex (Vertex2 x y)

display :: Double -> IORef [Body] -> IO ()
display r bodiesRef = do
    clear [ColorBuffer]
    bodies <- get bodiesRef
    renderPrimitive Points (mapM_ dispBody bodies)
    flush

timer :: Double -> IORef [Body] -> IO ()
timer r bodiesRef = do
    postRedisplay Nothing
    bodies <- get bodiesRef
    bodiesRef $= tick r bodies 
    addTimerCallback 10 (timer r bodiesRef)

instance Read Body where
    readsPrec _ input = 
        let (x:y:vx:vy:m:r:g:b:rest) = words input
        in (\str -> [(Body (read x,read y) (read vx,read vy) (read m) 
            (Color3 ((read r)/255) ((read g)/255) ((read b)/255)), 
            unwords rest)]) input

instance Show Body where
    show (Body (x,y) (vx,vy) _ _) =
        "x= " ++ show x ++ " y= " ++ show y ++ " vx= " ++
        show vx ++ " vy= " ++ show vy