import System.IO
import System.Exit

import Control.Monad
import Data.Char

data Color = R | B | BB deriving (Show)
data Tree elt = E | EE | T Color (Tree elt) elt (Tree elt) deriving (Show)

type Set a = Tree a
empty :: Set elt
empty = E
insert :: Ord elt => elt -> Set elt -> Set elt
insert x s = blacken (ins s)
  where ins E = T R E x E
        ins (T color a y b) | x < y = balance color (ins a) y b
                            | x == y = T color a y b
                            | x > y = balance color a y (ins b)
        blacken (T R (T R a x b) y c) = T B (T R a x b) y c
        blacken (T R a x (T R b y c)) = T B a x (T R b y c)
        blacken t = t
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance BB a x (T R (T R b y c) z d) = T B (T B a x b) y (T B c z d)
balance BB (T R a x (T R b y c)) z d = T B (T B a x b) y (T B c z d)
balance color a x b = T color a x b

delete :: Ord elt => elt -> Set elt -> Set elt
delete x s = del (redden s)
  where del E = E
        del (T R E y E) | x == y = E
                        | x /= y = T R E y E
        del (T B E y E) | x == y = EE
                        | x /= y = T B E y E
        del (T B (T R E y E) z E) | x < z = T B (del (T R E y E)) z E
                                  | x == z = T B E y E
                                  | x > z = T B (T R E y E) z E
        del (T c a y b) | x < y = rotate c (del a) y b
                        | x == y = let (yP,bP) = min_del b
                                   in rotate c a yP bP
                        | x > y = rotate c a y (del b)

        redden (T B (T B a x b) y (T B c z d)) =
            T R (T B a x b) y (T B c z d)
        redden t = t
rotate R (T BB a x b) y (T B c z d) = balance B (T R (T B a x b) y c) z d
rotate R EE y (T B c z d) = balance B (T R E y c) z d
rotate R (T B a x b) y (T BB c z d) = balance B a x (T R b y (T B c z d))
rotate R (T B a x b) y EE = balance B a x (T R b y E)
rotate B (T BB a x b) y (T B c z d) = balance BB (T R (T B a x b) y c) z d
rotate B EE y (T B c z d) = balance BB (T R E y c) z d
rotate B (T B a x b) y (T BB c z d) = balance BB a x (T R b y (T B c z d))
rotate B (T B a x b) y EE = balance BB a x (T R b y E)
rotate B (T BB a w b) x (T R (T B c y d) z e) =
  T B (balance B (T R (T B a w b) x c) y d) z e
rotate B EE x (T R (T B c y d) z e) = T B (balance B (T R E x c) y d) z e
rotate B (T R a w (T B b x c)) y (T BB d z e) =
  T B a w (balance B b x (T R c y (T B d z e)))
rotate B (T R a w (T B b x c)) y EE = T B a w (balance B b x (T R c y E))
rotate color a x b = T color a x b
min_del (T R E x E) = (x,E)
min_del (T B E x E) = (x,EE)
min_del (T B E x (T R E y E)) = (x,T B E y E)
min_del (T c a x b) = let (xP,aP) = min_del a
                      in (xP,rotate c aP x b)

main = forever (printMenu >> readChoice >>= menuAction)

printMenu = putStr "\na) Insertar Numero\nb)xit\nyour choice: " >> hFlush stdout

readChoice = hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> getChar
readNum = hSetBuffering stdin NoBuffering >> hSetEcho stdin False >> getLine

menuAction 'a' = do
 lastName <- getLine
 putStrLn "Digite el elemento a insertar: "
 n <- getLine
 let r = empty
 print(insert n r)

menuAction 'e' = exitSuccess
menuAction _ = hPutStrLn stderr "\nInvalid choice."