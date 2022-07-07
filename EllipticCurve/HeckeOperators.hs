import Math.NumberTheory.Primes (factorise,Prime,unPrime)
import Data.Bifunctor (second)

data TwoVarPoly a = VarX | VarY | ConstantTerm a | Product (TwoVarPoly a) (TwoVarPoly a) | Sum (TwoVarPoly a) (TwoVarPoly a)

evalPoly :: (Num a,Num b) => (b -> a) -> (a,a) -> TwoVarPoly b -> a
evalPoly _ (xval,_) VarX = xval
evalPoly _ (_,yval) VarY = yval
evalPoly numConverter _ (ConstantTerm x) = numConverter x
evalPoly numConverter (xval,yval) (Product xs ys) = evalPoly numConverter (xval,yval) xs*evalPoly numConverter (xval,yval) ys
evalPoly numConverter (xval,yval) (Sum xs ys) = evalPoly numConverter (xval,yval) xs+evalPoly numConverter (xval,yval) ys

myShow :: (Show a) => (String,String) -> TwoVarPoly a -> String
myShow (xstr,_) VarX = xstr
myShow (_,ystr) VarY = ystr
myShow (_,_) (ConstantTerm x) = show x
myShow (xstr,ystr) (Product xs ys) = "(" ++ myShow (xstr,ystr) xs ++ ") * (" ++ myShow (xstr,ystr) ys ++ ")"
myShow (xstr,ystr) (Sum xs ys) = "(" ++ myShow (xstr,ystr) xs ++ ") + (" ++ myShow (xstr,ystr) ys ++ ")"

newtype HeckeOperator = HeckeOperator{factors :: [(Prime Integer,Either Word (TwoVarPoly Integer))]}

instance Show HeckeOperator where
    show HeckeOperator{factors=f} = show $ showHelper <$> f where
        showHelper (p,Left x) = (p,Left x)
        showHelper (p,Right x) = (p,Right $ myShow ("T_p","p^{k-1}") x)

tn :: Integer -> Maybe HeckeOperator
tn x
    | x<1 = Nothing
    | otherwise = Just $ HeckeOperator{factors=second Left <$> factorise x}

heckeProduct :: HeckeOperator -> HeckeOperator -> HeckeOperator
heckeProduct HeckeOperator{factors = f} HeckeOperator{factors = g} = HeckeOperator{factors = f++g}

contextConvert :: ((a,b) -> (a,c)) -> (a,Either b c) -> (a,Either b c)
contextConvert f (x,Left y) = second Right $ f (x,y)
contextConvert _ zs = zs

fullExpand :: HeckeOperator -> HeckeOperator
fullExpand HeckeOperator{factors = f} = HeckeOperator{factors = contextConvert expandForP <$> f}

expandForP :: (Prime Integer,Word) -> (Prime Integer,TwoVarPoly Integer)
expandForP (p,power)
        | power==0 = (p,ConstantTerm 1)
        | power==1 = (p,VarX)
        | otherwise = (p,Sum (Product tpToRExpansion VarX) (Product (Product (ConstantTerm (-1)) VarY) tpToRm1Expansion))
            where tpToRExpansion = snd $ expandForP (p,power-1)
                  tpToRm1Expansion = snd $ expandForP (p,power-2)

contextRights :: [(a,Either b c)] -> [(a,c)]
contextRights [] = []
contextRights ((_,Left _):xs) = contextRights xs
contextRights ((y,Right z):xs) = (y,z):contextRights xs

heckeEigenValue :: (Num a) => Integer -> (Prime Integer -> a) -> HeckeOperator -> a
heckeEigenValue k lambdaPs op = product $ heckeEigenValueHelper fromInteger k lambdaPs <$> contextRights (factors $ fullExpand op) where
    heckeEigenValueHelper :: (Num a,Num b) => (b -> a) -> Integer -> (Prime Integer -> a) -> (Prime Integer,TwoVarPoly b) -> a
    heckeEigenValueHelper numConverter k lambdaPs (p,poly) = evalPoly numConverter (lambdaPs p,fromInteger $ unPrime p^(k-1)) poly