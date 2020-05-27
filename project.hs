import Data.Char
import Data.List
import Debug.Trace (traceShow)

data Expression = Var String -- variabile
  | And Expression Expression -- conjunctia a doua formule
  | Or Expression Expression -- disjunctia
  | Not Expression -- negatia unei formule
  deriving Eq

instance Show Expression where
  show (Var x) = x
  show (Not f) = "!" ++ (show f)
  show (And f1 f2) = "(" ++ (show f1) ++ " * " ++ (show f2) ++ ")"
  show (Or f1 f2) = "(" ++ (show f1) ++ " + " ++ (show f2) ++ ")"

data Assumption = Equal Expression Expression
                deriving Eq

instance Show Assumption where
    show (Equal e1 e2) =  "(" ++ (show e1) ++ " = " ++ (show e2) ++ ")"

and_x_x :: Expression -> Expression
and_x_x (And f1 f2) = if f1==f2
                      then traceShow ("Annihilator and: " ++ (show (And f1 f2)) ++ " -> " ++ (show f1)) (f1)
                      else (And f1 f2)
and_x_x f = f

and_x_x' :: Expression -> Expression -> Expression
and_x_x' f1 f2 = if (equality f1 f2)==True
                 then f1
                 else (and_x_x f1)

or_x_x :: Expression -> Expression
or_x_x (Or f1 f2) = if f1==f2
                    then traceShow ("Annihilator or: " ++ (show (Or f1 f2)) ++ " -> " ++ (show f1)) (f1)
                    else (Or f1 f2)
or_x_x f = f

or_x_x' :: Expression -> Expression -> Expression
or_x_x' f1 f2 = if (equality f1 f2)==True
                then f1
                else (or_x_x f1)

factor_comun :: Expression -> Expression
factor_comun (Or (And f1 f2) (And f3 f4)) = if f1==f3
                                            then traceShow ("Factor comun: " ++ (show (Or (And f1 f2) (And f3 f4))) ++ " -> " ++ (show (And f1 (Or f2 f4)))) (And f1 (Or f2 f4))
                                            else if f1==f4
                                                  then traceShow ("Factor comun: " ++ (show (Or (And f1 f2) (And f3 f4))) ++ " -> " ++ (show (And f1 (Or f2 f3)))) (And f1 (Or f2 f3))
                                                  else if f2==f3
                                                       then traceShow ("Factor comun: " ++ (show (Or (And f1 f2) (And f3 f4))) ++ " -> " ++ (show (And f2 (Or f1 f4)))) (And f2 (Or f1 f4))
                                                       else if f2==f4
                                                            then traceShow ("Factor comun: " ++ (show (Or (And f1 f2) (And f3 f4))) ++ " -> " ++ (show (And f2 (Or f1 f3)))) (And f2 (Or f1 f3))
                                                            else (Or (And f1 f2) (And f3 f4))
factor_comun (Or f1 (And f2 f3)) = if f1==f2
                                   then traceShow ("Factor comun: " ++ (show (Or f1 (And f2 f3))) ++ " -> " ++ (show f1)) (f1)
                                   else if f1==f3
                                        then traceShow ("Factor comun: " ++ (show (Or f1 (And f2 f3))) ++ " -> " ++ (show f1)) (f1)
                                        else if f1==(Not f2)
                                             then traceShow ("Factor comun: " ++ (show (Or f1 (And f2 f3))) ++ " -> " ++ (show (And f1 f3))) (And f1 f3)
                                             else if f3==(Not f1)
                                                  then traceShow ("Factor comun: " ++ (show (Or f1 (And f2 f3))) ++ " -> " ++ (show (And f1 f3))) (And f1 f3)
                                                  else if f1==(Not f2)
                                                       then traceShow ("Factor comun: " ++ (show (Or f1 (And f2 f3))) ++ " -> " ++ (show (And f1 f3))) (And f1 f3)
                                                       else if f2==(Not f1)
                                                            then traceShow ("Factor comun: " ++ (show (Or f1 (And f2 f3))) ++ " -> " ++ (show (And f1 f3))) (And f1 f3)
                                                            else (Or f1 (And f2 f3))
factor_comun (Or (And f1 f2) f3) = if f1==f3
                                   then traceShow ("Factor comun: " ++ (show (Or (And f1 f2) f3)) ++ " -> " ++ (show f1)) (f1)
                                   else if f2==f3
                                        then traceShow ("Factor comun: " ++ (show (Or (And f1 f2) f3)) ++ " -> " ++ (show f2)) (f2)
                                        else if f1==(Not f3)
                                             then traceShow ("Factor comun: " ++ (show (Or (And f1 f2) f3)) ++ " -> " ++ (show (And f3 f2))) (And f3 f2)
                                             else if f2==(Not f3)
                                                  then traceShow ("Factor comun: " ++ (show (Or (And f1 f2) f3)) ++ " -> " ++ (show (And f1 f3))) (And f1 f3)
                                                  else if f3==(Not f1)
                                                       then traceShow ("Factor comun: " ++ (show (Or (And f1 f2) f3)) ++ " -> " ++ (show (And f3 f2))) (And f3 f2)
                                                       else if f3==(Not f2)
                                                            then traceShow ("Factor comun: " ++ (show (Or (And f1 f2) f3)) ++ " -> " ++ (show (And f1 f3))) (And f1 f3)
                                                            else (Or (And f1 f2) f3)
factor_comun (And f1 (And f2 f3)) = if f1==f2
                                    then traceShow ("Factor comun: " ++ (show (And f1 (And f2 f3))) ++ " -> " ++ (show (And f2 f3))) (And f2 f3)
                                    else if f1==f3
                                         then traceShow ("Factor comun: " ++ (show (And f1 (And f2 f3))) ++ " -> " ++ (show (And f2 f3))) (And f2 f3)
                                         else (And f1 (And f2 f3))
factor_comun (And (And f1 f2) f3) = if f1==f3
                                    then traceShow ("Factor comun: " ++ (show (And (And f1 f2) f3)) ++ " -> " ++ (show (And f1 f2))) (And f1 f2)
                                    else if f2==f3
                                         then traceShow ("Factor comun: " ++ (show (And (And f1 f2) f3)) ++ " -> " ++ (show (And f1 f2))) (And f1 f2)
                                         else (And (And f1 f2) f3)
factor_comun (And f1 (Or (Not f2) f3)) = if f1==f2
                                         then traceShow ("Factor comun: " ++ (show (And f1 (Or (Not f2) f3))) ++ " -> " ++ (show (And f1 f3))) (And f1 f3)
                                         else (And f1 (Or (Not f2) f3))
factor_comun (And f1 (Or f2 (Not f3))) = if f1==f3
                                         then traceShow ("Factor comun: " ++ (show (And f1 (Or f2 (Not f3)))) ++ " -> " ++ (show (And f1 f2))) (And f1 f2)
                                         else (And f1 (Or f2 (Not f3)))
factor_comun (And (Or f1 (Not f2)) f3) = if f2==f3
                                         then traceShow ("Factor comun: " ++ (show (And (Or f1 (Not f2)) f3)) ++ " -> " ++ (show (And f1 f2))) (And f1 f2)
                                         else (And (Or f1 (Not f2)) f3)
factor_comun (And (Or (Not f1) f2) f3) = if f1==f3
                                         then traceShow ("Factor comun: " ++ (show (And (Or (Not f1) f2) f3)) ++ " -> " ++ (show (And f1 f2))) (And f1 f2)
                                         else (And (Or (Not f1) f2) f3)
factor_comun f = f

factor_comun' :: Expression -> Expression -> Expression
factor_comun' f1 f2 = if (equality f1 f2)==True
                      then f1
                      else (factor_comun f1)

not_not :: Expression -> Expression
not_not (Not (Not f)) = traceShow ("Anihilare negatie dubla: " ++ (show (Not (Not f))) ++ " -> " ++ (show f)) (f)
not_not f = f

not_not' :: Expression -> Expression -> Expression
not_not' f1 f2 = if(equality f1 f2)==True
                 then f1
                 else (not_not f1)

not_and :: Expression -> Expression
not_and (Not (And f1 f2)) = traceShow ("Not and: " ++ (show (Not (And f1 f2))) ++ " -> " ++ (show (Or (Not f1) (Not f2)))) (Or (Not f1) (Not f2))
not_and f = f

not_and' :: Expression -> Expression -> Expression
not_and' f1 f2 = if (equality f1 f2)==True
                 then f1
                 else (not_and f1)

not_or :: Expression -> Expression
not_or (Not (Or f1 f2)) = traceShow ("Not or: " ++ (show (Not (Or f1 f2))) ++ " -> " ++ (show (And (Not f1) (Not f2)))) (And (Not f1) (Not f2))
not_or f = f

not_or' :: Expression -> Expression -> Expression
not_or' f1 f2 = if (equality f1 f2)==True
                then f1
                else (not_or f1)

or_and_right :: Expression -> Expression
or_and_right (Or f1 (And f2 f3)) = traceShow ("Or distributivity: " ++ (show (Or f1 (And f2 f3))) ++ " -> " ++ (show (And (Or f1 f2) (Or f1 f3)))) (And (Or f1 f2) (Or f1 f3))
or_and_right f = f

or_and_right' :: Expression -> Expression -> Expression
or_and_right' f1 f2 = if (equality f1 f2)==True
                      then f1
                      else (or_and_right f1)

or_and_left :: Expression -> Expression
or_and_left (Or (And f1 f2) f3) = traceShow ("Or distributivity: " ++ (show (Or (And f1 f2) f3)) ++ " -> " ++ (show (And (Or f1 f3) (Or f2 f3)))) (And (Or f1 f3) (Or f2 f3))
or_and_left f = f

or_and_left' :: Expression -> Expression -> Expression
or_and_left' f1 f2 = if (equality f1 f2)==True
                     then f1
                     else (or_and_left f1)

and_assoc :: Expression -> Expression
and_assoc (And (And f1 f2) f3) = traceShow ("And associativity: " ++ (show (And (And f1 f2) f3)) ++ " -> " ++ (show (And f1 (And f2 f3)))) (And f1 (And f2 f3))
and_assoc f = f

and_assoc' :: Expression -> Expression -> Expression
and_assoc' f1 f2 = if (equality f1 f2)==True
                   then f1
                   else (and_assoc f1)

or_assoc :: Expression -> Expression
or_assoc (Or (Or f1 f2) f3) = traceShow ("Or associativity: " ++ (show (Or (Or f1 f2) f3)) ++ " -> " ++ (show (Or f1 (Or f2 f3)))) (Or f1 (Or f2 f3))
or_assoc f = f

or_assoc' :: Expression -> Expression -> Expression
or_assoc' f1 f2 = if (equality f1 f2)==True
                  then f1
                  else (or_assoc f1)

chain :: (Expression -> Expression) -> (Expression -> Expression) -> Expression -> Expression
chain t1 t2 f = let f' = t1 f in
                  if f == f' then t2 f
                  else f'

chain_list :: [Expression -> Expression] -> (Expression -> Expression)
chain_list [] = \x -> x
chain_list (hd : tl) = chain hd (chain_list tl)

topmost :: Expression -> Expression
topmost = chain_list [and_x_x , or_x_x, not_not, factor_comun , or_assoc , and_assoc , or_and_right , or_and_left , not_or , not_and]

once :: Expression -> Expression
once f = apply_once f topmost

fixpoint :: (Expression -> Expression) -> (Expression -> Expression)
fixpoint t f = let f' = t f in
                 if f == f' then f
                 else fixpoint t f'

cnf :: Expression -> Expression
cnf = fixpoint once

equality :: Expression -> Expression -> Bool
equality (Var v1) (Var v2) = if v1==v2
                             then True
                             else False
equality (Not x) (Not x1) = if x==x1
                            then True
                            else False
equality (Or f1 f2) (Or f3 f4) = if (equality f1 f3)==True && (equality f2 f4)==True
                                 then True
                                 else if (equality f1 f4)==True && (equality f2 f3)==True
                                      then True
                                      else False
equality (And f1 f2) (And f3 f4) = if (equality f1 f3)==True && (equality f2 f4)==True
                                   then True
                                   else if (equality f1 f4)==True && (equality f2 f3)==True
                                        then True
                                        else False
equality f1 f2 = if f1==f2
                 then True
                 else False

apply_once :: Expression -> (Expression -> Expression) -> Expression
apply_once f t =
  let f' = t f in
    if f == f' then
      apply_deep f t
    else
      f'

apply_deep :: Expression -> (Expression -> Expression) -> Expression
apply_deep (Not f) t = (Not (apply_once f t))
apply_deep (And f1 f2) t = let f1' = apply_once f1 t in
                             if f1 == f1' then
                               (And f1 (apply_once f2 t))
                             else
                               (And f1' f2)
apply_deep (Or f1 f2) t = let f1' = apply_once f1 t in
                             if f1 == f1' then
                               (Or f1 (apply_once f2 t))
                             else
                               (Or f1' f2)
apply_deep (Var x) _ = (Var x)

data Token = TVar String | TLParen | TRParen | TAnd | TOr | TNot | TEqual deriving (Eq, Show)

tokenize :: String -> Maybe [Token]
tokenize [] = Just []
tokenize ('(' : tl) = case tokenize tl of
                        Nothing -> Nothing
                        Just tl' -> Just (TLParen : tl')
tokenize (')' : tl) = case tokenize tl of
                        Nothing -> Nothing
                        Just tl' -> Just (TRParen : tl')
tokenize ('*' : tl) = case tokenize tl of
                        Nothing -> Nothing
                        Just tl' -> Just (TAnd : tl')
tokenize ('+' : tl) = case tokenize tl of
                        Nothing -> Nothing
                        Just tl' -> Just (TOr : tl')
tokenize ('!' : tl) = case tokenize tl of
                        Nothing -> Nothing
                        Just tl' -> Just (TNot : tl')
tokenize ('=' : tl) = case tokenize tl of
                        Nothing -> Nothing
                        Just tl' -> Just (TEqual : tl')
tokenize (c : tl) | isAlpha c = case tokenize tl of
                        Nothing -> Nothing
                        Just tl' -> Just (TVar [c] : tl')
tokenize (c : tl) | isSpace c = tokenize tl
tokenize _ = Nothing

parse_expr = parse_disjs

parse_disjs :: [Token] -> Maybe (Expression, [Token])
parse_disjs tokens =
  case parse_conjs tokens of
    Nothing -> Nothing
    Just (f1, []) -> Just (f1, [])
    Just (f1, TOr : tokens') ->
      case parse_disjs tokens' of
        Nothing -> Nothing
        Just (f2, tokens'') -> Just (Or f1 f2, tokens'')
    r -> r

parse_conjs :: [Token] -> Maybe (Expression, [Token])
parse_conjs tokens =
  case parse_negs tokens of
    Nothing -> Nothing
    Just (f1, []) -> Just (f1, [])
    Just (f1, TAnd : tokens') ->
      case parse_conjs tokens' of
        Nothing -> Nothing
        Just (f2, tokens'') -> Just (And f1 f2, tokens'')
    r -> r

parse_negs :: [Token] -> Maybe (Expression, [Token])
parse_negs (TVar var : tokens) = Just (Var var, tokens)
parse_negs (TNot : tokens) = case parse_negs tokens of
                               Nothing -> Nothing
                               Just (f, tokens') -> Just (Not f, tokens')
parse_negs (TLParen : tokens) = case parse_expr tokens of
  Nothing -> Nothing
  Just (f, TRParen : tokens') -> Just (f, tokens')
  _ -> Nothing
parse_negs _ = Nothing

parse_assumption :: [Token] -> Maybe(Assumption, [Token])
parse_assumption tokens = 
    case parse_expr tokens of
        Nothing -> Nothing
        Just(f1, TEqual : tokens') ->
            case parse_expr tokens' of
                Nothing -> Nothing
                Just(f2, tokens'') -> Just (Equal f1 f2, tokens'')

preprocess_expression :: Expression -> Expression
preprocess_expression (And e1 e2) = if e1==e2
                                    then (preprocess_expression e1)
                                    else (And (preprocess_expression e1) (preprocess_expression e2))
preprocess_expression (Or e1 e2) = if e1==e2
                                    then (preprocess_expression e1)
                                    else (Or (preprocess_expression e1) (preprocess_expression e2))
preprocess_expression (Var v) = (Var v)
preprocess_expression (Not e) = (Not e)

expressions_match :: Expression -> Expression -> Bool
expressions_match (Var v1) (Var v2) = if v1==v2
                                      then True
                                      else False
expressions_match (Not e1) (Not e2) = if e1==e2
                                      then True
                                      else False
expressions_match (And e1 e2) (And e3 e4) = ((expressions_match e1 e3) && (expressions_match e2 e4)) || ((expressions_match e1 e4) && (expressions_match e2 e3))
expressions_match (Or e1 e2) (Or e3 e4) = ((expressions_match e1 e3) && (expressions_match e2 e4)) || ((expressions_match e1 e4) && (expressions_match e2 e3))
expressions_match _ _ = False

allvars :: Expression -> [String]
allvars (Var s) = [s]
allvars (Not e) = allvars e
allvars (And e1 e2) = (allvars e1) ++ (allvars e2)
allvars (Or e1 e2) = (allvars e1) ++ (allvars e2)

replace :: Expression -> String -> Bool -> Expression
replace (Var s) s1 True = if s==s1
                          then (Var "True")
                          else (Var s)
replace (Var s) s1 False = if s==s1
                           then (Var "False")
                           else (Var s)
replace (Not e) s1 x = (Not (replace e s1 x))
replace (And e1 e2) s x = (And (replace e1 s x) (replace e2 s x))
replace (Or e1 e2) s x = (Or (replace e1 s x) (replace e2 s x))

replace_value :: Expression -> [String] -> Int -> Expression
replace_value e [] n = e
replace_value e (x:rest) 0 = replace_value (replace e x False) rest 0
replace_value e (x:rest) n = replace_value (replace e x True) rest (n-1)

replace_in :: Expression -> [String] -> Int -> [Expression]
replace_in e l 0 = [(replace_value e l 0)]
replace_in e l n = [(replace_value e l n)] ++ (replace_in e l (n-1))

expression_result :: Expression -> [[String]] -> [Expression]
expression_result e [] = []
expression_result e (x:rest) = (expression_result e rest) ++ (replace_in e x (length(x))) 

eval :: Expression -> Bool
eval (Var "True") = True
eval (Var "False") = False
eval (Not e) = not (eval e)
eval (And e1 e2) = (eval e1) && (eval e2)
eval (Or e1 e2) = (eval e1) || (eval e2)

evaluate :: [Expression] -> [Expression] -> Bool
evaluate [] _ = True
evaluate (x:rest1) (y:rest2) = if (eval x)==(eval y)
                               then True && (evaluate rest1 rest2)
                               else traceShow("Evaluate is false for : " ++ (show (x)) ++ " = " ++ (show (y))) (False)


-- afisare :: Expression -> Expression -> Maybe Expression
-- afisare (Var x1) (Var x2) = if x1==x2
--                             then Just (Var x1)
--                             else Nothing
-- afisare (Not x1) (Not x2) = if x1==x2
--                             then Just (Not x1)
--                             else Nothing
-- afisare (And x1 x2) (And x3 x4) = if (afisare x1)==Nothing || (afisare x2)==Nothing || (afisare x3)==Nothing || (afisare x4) ==Nothing
--                                   then Nothing
--                                   else if (afisare x1)==(afisare x3) && (afisare x2)==(afisare x4)
--                                        then Just (And x1 x2)
--                                        else if (afisare x1)==(afisare x4) && (afisare x2)==(afisare x3)
--                                             then traceShow("Schimbare ordine in and: " ++ (show (And x1 x2)) " -> " (show (And x2 x1))) (Just (And x2 x1))
--                                             else Nothing
-- afisare (Or x1 x2) (Or x3 x4) = if (afisare x1)==Nothing || (afisare x2)==Nothing || (afisare x3)==Nothing || (afisare x4) ==Nothing
--                                 then Nothing
--                                 else if (afisare x1)==(afisare x3) && (afisare x2)==(afisare x4)
--                                      then Just (Or x1 x2)
--                                      else if (afisare x1)==(afisare x4) && (afisare x2)==(afisare x3)
--                                           then traceShow("Schimbare ordine in and: " ++ (show (Or x1 x2))" -> " (show (Or x2 x1))) (Just (Or x2 x1))
--                                           else Nothing
-- afisare f1 f2 = if f1==f2
--                 then Just f1
--                 else Nothing


ex1 = "x * y + x * y * z = x * y"
ex2 = "x * x + x * x = x"
ex3 = "x * c + g * x= t"
ex4 = "x * y + y * x = x * y"
ex5 = "x * y + y * x = y * x + x * x * y"

calc :: String -> IO()
calc fs = case tokenize fs of
            Nothing -> putStrLn "Eroare lexicala."
            Just tokens -> case parse_assumption tokens of
                            --  Just (f, []) -> do putStrLn $ "Ati introdus formula " ++ (show f)
                             Just (Equal f1 f2, []) -> do putStrLn $ "Ati introdus formula " ++ (show (Equal f1 f2))
                                                          -- putStrLn $ "Transformarea primei expresii este \n" ++ (show $ cnf f1)
                                                          -- putStrLn $ "Transformarea celei de-a doua expresii este \n" ++ (show $ cnf f2)
                                                          -- if (expressions_match (preprocess_expression f1) (preprocess_expression f2))==True
                                                          -- then putStrLn $ "Rezultat : True"
                                                          -- else putStrLn $ "Rezultat : False"
                                                          -- putStrLn $ "FNC a primei expresii este " ++ (show $ (replace_in f1 ((allvars f1) ++ (allvars f2)) (length((allvars f1) ++ (allvars f2)))))
                                                          -- putStrLn $ "Perm " ++ (show $ permutations (union (allvars f1) (allvars f2)))
                                                          -- putStrLn $ "X  " ++ (show $ (expression_result f1 (permutations (union (allvars f1) (allvars f2)))))
                                                          -- putStrLn $ "X  " ++ (show $ (expression_result f2 (permutations (union (allvars f1) (allvars f2)))))
                                                          if (evaluate (expression_result f1 (permutations (union (allvars f1) (allvars f2)))) (expression_result f2 (permutations (union (allvars f1) (allvars f2))))) == True
                                                          then do putStrLn $ "Cele doua expresii sunt echivalente!"
                                                                  putStrLn $ "Transformarea primei expresii este \n" ++ (show $ cnf f1)
                                                                  putStrLn $ "Transformarea celei de-a doua expresii este \n" ++ (show $ cnf f2)
                                                                  -- putStrLn $ "Cele doua expresii : " ++ (show $ afisare f1 f2)
                                                          else putStrLn $ "Cele doua expresii nu sunt echivalente!"
                                                          -- forM_ :: [a] -> (a -> IO b) -> IO ()
                                                          -- forM_ permutations (union (allvars f1) (allvars f2)) $ \s -> do
                                                          --   putStrLn $ "FNC a primei expresii este " ++ (show $ (replace_in f1 s (length(s))))
                             _ -> putStrLn "Eroare de sintaxa."

main :: IO ()
main = do putStrLn "Introduceti o formula logica: "
          fs <- getLine
          (calc fs)