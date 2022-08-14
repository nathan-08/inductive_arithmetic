data Nat = Zero
         | Suc Nat
         deriving (Show)

pre Zero = Zero
pre (Suc n) = n

to_int Zero = 0
to_int (Suc n) = (to_int n) + 1

-- a + (b + 1) = 1 + (a + b)
add a Zero = a
add a (Suc b) = Suc(add a b)

-- a * (b + 1) = a + (a * b)
mult a Zero = Zero
mult a (Suc b) = add a (mult a b)

-- a^(b + 1) = a * (a^b)
pow a Zero = Suc Zero
pow a (Suc b) = mult a (pow a b)

-- (a + 1) = (b + 1) iff a = b
eq Zero Zero = True
eq (Suc a) (Suc b) = eq a b
eq _ _ = False

lt a Zero = False
lt Zero (Suc b) = True
lt (Suc a) (Suc b) = lt a b

gt Zero b = False
gt (Suc a) Zero = True
gt (Suc a) (Suc b) = gt a b

one = Suc Zero
two = Suc one
three = Suc two
four = Suc three
five = Suc four
