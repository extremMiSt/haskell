{-# LANGUAGE InstanceSigs #-}
module Main where
import Test.QuickCheck ((===), quickCheck, oneof, applyFun, Fun)
import qualified Test.QuickCheck.Property
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import qualified Test.QuickCheck as Test.QuickCheck.Gen

data Validation e a = VFail e | VOk a
    deriving (Eq,Show)
data Error e a = EFail e | EOk a
    deriving (Eq,Show)

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation a e) where
    arbitrary :: (Arbitrary a, Arbitrary e) => Test.QuickCheck.Gen.Gen (Validation a e)
    arbitrary = oneof [VFail <$> arbitrary, VOk <$> arbitrary]

instance (Arbitrary a, Arbitrary e) => Arbitrary (Error a e) where
    arbitrary :: (Arbitrary a, Arbitrary e) => Test.QuickCheck.Gen.Gen (Error a e)
    arbitrary = oneof [EFail <$> arbitrary, EOk <$> arbitrary]

instance Functor (Validation a) where 
    fmap :: (a2 -> b) -> Validation a1 a2 -> Validation a1 b
    fmap f (VFail e) = VFail e
    fmap f (VOk a) = VOk (f a)

instance Functor (Error a) where 
    fmap :: (a2 -> b) -> Error a1 a2 -> Error a1 b
    fmap f (EFail e) = EFail e
    fmap f (EOk a) = EOk (f a)

prop_f_id :: (Functor f, Eq (f a)) => f a -> Bool
prop_f_id e = (fmap id e) == (id e)

prop_f_comp :: (Eq (f a), Functor f) => Fun a a -> Fun a a -> f a -> Bool
prop_f_comp g f e = fmap ((applyFun f).(applyFun g)) e == (fmap (applyFun f) . fmap (applyFun g)) e

instance Semigroup a => Applicative (Validation a) where
    pure :: a2 -> Validation a1 a2
    pure = VOk
    (<*>) :: Semigroup a1 => Validation a1 (a2 -> b) -> Validation a1 a2 -> Validation a1 b
    VFail a <*> VFail b = VFail (a <> b)
    VFail a <*> _ = VFail a
    _ <*> VFail a = VFail a
    VOk f <*> VOk b = VOk (f b)

instance Applicative (Error a) where
    pure :: a2 -> Error a1 a2
    pure = EOk
    (<*>) :: Error a1 (a2 -> b) -> Error a1 a2 -> Error a1 b
    EFail a <*> EFail b = EFail a
    EFail a <*> _ = EFail a
    _ <*> EFail a = EFail a
    EOk f <*> EOk b = EOk (f b)

prop_a_id :: (Eq (f b), Applicative f) => f b -> Bool
prop_a_id v = (pure id <*> v) == v

prop_a_comp :: (Eq (f b), Applicative f) => Fun a1 b -> Fun a2 a1 -> f a2 -> Bool
prop_a_comp uf vf w = (pure (.) <*> u <*> v <*> w) == (u <*> (v <*> w))
    where 
        u = pure (applyFun uf)
        v = pure (applyFun vf)

prop_a_V_hom :: Fun String String -> String -> Bool
prop_a_V_hom f x = (pure (applyFun f) <*> (pure x::Validation String String)) == (pure ((applyFun f) x))

prop_a_E_hom :: Fun String String -> String -> Bool
prop_a_E_hom f x = (pure (applyFun f) <*> (pure x::Error String String)) == pure (applyFun f x)

prop_a_V_inter :: Fun String String -> String -> Bool
prop_a_V_inter uf y = (u  <*> (pure y::Validation String String)) == (pure ($ y) <*> u)
    where 
        u = pure (applyFun uf)

prop_a_E_inter :: Fun String String -> String -> Bool
prop_a_E_inter uf y = (u  <*> (pure y::Error String String)) == (pure ($ y) <*> u)
    where 
        u = pure (applyFun uf)

prop_ErrorShortCircuits :: String -> Error String String -> Bool
prop_ErrorShortCircuits e m = t == EFail e
    where 
        t = (EFail e <*> m) :: Error String String

instance Semigroup a => Monad (Validation a) where
    (>>=) :: Semigroup a => Validation a a1 -> (a1 -> Validation a b) -> Validation a b
    VFail a >>= f = VFail a
    VOk a >>= f = f a 

instance Monad (Error a) where
    (>>=) :: Error a a1 -> (a1 -> Error a b) -> Error a b
    EFail a >>= f = EFail a
    EOk a >>= f = f a 

prop_m_lid :: (Eq (m b), Monad m) => a -> Fun a (m b) -> Bool
prop_m_lid a kf = (return a >>= k) == k a 
    where
        k = applyFun kf

prop_m_rid :: (Eq (m b), Monad m) => m b -> Bool
prop_m_rid m = (m >>= return) == m

prop_m_assoc :: (Eq (m b), Monad m) => m a1 -> Fun a1 (m a2) -> Fun a2 (m b) -> Bool
prop_m_assoc m kf hf = (m >>= (\x -> k x >>= h)) == ((m >>= k) >>= h)
    where 
        k = applyFun kf
        h = applyFun hf

prop_m_a_match :: (Eq (f b), Monad f) => Fun a b -> f a -> Bool
prop_m_a_match m1f m2= (m1 <*> m2) == (m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2))))
    where 
        m1 = pure (applyFun m1f)

main :: IO ()
main = do 
    putStrLn "Functor Laws"
    quickCheck (prop_f_id::Validation String String -> Bool)
    quickCheck (prop_f_id::Error String String -> Bool)
    quickCheck (prop_f_comp::Fun String String -> Fun String String -> Validation String String -> Bool)
    quickCheck (prop_f_comp::Fun String String -> Fun String String -> Error String String -> Bool)
    putStrLn "Applicative Laws"
    quickCheck (prop_a_id::Validation String String -> Bool)
    quickCheck (prop_a_id::Error String String -> Bool)
    quickCheck (prop_a_comp::Fun String String -> Fun String String -> Validation String String -> Bool)
    quickCheck (prop_a_comp::Fun String String -> Fun String String -> Error String String -> Bool)
    quickCheck prop_a_V_hom
    quickCheck prop_a_E_hom
    quickCheck prop_a_V_inter
    quickCheck prop_a_E_inter
    putStrLn "Short Circuit"
    quickCheck prop_ErrorShortCircuits
    putStrLn "Monad Laws"
    quickCheck (prop_m_lid::String -> Fun String (Validation String String) -> Bool)
    quickCheck (prop_m_lid::String -> Fun String (Error String String) -> Bool)
    quickCheck (prop_m_rid::Validation String String -> Bool)
    quickCheck (prop_m_rid::Error String String -> Bool)
    quickCheck (prop_m_assoc::Validation String String -> Fun String (Validation String String) -> Fun String (Validation String String) -> Bool)
    quickCheck (prop_m_assoc::Error String String -> Fun String (Error String String) -> Fun String (Error String String) -> Bool)
    putStrLn "Monad+Applicative Match"
    quickCheck (prop_m_a_match::Fun String String -> Validation String String -> Bool)
    quickCheck (prop_m_a_match::Fun String String -> Error String String -> Bool)

    putStrLn "done"
