import Test.QuickCheck

dollarRate = 0.98546541

-- | convert EUR to USD
usd euros = euros *dollarRate

-- | convert USD to EUR
euro dollars = dollars / dollarRate


prop_EuroUSD x = euro (usd x) ~== x

x ~== y = abs (x-y) < 10e-15 * abs x

price = 79

price'::Double
price' = 79


price'' :: Num a=>a
price'' = 79
