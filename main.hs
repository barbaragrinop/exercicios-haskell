doisA :: [Int]
doisA = [11 ^ x | x <- [0 .. 6]]

doisB :: [Int]
doisB = [x | x <- [1 .. 40], mod x 4 /= 0]

doisC :: [String]
doisC = [[a] ++ [xs] ++ "BB" | a <- ['A'], xs <- ['a' .. 'g']]

-- doisD :: [Int] perguntar pro felipe
-- doisD =  [x | x <- [5..41]]

doisE :: [Float]
doisE = [1 / (2 ^ x) | x <- [1 .. 5]]

doisF :: [Int]
doisF = [9 * x + 1 | x <- [0 .. 7]]

-- doisG :: perguntar pro felipe
-- [2,4,8,10,12,16,18,22,24,28,30]
--     6       14       22  26

doisH :: [Char]
doisH = ['@', 'A', 'C', 'D', 'E', 'G', 'J', 'L']

-- 2.2
tamanhoStringEhPar :: String -> Bool
tamanhoStringEhPar x = even (length x)

-- 2.3
listaReversa :: [String] -> [String]
listaReversa xs = reverse xs

-- 2.4
tamanhoStringApenasImpares :: [String] -> [Int]
tamanhoStringApenasImpares xs = [length x | x <- xs, odd (length x)]

-- 2.5
-- boa sorte

-- 2.6
ehPalindromo :: String -> Bool
ehPalindromo x = reverse x == x

-- 2.7
foo :: Int -> (Int, Int, Int, Int)
-- foo x = (x * 2, x * 3, x * 4, x * 5)
foo x = let [a, b, c, d] = [x * y | y <- [2 .. 5]] in (a, b, c, d)

-- os dois fazem a mesma coisa, só coloquei pra ver como fazia com esse "map"

---------------------------------------------- cabo o 2 (falta alguns depois nós ve com o felipe)

-- 3.1
data Pergunta = Sim | Nao deriving (Show)

pergNum :: Pergunta -> Int
pergNum Nao = 0
pergNum Sim = 1

listPergs :: [Pergunta] -> [Int]
listPergs xs = [pergNum x | x <- xs]

and' :: Pergunta -> Pergunta -> Bool
and' Sim Sim = True
and' _ _ = False

or' :: Pergunta -> Pergunta -> Bool
or' Nao Nao = False
or' _ _ = True

not' :: Pergunta -> Bool
not' Sim = False
not' Nao = True

-- 3.2
data Temperatura = Celsius Double | Fahrenheit Double | Kevin Double deriving (Show)

converterCelsius :: Temperatura -> Double
converterCelsius (Celsius c) = c
converterCelsius (Fahrenheit c) = (c * 9 / 5) + 32
converterCelsius (Kevin c) = c + 273.15

converterKevin :: Temperatura -> Double
converterKevin (Kevin k) = k
converterKevin (Fahrenheit k) = ((k - 273.15) * 9 / 5) + 32
converterKevin (Celsius k) = k - 273.15

converterFahrenheit :: Temperatura -> Double
converterFahrenheit (Fahrenheit f) = f
converterFahrenheit (Celsius f) = (f - 32) * 5 / 9
converterFahrenheit (Kevin f) = ((f - 32) * 5 / 9) + 273.15

-- 3.3
data Jogo = Pedra | Papel | Tesoura deriving (Show)

jogoDaVelha :: Jogo -> Jogo -> String
jogoDaVelha Pedra Pedra = "Empate"
jogoDaVelha Papel Papel = "Empate"
jogoDaVelha Tesoura Tesoura = "Empate"

jogoDaVelha Tesoura Papel = "Tesoura venceu!"
jogoDaVelha Tesoura Pedra = "Pedra venceu!"

jogoDaVelha Papel Pedra = "Papel venceu!"
jogoDaVelha Papel Tesoura = "Tesoura Venceu!"

jogoDaVelha Pedra Tesoura = "Pedra venceu!"
jogoDaVelha Pedra Papel = "Papel venceu!" 
