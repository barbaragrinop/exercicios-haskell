-- Gerar as listas

doisA :: [Int] -- [1,11,121,1331,14641,161051,1771561]
doisA = [11 ^ x | x <- [0 .. 6]]

doisB :: [Int] -- [1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39]
doisB = [x | x <- [1 .. 40], mod x 4 /= 0]

doisC :: [String] -- ["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB", "AgBB"]
doisC = [[a] ++ [xs] ++ "BB" | a <- ['A'], xs <- ['a' .. 'g']]

doisD :: [Int] -- [5,8,11,17,20,26,29,32,38,41]
doisD = [3 * n + 2 | n <- [1 .. 13]]

doisE :: [Float] -- [1.0,0.5,0.25,0.125,0.0625,0.03125]
doisE = [1 / (2 ^ x) | x <- [1 .. 5]]

doisF :: [Int] -- [1,10,19,28,37,46,55,64]
doisF = [9 * x + 1 | x <- [0 .. 7]]

doisG :: [Int] -- [2,4,8,10,12,16,18,22,24,28,30]
doisG = [x * 2 | x <- [1 .. 20], x * 2 `notElem` [6, 14, 20, 26]]

doisH :: [Char] -- ['@','A','C','D','E','G','J','L']
doisH = ['@', 'A', 'C', 'D', 'E', 'G', 'J', 'L']

{-
2.2. Crie uma função que verifique se o tamanho de uma String é par	ou	não. Use Bool comoretorno.
-}
tamanhoStringEhPar :: String -> Bool
tamanhoStringEhPar x = even (length x)

-- 2.3
{-
 Escreva	 uma	 função	 que	 receba	 um	 vetor	 de	 Strings	 e
retorne	uma	lista	com	todos	os	elementos	em	ordem	reversa
-}
listaReversa :: [String] -> [String]
listaReversa xs = reverse xs

-- 2.4
{- 	 Escreva	 uma	 função	 que	 receba	 um	 vetor	 de	 Strings	 e
retorne	 uma	lista	 com	 o	 tamanho	 de	 cada	 String.	As	palavras	 de
tamanho	par	devem	ser	excluídas	da	resposta -}
tamanhoStringApenasImpares :: [String] -> [Int]
tamanhoStringApenasImpares xs = [length x | x <- xs, odd (length x)]

-- 2.5
{- Escreva	a	função		head		como	composição	de	duas	outras -}
{-Pegar a primeira letra da primeira String de um vetor-}
composicao :: [String] -> Char
composicao xs = head $ head xs

-- 2.6
{- Faça	uma	função	que	receba	uma	String	e	retorne		True
se	esta	for	um	palíndromo;	caso	contrário,		False	. -}
ehPalindromo :: String -> Bool
ehPalindromo x = reverse x == x

-- 2.7
{-
Faça	 uma	 função	 que	 receba	 um	 inteiro	 e	 retorne	 uma
tupla,	contendo:	o	dobro	deste	número	na	primeira	coordenada,	o
triplo	na	segunda,	o	quádruplo	na	terceira	e	o	quíntuplo	na	quarta.
-}
foo :: Int -> (Int, Int, Int, Int)
foo x = (x * 2, x * 3, x * 4, x * 5)
-- OU
-- foo x = let [a, b, c, d] = [x * y | y <- [2 .. 5]] in (a, b, c, d)
------------------------------------------------------------------------------------------

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
converterCelsius (Fahrenheit c) = 32 + (c * 9 / 5) 
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

partida :: Jogo -> Jogo -> String
partida Pedra Pedra = "Empate"
partida Papel Papel = "Empate"
partida Tesoura Tesoura = "Empate"
partida Tesoura Papel = "Tesoura venceu!"
partida Tesoura Pedra = "Pedra venceu!"
partida Papel Pedra = "Papel venceu!"
partida Papel Tesoura = "Tesoura Venceu!"
partida Pedra Tesoura = "Pedra venceu!"
partida Pedra Papel = "Papel venceu!"
