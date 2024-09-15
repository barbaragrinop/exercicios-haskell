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
doisE = [1 / (2 ^ x) | x <- [0 .. 5]]

doisF :: [Int] -- [1,10,19,28,37,46,55,64]
doisF = [9 * x + 1 | x <- [0 .. 7]]

doisG :: [Int] -- [2,4,8,10,12,16,18,22,24,28,30]
doisG = [x * 2 | x <- [1 .. 20], x * 2 `notElem` [6, 14, 20, 26]]

doisH :: [Char] -- ['@','A','C','D','E','G','J','L']
doisH = [x | x <- '@' : ['A' .. 'L'], x `notElem` ['B', 'F', 'H', 'I', 'K']]

{-
2.2. Crie uma função que verifique se o tamanho de uma String é par ou	não. Use Bool comoretorno.
-}
tamanhoStringEhPar :: String -> Bool
tamanhoStringEhPar x = even (length x)

-- 2.3
{-
 Escreva uma função que receba um vetor de Strings e
retorne	uma	lista	com	todos	os	elementos	em	ordem	reversa
-}
listaReversa :: [String] -> [String]
listaReversa xs = reverse xs

-- 2.4
{-  Escreva uma função que receba um vetor de Strings e
retorne uma	lista com o tamanho de cada String.	As	palavras de
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
Faça uma função que receba um inteiro e retorne uma
tupla,	contendo:	o	dobro	deste	número	na	primeira	coordenada,	o
triplo	na	segunda,	o	quádruplo	na	terceira	e	o	quíntuplo	na	quarta.
-}
foo :: Int -> (Int, Int, Int, Int)
foo x = let [a, b, c, d] = [x * y | y <- [2 .. 5]] in (a, b, c, d)

------------------------------------------------------------------------------------------

-- 3.1
{-
Crie o tipo Pergunta com os values constructors Sim
ou Nao . Faça as funções seguintes, determinando seus tipos
explicitamente.
-}
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
{-
Faça o tipo Temperatura que pode ter valores Celsius, Farenheit ou Kelvin.
-}
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
{-
Implemente uma função que simule o vencedor de uma
partida de pedra, papel e tesoura usando tipos criados. Casos de
empate devem ser considerados em seu tipo.
-}
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

-- 3.4
{-
Faça uma função que retorne uma string, com todas as
vogais maiúsculas e minúsculas eliminadas de uma string passada
por parâmetro usando list compreenshion.
-}
vogais :: String
vogais = "aeiouAEIOU"

removeVogais :: String -> String
removeVogais palavra = [x | x <- palavra, x `notElem` vogais]

-- 3.10
{-
Faça  uma  função  chamada   revNum ,  que  receba  uma
String s e um Int n . Esta deverá retornar as n primeiras letras
em ordem reversa e o restante em sua ordem normal.
-}

revNum :: Int -> String -> String
revNum n s = reverse (take n s) ++ drop n s

-- 3.11
{-
Crie o tipo	de dado Binario que pode ser Zero ou Um. Faça outro tipo de dado
chamado Funcao que pode ser Soma2, Maior, Menor ou Mult2. Implemente a 
função aplicar que recebe uma Funcao e dois Binarios. Seu retorno consiste	
em executar a operação desejada
-}


data Binario = Zero | Um deriving (Show, Eq)

data Funcao = Soma2 | Maior | Menor | Mult2 deriving (Show, Eq)

aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar f x y = case f of
    Soma2  -> soma2 x y
    Maior  -> maior x y
    Menor  -> menor x y
    Mult2  -> mult2 x y

soma2 :: Binario -> Binario -> Binario
soma2 Zero Zero = Zero
soma2 Zero Um   = Um
soma2 Um   Zero = Um
soma2 Um   Um   = Zero

maior :: Binario -> Binario -> Binario
maior Zero Zero = Zero
maior Zero Um   = Um
maior Um   Zero = Um
maior Um   Um   = Um

menor :: Binario -> Binario -> Binario
menor Zero Zero = Zero
menor Zero Um   = Zero
menor Um   Zero = Zero
menor Um   Um   = Um

mult2 :: Binario -> Binario -> Binario
mult2 Zero _ = Zero
mult2 _ Zero = Zero
mult2 Um   Um = Um


-- 3.21
{-
Crie a função maxMoeda que recebe uma lista de moedas
e retorna o valor máximo absoluto (sem conversão alguma) dentre
os campos val desta lista. Exemplo:

Prelude> maxMoeda [Moeda 3 Real, Moeda 7 Dollar, Moeda 1 Euro] 7
Use a função maximum.
-}
data Unidade = Real | Dollar | Euro deriving (Show)

data Moeda = Moeda {valor :: Int, unidade :: Unidade} deriving (Show)

maxMoeda :: [Moeda] -> Int
maxMoeda moedas = maximum [valor moeda | moeda <- moedas]
