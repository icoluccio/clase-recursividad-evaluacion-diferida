---- Recursividad
-- Caso base
factorial 0 = 1
-- Expresión recursiva
factorial n = n * factorial (n - 1)

-- "asdf" -> ["asdf", "asd", "as", "a"]
prefijos [] = []
prefijos lista = prefijos (init lista) ++ [lista]

-- "asdf" -> ["f", "df", "sdf", "asdf"]
sufijos [] = []
sufijos lista = sufijos (tail lista) ++ [lista]

-- Reimplementación del any
any' condicion [elemento] = condicion elemento
any' condicion (x:xs) = condicion x || any' condicion xs

-- Reimplementación del filter
filter' condicion [] = []
filter' condicion (x:xs) | condicion x = x:(filter' condicion xs)
                         | otherwise = filter' condicion xs

-- Reimplementación del map
map' transformacion [] = []
map' transformacion (x:xs) = (transformacion x):(map' transformacion xs)

-- Reimplementación del sum
sumatoria [numero] = numero
sumatoria lista = (head lista) + sumatoria (tail lista)

-- Lo mismo pero con multiplicación
producto [numero] = numero
producto lista = (head lista) * producto (tail lista)

-- Reescribiendo
producto' [numero] = numero
producto' lista = (*) (head lista) (producto' (tail lista))

sumatoria' [numero] = numero
sumatoria' lista = (+) (head lista) (sumatoria' (tail lista))

-- Transformando en una función de orden superior el hecho de aplicar en serie una función
-- Igual a foldr1, desde la izquierda y sin valor inicial externo
aplicarEnSerie _ [x] = x
aplicarEnSerie funcion lista = funcion (head lista) (aplicarEnSerie funcion (tail lista))
-- Con lo que podemos reescribir las funciones anteriores como
sumatoria'' = aplicarEnSerie (+)
productoria = aplicarEnSerie (*)
factorial' n = productoria [1..n]
divoria = aplicarEnSerie div -- Funciona como esperamos? O el orden es distinto?

-- Implementaciones oficiales (con variables renombradas)
-- Empezando desde la derecha, con valor inicial
foldr' funcion inicial []     = inicial
foldr' funcion inicial (x:xs) = funcion x (foldr' funcion inicial xs)
--  Empezando desde la izquierda, con valor inicial
foldl' funcion inicial []     = inicial
foldl' funcion inicial (x:xs) = foldl' funcion (funcion inicial x) xs

-- *Main> foldr (-) 10 [1,2,3]
-- (-) 1               (foldr (-) 10 [2,3])
-- (-) 1 ((-) 2        (foldr (-) 10 [3]))
-- (-) 1 ((-) 2 ((-) 3 (foldr (-) 10 [])))
-- (-) 1 ((-) 2 ((-) 3                10))
-- (-) 1 ((-) 2 (-7))
-- (-) 1 9
-- -8

-- Evaluación diferida
funcionLoca numero = [numero] ++ [factorial 55555] ++ [div 2 0]
funcion1 a b = a + b - 4
funcion2 b = funcion1 6 b
funcion3 a b c = funcion1 a (funcion2 b) + c

-- Listas infinitas
replicar n = (n:replicar n)
potencias n = map (\x -> n^x) [0..]

-- Listas por comprensión
-- [salida | generadores, condiciones]
numerosCondicionLoca n = [i | i <- [1..n], mod i 3 == mod i 5]
combinacionesHasta n m = [(i,j) | i <- [1..n], j <- [1..m]]
combinacionesParImparHasta n m = [(i,j) | i <- [1..n], j <- [1..m], even i, odd j]
