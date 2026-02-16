{-
________________________________________________________________________________

Programación Funcional - ICC302
________________________________________________________________________________

Proyecto 1 - Aplicacion de Programacion Funcional en Haskell
________________________________________________________________________________

Evaluador de Expresiones Aritmeticas

@Autores:
  - Randy Alexander Germosén Ureña
  - Hugo Fernando Concepción López

@Profesor:
    - Justin Eladio Bueno Díaz

@Descripción:
  Este programa implementa un evaluador de expresiones aritmeticas utilizando
  tipos algebraicos para modelar las expresiones y funciones puras para su
  evaluacion y maneja errores como la division por cero mediante el tipo Maybe.

@Github: 

________________________________________________________________________________
-}

-- Tipo algebraico para representar expresiones aritmeticas
data Expr
  = Numero Double
  | Suma Expr Expr
  | Resta Expr Expr
  | Multiplicacion Expr Expr
  | Division Expr Expr
  | Negacion Expr
  deriving (Show, Eq)

-- Tipo algebraico para representar el resultado de una operacion
data Resultado
  = Exito Double
  | ErrorDivisionCero
  | ErrorOperacionInvalida
  deriving (Show, Eq)

-- Funcion que evalua una expresion aritmetica
evaluar :: Expr -> Maybe Double
evaluar (Numero n) = Just n
evaluar (Suma e1 e2) = sumarMaybe (evaluar e1) (evaluar e2)
evaluar (Resta e1 e2) = restarMaybe (evaluar e1) (evaluar e2)
evaluar (Multiplicacion e1 e2) = multiplicarMaybe (evaluar e1) (evaluar e2)
evaluar (Division e1 e2) = dividirSeguro (evaluar e1) (evaluar e2)
evaluar (Negacion e) = negarMaybe (evaluar e)

-- Suma dos valores Maybe Double
sumarMaybe :: Maybe Double -> Maybe Double -> Maybe Double
sumarMaybe Nothing _ = Nothing
sumarMaybe _ Nothing = Nothing
sumarMaybe (Just x) (Just y) = Just (x + y)

-- Resta dos valores Maybe Double
restarMaybe :: Maybe Double -> Maybe Double -> Maybe Double
restarMaybe Nothing _ = Nothing
restarMaybe _ Nothing = Nothing
restarMaybe (Just x) (Just y) = Just (x - y)

-- Multiplica dos valores Maybe Double
multiplicarMaybe :: Maybe Double -> Maybe Double -> Maybe Double
multiplicarMaybe Nothing _ = Nothing
multiplicarMaybe _ Nothing = Nothing
multiplicarMaybe (Just x) (Just y) = Just (x * y)

-- Division segura que maneja division por cero
dividirSeguro :: Maybe Double -> Maybe Double -> Maybe Double
dividirSeguro Nothing _ = Nothing
dividirSeguro _ Nothing = Nothing
dividirSeguro _ (Just 0) = Nothing
dividirSeguro (Just x) (Just y) = Just (x / y)

-- Niega un valor Maybe Double
negarMaybe :: Maybe Double -> Maybe Double
negarMaybe Nothing = Nothing
negarMaybe (Just x) = Just (-x)

-- Evalua una lista de expresiones de forma recursiva
evaluarLista :: [Expr] -> [Maybe Double]
evaluarLista [] = []
evaluarLista (x : xs) = evaluar x : evaluarLista xs

-- Suma todos los resultados exitosos de una lista
sumarResultados :: [Maybe Double] -> Maybe Double
sumarResultados [] = Just 0
sumarResultados (Nothing : xs) = sumarResultados xs
sumarResultados (Just x : xs) = sumarMaybe (Just x) (sumarResultados xs)

-- Convierte un Maybe Double a una representacion textual
resultadoToString :: Maybe Double -> String
resultadoToString Nothing = "Error: No se pudo evaluar la expresion"
resultadoToString (Just x) = "Resultado: " ++ show x

-- Convierte una expresion a formato legible
exprToString :: Expr -> String
exprToString (Numero n) = show n
exprToString (Negacion e) = "(-" ++ exprToString e ++ ")"
exprToString (Suma e1 e2) = "(" ++ exprToString e1 ++ " + " ++ exprToString e2 ++ ")"
exprToString (Resta e1 e2) = "(" ++ exprToString e1 ++ " - " ++ exprToString e2 ++ ")"
exprToString (Multiplicacion e1 e2) = "(" ++ exprToString e1 ++ " * " ++ exprToString e2 ++ ")"
exprToString (Division e1 e2) = "(" ++ exprToString e1 ++ " / " ++ exprToString e2 ++ ")"

-- Verifica si una expresion puede producir error
tieneErrorPotencial :: Expr -> Bool
tieneErrorPotencial (Numero _) = False
tieneErrorPotencial (Negacion e) = tieneErrorPotencial e
tieneErrorPotencial (Suma e1 e2) = tieneErrorPotencial e1 || tieneErrorPotencial e2
tieneErrorPotencial (Resta e1 e2) = tieneErrorPotencial e1 || tieneErrorPotencial e2
tieneErrorPotencial (Multiplicacion e1 e2) = tieneErrorPotencial e1 || tieneErrorPotencial e2
tieneErrorPotencial (Division _ (Numero 0)) = True
tieneErrorPotencial (Division e1 e2) = tieneErrorPotencial e1 || tieneErrorPotencial e2

-- Verifica si una expresion es solo un numero
esConstante :: Expr -> Bool
esConstante (Numero _) = True
esConstante _ = False

-- _____________________________________________________________________________
--
-- Funciones Test para verificar la evualuación de multiples expresiones
-- _____________________________________________________________________________

-- expresionTest1: (10 + 5) * 2
-- deberia dar: 30
expresionTest1 :: Expr
expresionTest1 = Multiplicacion (Suma (Numero 10) (Numero 5)) (Numero 2)

-- expresionTest2: 100 / (5 - 5)
-- deberia dar: error al dividir por cero
expresionTest2 :: Expr
expresionTest2 = Division (Numero 100) (Resta (Numero 5) (Numero 5))

-- expresionTest3: ((8 / 2) + 3) * 4
-- deberia dar: 28
expresionTest3 :: Expr
expresionTest3 =
  Multiplicacion
    ( Suma
        (Division (Numero 8) (Numero 2))
        (Numero 3)
    )
    (Numero 4)

-- expresionTest4: -(5 + 3)
-- deberia dar: -8
expresionTest4 :: Expr
expresionTest4 = Negacion (Suma (Numero 5) (Numero 3))

-- expresionTest5: (15 - 3) / (2 + 2) = 3
-- deberia dar: 3
expresionTest5 :: Expr
expresionTest5 =
  Division
    (Resta (Numero 15) (Numero 3))
    (Suma (Numero 2) (Numero 2))

-- _____________________________________________________________________________
--
-- Funciones IO
-- _____________________________________________________________________________

-- Muestra una expresion y su resultado
mostrarEvaluacion :: Expr -> IO ()
mostrarEvaluacion expr = do
  putStrLn $ "Expresion: " ++ exprToString expr
  putStrLn $ resultadoToString (evaluar expr)
  putStrLn ""

-- Construye una expresion basada en la eleccion del usuario
procesarEntradaUsuario :: String -> Double -> Double -> Maybe Expr
procesarEntradaUsuario "1" a b = Just (Suma (Numero a) (Numero b))
procesarEntradaUsuario "2" a b = Just (Resta (Numero a) (Numero b))
procesarEntradaUsuario "3" a b = Just (Multiplicacion (Numero a) (Numero b))
procesarEntradaUsuario "4" a b = Just (Division (Numero a) (Numero b))
procesarEntradaUsuario _ _ _ = Nothing

-- Intenta leer un Double de forma segura
leerDouble :: String -> Maybe Double
leerDouble s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

-- _____________________________________________________________________________
--
-- Main
-- _____________________________________________________________________________

main :: IO ()
main = do
  putStrLn "______________________________________________"
  putStrLn ""
  putStrLn "  Evaluador de Expresiones Aritmeticas"
  putStrLn "  Proyecto 1 - Programacion Funcional"
  putStrLn "______________________________________________"
  putStrLn ""

  putStrLn "=== Tests predefinidos ==="
  putStrLn ""

  mostrarEvaluacion expresionTest1
  mostrarEvaluacion expresionTest2
  mostrarEvaluacion expresionTest3
  mostrarEvaluacion expresionTest4
  mostrarEvaluacion expresionTest5

  putStrLn "=== Evaluacion interactiva ==="
  putStrLn ""
  putStrLn "Ingrese el primer numero:"
  input1 <- getLine

  putStrLn "Ingrese el segundo numero:"
  input2 <- getLine

  putStrLn "Seleccione la operacion:"
  putStrLn "  1. Suma"
  putStrLn "  2. Resta"
  putStrLn "  3. Multiplicacion"
  putStrLn "  4. Division"
  opcion <- getLine

  case (leerDouble input1, leerDouble input2) of
    (Just num1, Just num2) ->
      case procesarEntradaUsuario opcion num1 num2 of
        Just expr -> do
          putStrLn ""
          putStrLn "=== Resultado ==="
          mostrarEvaluacion expr
        Nothing -> putStrLn "Opcion no valida"
    _ -> putStrLn "Error: Ingrese numeros validos"

  putStrLn ""
