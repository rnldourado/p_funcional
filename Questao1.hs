-- Questão 1)

sol :: [Int] -> Int -> (Int, Int)
sol [] _ = error "lista vazia"
sol xs x = parComSomaMaisProxima (produtoCartesiano xs) x

produtoCartesiano xs = [(a, b) | a <- xs, b <- xs, a /= b]

parComSomaMaisProxima pares x  
    | pares == [] = error "lista vazia"
    | otherwise = foldr1 escolherPar pares
  where  
    escolherPar par1 par2 =  
        if abs (somapar par1 - x) < abs (somapar par2 - x)  
        then par1  
        else par2  

    somapar (a, b) = a + b 

-- testes
run_testes = do
    -- 30 and 22
    let arr = [10, 22, 28, 29, 30, 40]
    let value = 54
    assertEqual (show (30,22)) (show (sol arr value))

    -- 10 and 4
    let arr = [1, 3, 4, 7, 10]
    let value = 15
    assertEqual (show (10,4)) (show (sol arr value))

    -- 128 and 1
    let arr = [1, 2, 4, 8, 16, 32 ,64 , 128]
    let value = 128
    assertEqual (show (128,1)) (show (sol arr value))

    let arr = [0..500]
    let value = 1000
    assertEqual (show (500,499)) (show (sol arr value))

    let arr = [0..500]
    let value = 0
    assertEqual (show (1,0)) (show (sol arr value))
    
    let arr = [-500..500]
    let value = -999
    assertEqual (show (-499,-500)) (show (sol arr value))

assertEqual esperado atual =
    if esperado == atual
    then putStrLn $ "[SUCESSO] "
    else putStrLn $ "[FALHA] " ++ " - Esperado: " ++ show esperado ++ ", Obtido: " ++ show atual