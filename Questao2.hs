-- Questão 2)
floorCeil :: [Int] -> Int -> (Int, Int)
floorCeil xs x = ((findFloor xs x), (findCeil xs x))

findFloor xs x 
    | (filter (< x) xs) == [] = error "não há nenhum valor menor na lista"
    | otherwise = maximum (filter (< x) xs)  

findCeil xs x 
    | (filter (> x) xs) == [] = error "não há nenhum valor maior na lista"
    | otherwise = minimum (filter (> x) xs)  

-- testes
run_testes = do  
    -- Floor: 2 (maior valor <= 5) e Ceil: 7 (menor valor >= 5).
    let array = [1, 3, 5, 7, 9]  
    let x = 6  
    assertEqual (show (5,7)) (show (floorCeil array x)) 

    -- Floor: 2 (maior valor < 5) e Ceil: 7 (menor valor > 5).
    let array = [10, 2, 7, 15, 22, 26, 30, 35, 40, 50, 60, 70]  
    let x = 5 
    assertEqual (show (2,7)) (show (floorCeil array x)) 

    -- Floor: 22 (maior valor < 24) e Ceil: 26 (menor valor > 24).
    let x = 24
    assertEqual (show (22,26)) (show (floorCeil array x))

    -- Floor: 98 (maior valor < 99) e Ceil: 100 (menor valor > 99).
    let array = [1..100]
    let x = 99
    assertEqual (show (98,100)) (show (floorCeil array x))

    -- Floor: 98 (maior valor < 99) e Ceil: 100 (menor valor > 99).
    let array = [-100..100]
    let x = 0
    assertEqual (show (-1,1)) (show (floorCeil array x))

assertEqual esperado atual =
    if esperado == atual
    then putStrLn $ "[SUCESSO] "
    else putStrLn $ "[FALHA] " ++ " - Esperado: " ++ show esperado ++ ", Obtido: " ++ show atual