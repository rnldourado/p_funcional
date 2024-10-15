-- Questão 4)

type Queue a = [a]

filaVazia = []

taVazia [] = True
taVazia _ = False

enfileirar x fila = fila ++ [x]

desenfileirar [] = error "fila vazia"
desenfileirar (x:xs) = (x, xs)

primeiro [] = error "fila vazia"
primeiro (x:_) = x

-- testes
run_testes = do
    -- deve criar um fila com os valores enfileirados corretamente
    let fila = (enfileirar 3 ( enfileirar 2 ( enfileirar 1 filaVazia)))
    assertEqual [1,2,3] fila
    
    -- deve desenfileirar um valor da fila
    let (frente, fila1) = desenfileirar fila
    assertEqual 1 frente
    assertEqual [2,3] fila1 
    
    -- deve exibir o priomeiro da fila
    assertEqual 2 (primeiro fila1)
    assertEqual False (taVazia fila1) 
    
    -- deve exibir o valor desenfileirado e verificar se a fila está vazia
    let (frente2, fila2) = desenfileirar fila1
    assertEqual 2 frente2
    assertEqual False (taVazia fila2)


assertEqual esperado atual =
    if esperado == atual
    then putStrLn $ "[SUCESSO] "
    else putStrLn $ "[FALHA] " ++ " - Esperado: " ++ show esperado ++ ", Obtido: " ++ show atual