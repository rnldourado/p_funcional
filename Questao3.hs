-- Questão 3)

type Pilha a = [a]

novaPilha = []

empilhar x xs = x : xs

retirar [] = error "pilha vazia"
retirar (x:xs) = (x, xs)

topo [] = error "pilha vazia"
topo (x:xs) = x

taVazia [] = True
taVazia _  = False

-- testes 
run_testes = do
    -- deve criar uma pilha com os valores empilhados corretamente
    let pilha = (empilhar 3 ( empilhar 2 ( empilhar 1 novaPilha)))
    assertEqual (show ([3,2,1])) (show (pilha))
    
    -- deve retirar o valor do topo da pilha 
    let (topo1, pilha1) = retirar pilha
    assertEqual (show (3)) (show (topo1))

    -- deve retirar o valor da pilha e verificar se está vazia (falso)
    let (topo2, pilha2) = retirar pilha1
    assertEqual (show (2)) (show (topo2))
    assertEqual (show ([1])) (show (pilha2))
    assertEqual (show (False)) (show(taVazia pilha1))
    
    -- deve retirar o valor da pilha e verificar se está vazia (verdadeiro)
    let (topo3, pilha3) = retirar pilha2
    assertEqual (show (1)) (show (topo3))
    assertEqual "[]" (show (pilha3))
    assertEqual (show (True)) (show (taVazia pilha3))

assertEqual esperado atual =
    if esperado == atual
    then putStrLn $ "[SUCESSO] "
    else putStrLn $ "[FALHA] " ++ " - Esperado: " ++ show esperado ++ ", Obtido: " ++ show atual