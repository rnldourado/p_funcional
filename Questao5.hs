-- Questao 5)

data Aluno = Aluno {
    matricula :: Int,
    p_nome :: String,
    sobrenome :: String,
    periodo :: String,
    cra :: Float
} deriving Show

addAluno aluno [] = [(cra aluno, [aluno])]
addAluno aluno ((cra_aux, arr):xs)
    | cra_aux == cra aluno = (cra_aux, aluno: arr):xs
    | otherwise = (cra_aux, arr) : addAluno aluno xs

mediaCRAs alunos = media (foldr somaCRA (0, 0) alunos)

media (_, 0) = 0
media (soma, quantidade) = soma / quantidade

somaCRA aluno (soma, quantidade) = (soma + cra aluno, quantidade + 1)


groupByCRA alunos = agrupados [] alunos

agrupados acumulo [] = acumulo
agrupados acumulo (x:xs) = agrupados (addAluno x acumulo) xs


-- testes
run_testes = do  
    let alunos = [ Aluno 121110581 "Maria" "Silva" "2021.1" 3.5, Aluno 121110582 "Carlos" "Souza" "2021.1" 3.5, Aluno 121110583 "Ana" "Pereira" "2021.1" 4.0, Aluno 121110584 "Pedro" "Almeida" "2021.1" 2.8]
    let alunosAgrupados = show ([(3.5,[Aluno {matricula = 121110582, p_nome = "Carlos", sobrenome = "Souza", periodo = "2021.1", cra = 3.5},Aluno {matricula = 121110581, p_nome = "Maria", sobrenome = "Silva", periodo = "2021.1", cra = 3.5}]),(4.0,[Aluno {matricula = 121110583, p_nome = "Ana", sobrenome = "Pereira", periodo = "2021.1", cra = 4.0}]),(2.8,[Aluno {matricula = 121110584, p_nome = "Pedro", sobrenome = "Almeida", periodo = "2021.1", cra = 2.8}])])
    assertEqual alunosAgrupados (show (groupByCRA alunos))

    let alunos = [ ]
    assertEqual 0.0 (mediaCRAs alunos)
    let alunosAgrupados = "[]"
    assertEqual alunosAgrupados (show (groupByCRA alunos))

    let alunos = [ Aluno 121110555 "Joana" "Silva" "2021.1" 7.5, Aluno 121110582 "Manoel" "Farias" "2021.1" 7.5, Aluno 121110583 "Agatha" "Pereira" "2021.1" 8.0, Aluno 121110584 "Pedro" "Almeida" "2021.1" 8.0, Aluno 121110584 "Paula" "" "2021.1" 8.0]
    let alunosAgrupados = show ([(7.5,[Aluno {matricula = 121110582, p_nome = "Manoel", sobrenome = "Souza", periodo = "2021.1", cra = 7.5},Aluno {matricula = 121110555, p_nome = "Joana", sobrenome = "Silva", periodo = "2021.1", cra = 7.5}]),(8.0,[Aluno {matricula = 121110584, p_nome = "Paula", sobrenome = "", periodo = "2021.1", cra = 8.0},Aluno {matricula = 121110584, p_nome = "Pedro", sobrenome = "Almeida", periodo = "2021.1", cra = 8.0},Aluno {matricula = 121110583, p_nome = "Agatha", sobrenome = "Pereira", periodo = "2021.1", cra = 8.0}])])
    assertEqual alunosAgrupados (show (groupByCRA alunos))
    assertEqual 7.8 (mediaCRAs alunos)

assertEqual esperado atual =
    if esperado == atual
    then putStrLn $ "[SUCESSO] "
    else putStrLn $ "[FALHA] " ++ " - Esperado: " ++ show esperado ++ ", Obtido: " ++ show atual