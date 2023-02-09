{- |
Module      : Tarefa2_2022li1g011
Description : Geração contínua de um mapa
Copyright   : Pedro Seabra Vieira <a104352@alunos.uminho.pt>
              Pedro Filipe Maneta Pinto <a104176@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g011_Gloss where

import LI12223
import System.Random
import System.IO.Unsafe  


{-| 
A função 'estendeMapa' vai gerar e adicionar uma nova linha válida ao topo de um dado mapa válido. Ou seja está função vai receber um mapa válido e um inteiro (seed) que é um inteiro aleatório (no intervalo [0,100]) que foi usado para acrescentar alguma pseudo-aleatoriedade à geração da nova linha.

Para esta função foi utilizada várias outras funçoes auxiliares.

A função ainda utiliza uma seed, que é uma valor inteiro que o gerador vai se basear nele para gerar novos números/obstáculos/terrenos.

A função pode ser definida da seguinte forma:

@
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa mapa@(Mapa l ((t,(o:os)):r)) seed | 0 <= seed && seed <= 100 = (Mapa l (completarLinha l (escolherTerrenoRandom mapa seed) seed))
                                              | otherwise = error "seed tem que ser entra 0 e 100"
@

== Exemplos de utilização
=== Exemplo 1
>>> estendeMapa (Mapa 4 [(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])]) 12
Mapa 4 [(Rio (-1),[Nenhum,Nenhum,Nenhum,Tronco]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])]

-} 

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa mapa@(Mapa l ((t,(o:os)):r)) seed | 0 <= seed && seed <= 100 = (Mapa l (completarLinha l (escolherTerrenoRandom mapa seed) seed))
                                              | otherwise = error "seed tem que ser entra 0 e 100"


{-| 
A função 'velocidadeRandomAux' gera uma velocidade random diferente de 0.

A função pode ser definida da seguinte forma:

@
velocidadeRandomAux :: Int -> Int
velocidadeRandomAux l | x == 0 = velocidadeRandomAux l
                      | otherwise = x
                        where x = unsafePerformIO (getStdRandom (randomR (-l, l)))
@

== Exemplos de utilização
=== Exemplo 1
>>> velocidadeRandomAux 2
-2

=== Exemplo 2
>>> velocidadeRandomAux 2
1

-} 
velocidadeRandomAux :: Int -> Int
velocidadeRandomAux l | x == 0 = velocidadeRandomAux l
                      | otherwise = x
                        where x = unsafePerformIO (getStdRandom (randomR (-l, l)))


{-| 
A função 'velocidadeRandomRioPositivo' gera uma velocidade positiva random diferente de 0.

A função pode ser definida da seguinte forma:

@
velocidadeRandomAux :: Int -> Int
velocidadeRandomAux l | x == 0 = velocidadeRandomAux l
                      | otherwise = x
                        where x = unsafePerformIO (getStdRandom (randomR (-l, l)))
@

== Exemplos de utilização
=== Exemplo 1
>>> velocidadeRandomRioPositivo 2
2

=== Exemplo 2
>>> velocidadeRandomRioPositivo 2
1

-} 
velocidadeRandomRioPositivo :: Int -> Int
velocidadeRandomRioPositivo l | x == 0 = velocidadeRandomRioPositivo l
                              | otherwise = x
                               where x = unsafePerformIO (getStdRandom (randomR (0, l)))


{-| 
A função 'velocidadeRandomRioNegativo' gera uma velocidade negativa random diferente de 0.

A função pode ser definida da seguinte forma:

@
velocidadeRandomRioNegativo :: Int -> Int
velocidadeRandomRioNegativo l | x == 0 = velocidadeRandomRioNegativo l
                              | otherwise = x
                               where x = unsafePerformIO (getStdRandom (randomR (-l, 0)))
@

== Exemplos de utilização
=== Exemplo 1
>>> velocidadeRandomRioNegativo 2
-2

=== Exemplo 2
>>> velocidadeRandomRioPositivo 2
-1

-} 
velocidadeRandomRioNegativo :: Int -> Int
velocidadeRandomRioNegativo l | x == 0 = velocidadeRandomRioNegativo l
                              | otherwise = x
                               where x = unsafePerformIO (getStdRandom (randomR (-l, 0)))


{-| 
A função 'velocidadeRandom' função auxiliar para função principal que recebe um inteiro (largura) e recebe a uma lista de linhas (ou seja, lista de pares de terrenos e conjunto de obstáculos), está função vai gerar um velocidade random ao terreno quando o mesmo é um rio ou estrada com velocidade igual a 0.

Nesta função é utilizada as funções auxiliares: 'velocidadeRandomAux', 'velocidadeRandomRioNegativo' e 'velocidadeRandomRioPositivo'.

A função pode ser definida da seguinte forma:

@
velocidadeRandom :: Int -> [(Terreno,[Obstaculo])] -> Terreno
velocidadeRandom l ((Rio 0,(o:os)):(Rio v,(o1:os1)):r) | v > 0 = Rio (velocidadeRandomRioNegativo l)
                                                        | otherwise = Rio (velocidadeRandomRioPositivo l)
velocidadeRandom l ((Estrada 0,(o:os)):r) = Estrada (velocidadeRandomAux l)
velocidadeRandom l ((Rio 0,(o:os)):r) = Rio (velocidadeRandomAux l)
velocidadeRandom l ((t,(o:os)):r) = t

@

== Exemplos de utilização
=== Exemplo 1
>>> velocidadeRandom 3 [(Relva, [Nenhum, Nenhum, Nenhum]),(Rio 1, [Nenhum, Tronco, Tronco]),(Relva, [Nenhum, Nenhum, Nenhum])]
Relva

=== Exemplo 2
>>> velocidadeRandom 3 [(Rio 0, [Nenhum, Tronco, Tronco]),(Relva, [Nenhum, Nenhum, Nenhum])]
Rio -1

-} 
velocidadeRandom :: Int -> [(Terreno,[Obstaculo])] -> Terreno
velocidadeRandom l ((Rio 0,_):(Rio v,_):r) | v > 0 = Rio (velocidadeRandomRioNegativo l)
                                                       | otherwise = Rio (velocidadeRandomRioPositivo l)
velocidadeRandom l ((Estrada 0,_):r) = Estrada (velocidadeRandomAux l)
velocidadeRandom l ((Rio 0,_):r) = Rio (velocidadeRandomAux l)
velocidadeRandom _ ((t,(_)):r) = t


{-| 
A função 'completarLinha' recebe a largura do mapa, uma lista de pares de terrenos e lista de obstáculos que foi gerada com o uso de funções auxiliares e um inteiro (seed)



A função pode ser definida da seguinte forma:

@
completarLinha :: Int -> [(Terreno,[Obstaculo])] -> Int -> [(Terreno,[Obstaculo])]
completarLinha l ((t,(o:os)):r) seed | l > length (o:os) + 1 = completarLinha l ([(terrenoV,(o:os)++[proximosObstaculosValidos l (t,(o:os)) !! (escolherNumero $ terrenoRandom (proximosObstaculosValidos l (t, (o:os))) seed)])]++r) seed
                                     | otherwise = [(terrenoV,(o:os)++[proximosObstaculosValidos l (t, (o:os)) !! (escolherNumero $ terrenoRandom (proximosObstaculosValidos l (t, (o:os))) seed)])] ++ r
                                     where terrenoV = velocidadeRandom l ((t,(o:os)):r)
@

== Exemplos de utilização
=== Exemplo 1
>>> completarLinha 3 [(Rio 0,[Nenhum]),(Estrada 0,[Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])] 12
[(Rio 1,[Nenhum,Nenhum,Tronco]),(Estrada 0,[Carro,Nenhum,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum])]

-} 
completarLinha :: Int -> [(Terreno,[Obstaculo])] -> Int -> [(Terreno,[Obstaculo])]
completarLinha l ((t,(o:os)):r) seed | l > length (o:os) + 1 = completarLinha l ([(terrenoV,(o:os)++proximosTerrenos)]++r) seed
                                     | otherwise = [(terrenoV,(o:os)++proximosTerrenos)] ++ r
                                     where terrenoV = velocidadeRandom l ((t,(o:os)):r)
                                           proximosTerrenos = [proximoTerreno!! (escolherNumero $ terrenoRandom (proximoTerreno) seed)]
                                           proximoTerreno = proximosObstaculosValidos l (t,(o:os))

{-| 
A função 'escolherTerrenoRandom' escolhe um terreno random.

A função pode ser definida da seguinte forma:

@
escolherTerrenoRandom :: Mapa -> Int -> [(Terreno,[Obstaculo])]
escolherTerrenoRandom mapa@(Mapa l lista@((t,(o:os)):r)) seed = [(novoTerreno,[proximosObstaculosValidos l (novoTerreno, []) !! (escolherNumero $ terrenoRandom (proximosObstaculosValidos l (novoTerreno, [])) seed)])] ++ lista
                                                              where novoTerreno = ((proximosTerrenosValidos mapa) !! (escolherNumero $ terrenoRandom (proximosTerrenosValidos mapa) seed))           
@

== Exemplos de utilização
=== Exemplo 1
>>> escolherTerrenoRandom (Mapa 4 [(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])]) 12
[(Rio 0,[Nenhum]),(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])]

-} 
escolherTerrenoRandom :: Mapa -> Int -> [(Terreno,[Obstaculo])]
escolherTerrenoRandom mapa@(Mapa l lista@((t,(o:os)):r)) seed = [(terrenoComVel,obstaculosF)] ++ lista
                                                            where terrenoF = ((proximosTerrenosValidos mapa) !! (escolherNumero $ terrenoRandom (proximosTerrenosValidos mapa) seed))
                                                                  obstaculosF = [proximosObstaculosValidos l (terrenoComVel, []) !! (escolherNumero $ terrenoRandom (proximosObstaculosValidos l (terrenoComVel, [])) seed)]
                                                                  terrenoComVel = velocidadeRandom l ([(terrenoF,[])]++lista)
{-| 
A função 'escolherRandom' usa as funções auxiliares /escolherNumero/ e /terrenoRandom/ para escolher um elemento da lista à sorte, para esta função funcionar está precisa de receber uma lista e um inteiro (seed).

A função pode ser definida da seguinte forma:

@
escolherRandom :: [a] -> Int -> a
escolherRandom (t:ts) seed = (t:ts) !! (escolherNumero $ terrenoRandom (t:ts) seed)
@

== Exemplos de utilização
=== Exemplo 1 
>>>  escolherRandom [Tronco, Nenhum] 12
Tronco

=== Exemplo 2 
>>> escolherRandom [Tronco, Nenhum] 1
Nenhum

-} 
escolherRandom :: [a] -> Int -> a
escolherRandom (t:ts) seed = (t:ts) !! (escolherNumero $ terrenoRandom (t:ts) seed)

{-| 
A função 'terrenoRandom' é uma função que recebe uma lista e um inteiro, a seed, está função vai gerar uma lista de intieros aleatórios, que depois vai ser escolhido a posição do maior número e assim temos um valor random para pegar num terreno ou obstaculos da lista dos possíveis aleatoriamente. 


A função pode ser definida da seguinte forma:

@
terrenoRandom :: [a] -> Int -> [Int]     
terrenoRandom (t:ts) seed = take(length(t:ts)) $ randoms (mkStdGen seed) 
@

== Exemplos de utilização
=== Exemplo 1 
>>> terrenoRandom [(Rio 0),(Estrada 0), (Relva)] 12
[5383096891209391546,3209554951090738563,-6127366005419924004]

=== Exemplo 2
>>> terrenoRandom [Tronco,Nenhum] 12
[5383096891209391546,3209554951090738563]

-} 
terrenoRandom :: [a] -> Int -> [Int]     
terrenoRandom (t:ts) seed = take(length(t:ts)) $ randoms (mkStdGen seed) 

{-| 
A função 'escolherNumero' tem como objetivo dar-nos a posição do maior número da lista, recebendo uma lista de inteiros e devolvendo a posição do maior inteiro da lista.


A função pode ser definida da seguinte forma:

@
escolherNumero :: [Int] -> Int
escolherNumero (h:t) = escolherNumeroAux 0 0 h t
             where escolherNumeroAux i im _ [] = im
                   escolherNumeroAux i im x (y:ys) | x < y = escolherNumeroAux (i+1) (i+1) y ys
                                                   | otherwise = escolherNumeroAux (i+1) im x ys
@

== Exemplos de utilização
=== Exemplo 1 
>>> escolherNumero [5383096891209391546,3209554951090738563,-6127366005419924004]
0

=== Exemplo 2
>>> escolherNumero  [9,7,3,2,1]
0

-} 
escolherNumero :: [Int] -> Int
escolherNumero (h:t) = unsafePerformIO (getStdRandom (randomR (0, (length (h:t)-1))))
{-escolherNumero (h:t) = escolherNumeroAux 0 0 h t
            where escolherNumeroAux i im _ [] = im
                  escolherNumeroAux i im x (y:ys) | x < y = escolherNumeroAux (i+1) (i+1) y ys
                                                  | otherwise = escolherNumeroAux (i+1) im x ys
-}



{-| 
A função 'proximosTerrenosValidos' que recebe um Mapa e uma lista de terrenos possíveis para a próxima linha, para que o mapa seja válido.


A função pode ser definida da seguinte forma:

@
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l ((Rio x,(o1:os1)):(Rio y,(o2:os2)):(Rio z,(o3:os3)):r))| r == [] = [Estrada 0, Relva]
                                                                                       | otherwise = proximosTerrenosValidos (Mapa l ((Rio y,(o2:os2)):(Rio z,(o3:os3)):r))
proximosTerrenosValidos (Mapa l ((Estrada x,(o1:os1)):(Estrada y,(o2:os2)):(Estrada z,(o3:os3)):(Estrada w,(o4:os4)):r)) | r == [] = [Rio 0,Relva]
                                                                                                                         | otherwise = proximosTerrenosValidos (Mapa l ((Estrada y,(o2:os2)):(Estrada z,(o3:os3)):(Estrada w,(o4:os4)):r))
proximosTerrenosValidos (Mapa l ((Relva,(o1:os1)):(Relva,(o2:os2)):(Relva,(o3:os3)):(Relva,(o4:os4)):r)) | r == [] = [Rio 0, Estrada 0]
                                                                                                         | otherwise = proximosTerrenosValidos (Mapa l ((Relva,(o2:os2)):(Relva,(o3:os3)):(Relva,(o4:os4)):r))
proximosTerrenosValidos (Mapa l ((_,(o:os)):r)) = proximosTerrenosValidos (Mapa l r)
proximosTerrenosValidos (Mapa l []) = [Rio 0, Estrada 0, Relva]
@

== Exemplos de utilização
=== Exemplo 1
>>> proximosTerrenosValidos (Mapa 3 [(Rio 2, [Nenhum,Tronco,Nenhum]),(Rio (-2), [Nenhum,Tronco,Nenhum]),(Rio 3, [Tronco,Nenhum,Tronco])])
[Estrada 0,Relva]

=== Exemplo 2
>>> proximosTerrenosValidos (Mapa 3 [(Rio 2, [Nenhum,Tronco,Nenhum]),(Estrada 3, [Nenhum,Carro,Nenhum]),(Rio 1, [Tronco,Nenhum,Tronco])])
[Rio 0,Estrada 0,Relva]


-} 
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l ((Rio x,(o1:os1)):(Rio y,(o2:os2)):(Rio z,(o3:os3)):r))| r == [] = [Estrada 0, Relva]
                                                                                       | otherwise = proximosTerrenosValidos (Mapa l ((Rio y,(o2:os2)):(Rio z,(o3:os3)):r))
proximosTerrenosValidos (Mapa l ((Estrada x,(o1:os1)):(Estrada y,(o2:os2)):(Estrada z,(o3:os3)):(Estrada w,(o4:os4)):r)) | r == [] = [Rio 0,Relva]
                                                                                                                         | otherwise = proximosTerrenosValidos (Mapa l ((Estrada y,(o2:os2)):(Estrada z,(o3:os3)):(Estrada w,(o4:os4)):r))
proximosTerrenosValidos (Mapa l ((Relva,(o1:os1)):(Relva,(o2:os2)):(Relva,(o3:os3)):(Relva,(o4:os4)):r)) | r == [] = [Rio 0, Estrada 0]
                                                                                                         | otherwise = proximosTerrenosValidos (Mapa l ((Relva,(o2:os2)):(Relva,(o3:os3)):(Relva,(o4:os4)):r))
proximosTerrenosValidos (Mapa l ((_,(o:os)):r)) = proximosTerrenosValidos (Mapa l r)
proximosTerrenosValidos (Mapa l []) = [Rio 0, Estrada 0, Relva]



{-| 
A função 'proximosObstaculosValidos' que recebe um inteiro (a largura do mapa) e um par de um terreno e um conjunto de obstáculos e retorna uma lista de obstáculos possíveis para o próximo obstáculos dessa linha, para a linha válida.

Para está função funcionar corretamente, foi utilizada uma função auxiliar /ObstaculosValidosAux/. 

A função pode ser definida da seguinte forma:

@
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos l (Estrada x, []) =  [Nenhum, Carro]
proximosObstaculosValidos l (Estrada x, (o:os)) | l == length (o:os) = []
                                                | Carro == head(head(obstaculosValidosAux(o:os))) && Carro == head(last(obstaculosValidosAux(o:os))) && (length(last(obstaculosValidosAux(o:os))))+length(head(obstaculosValidosAux (o:os))) >= 3 = [Nenhum]
                                                | Carro == head(head(obstaculosValidosAux(o:os))) && Carro == head(last(obstaculosValidosAux(o:os))) && (length(last(obstaculosValidosAux(o:os))))+length(head(obstaculosValidosAux (o:os))) >= 3 && (l - length(o:os)) > 1 = [Nenhum, Carro]
                                                | not(elem Nenhum (o:os)) && (l - (length (o:os))) > 1  = [Nenhum, Carro] -- lista não completa da nenhum ou carro
                                                | not(elem Nenhum (o:os)) && (length (o:os)) < l = [Nenhum] -- tem de 1 ter Nenhum pelo menos
                                                | not(elem Nenhum (drop (length (o:os) - 5) (o:os))) && l == 1 + length(o:os) = [Nenhum] -- (aqui tmb pode receber carro se for nenhum e deopis carro) 
                                                | not(elem Nenhum (o:os)) = [Nenhum, Carro]
                                                | otherwise = [Nenhum, Carro]

proximosObstaculosValidos l (Rio x, []) =  [Nenhum, Tronco]
proximosObstaculosValidos l (Rio x, (o:os)) | l == length (o:os) = []
                                            | Tronco == head(head(obstaculosValidosAux(o:os))) && Tronco == head(last(obstaculosValidosAux(o:os))) && (length(last(obstaculosValidosAux(o:os))))+length(head(obstaculosValidosAux (o:os))) >= 5 = [Nenhum]
                                            | Tronco == head(head(obstaculosValidosAux(o:os))) && Tronco == head(last(obstaculosValidosAux(o:os))) && (length(last(obstaculosValidosAux(o:os))))+length(head(obstaculosValidosAux (o:os))) >= 5 && (l - length(o:os)) > 1  = [Nenhum, Tronco]
                                            | not(elem Nenhum (o:os)) && (l - (length (o:os))) > 1  = [Nenhum, Tronco] -- lista não completa da nenhum ou tronco
                                            | not(elem Nenhum (o:os)) && (length (o:os)) < l = [Nenhum] -- tem de 1 ter Nenhum pelo menos
                                            | not(elem Tronco (o:os)) && (l - length (o:os)) == 1 = [Tronco]
                                            | not(elem Nenhum (drop (length (o:os) - 5) (o:os))) && l == 1 + length(o:os) = [Nenhum] 
                                            | not(elem Nenhum (o:os)) = [Nenhum, Tronco]
                                            | otherwise = [Nenhum, Tronco]

proximosObstaculosValidos l (Relva, []) =  [Nenhum, Arvore]
proximosObstaculosValidos l (Relva, (o:os)) | l == length (o:os) = []
                                            | not(elem Nenhum (o:os)) && (l - (length (o:os))) > 1  = [Nenhum, Arvore] -- lista não completa da nenhum ou Arvore
                                            | not(elem Nenhum (o:os)) && (length (o:os)) < l = [Nenhum] -- tem de 1 ter Nenhum pelo menos
                                            | not(elem Nenhum (drop (length (o:os) - 5) (o:os))) && l == 1 + length(o:os) = [Nenhum] -- aqui tmb pode receber carro se for nenhum e deopis carro 
                                            | not(elem Nenhum (o:os)) = [Nenhum, Arvore]
                                            | otherwise = [Nenhum, Arvore]
@

== Exemplos de utilização
=== Exemplo 1 
>>> proximosObstaculosValidos 3 (Rio 1, [Nenhum, Nenhum])
[Tronco]

=== Exemplo 2 
>>> proximosObstaculosValidos 3 (Rio (-1), [Nenhum, Tronco])
[Nenhum,Tronco]

=== Exemplo 3
>>> proximosObstaculosValidos 5 (Estrada 2, [Nenhum, Carro, Carro])
[Nenhum,Carro]

-} 
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos l (Estrada x, []) | x < 0 = [NenhumC, Carro]
                                            | otherwise = [NenhumC, CarroVP]
proximosObstaculosValidos l (Estrada x, (o:os)) | l == length (o:os) = []
                                                | NenhumC == head(last(obstaculosValidosAux(o:os))) && (l - length(o:os)) > 1 && x < 0 = [NenhumC, Carro]
                                                | NenhumC == head(last(obstaculosValidosAux(o:os))) && (l - length(o:os)) > 1 && x > 0 = [NenhumC, CarroVP]
                                                | NenhumC == head(last(obstaculosValidosAux(o:os))) && (l - length(o:os)) == 1 = [NenhumC]
                                                | (Carro == head(last(obstaculosValidosAux(o:os))) || CarroVP == head(last(obstaculosValidosAux(o:os)))) && length(last(obstaculosValidosAux(o:os))) >= 3 = [NenhumC]
                                                | ((Carro == head(head(obstaculosValidosAux(o:os))) && Carro == head(last(obstaculosValidosAux(o:os)))) || (CarroVP == head(head(obstaculosValidosAux(o:os))) && CarroVP == head(last(obstaculosValidosAux(o:os))))) && (length(last(obstaculosValidosAux(o:os))))+length(head(obstaculosValidosAux (o:os))) >= 3 = [NenhumC]
                                                | not(elem NenhumC (o:os)) && (l - (length (o:os))) > 1 && x < 0 = [NenhumC, Carro] -- lista não completa da nenhum ou carro
                                                | not(elem NenhumC (o:os)) && (l - (length (o:os))) > 1 && x > 0   = [NenhumC, CarroVP] -- lista não completa da nenhum ou carro
                                                | not(elem NenhumC (o:os)) && (length (o:os)) < l = [NenhumC] -- tem de 1 ter Nenhum pelo menos
                                                | not(elem NenhumC (drop (length (o:os) - 3) (o:os))) && l == 1 + length(o:os) = [NenhumC] -- (aqui tmb pode receber carro se for nenhum e deopis carro) 
                                                | not(elem NenhumC (o:os)) && x < 0 = [NenhumC, Carro]
                                                | not(elem NenhumC (o:os)) && x > 0 = [NenhumC, CarroVP]
                                                | x > 0 = [NenhumC, CarroVP]
                                                | otherwise = [NenhumC, Carro]
                                                
proximosObstaculosValidos l (Rio x, []) =  [NenhumT, Tronco]
proximosObstaculosValidos l (Rio x, (o:os)) | l == length (o:os) = []
                                            | Tronco == head(head(obstaculosValidosAux(o:os))) && Tronco == head(last(obstaculosValidosAux(o:os))) && (length(last(obstaculosValidosAux(o:os))))+length(head(obstaculosValidosAux (o:os))) >= 5 = [NenhumT]
                                            | Tronco == head(head(obstaculosValidosAux(o:os))) && Tronco == head(last(obstaculosValidosAux(o:os))) && (length(last(obstaculosValidosAux(o:os))))+length(head(obstaculosValidosAux (o:os))) >= 5 && (l - length(o:os)) > 1  = [NenhumT, Tronco]
                                            | not(elem NenhumT (o:os)) && (l - (length (o:os))) > 1  = [NenhumT, Tronco] -- lista não completa da NenhumT ou tronco
                                            | not(elem NenhumT (o:os)) && (length (o:os)) < l = [NenhumT] -- tem de 1 ter NenhumT pelo menos
                                            | not(elem Tronco (o:os)) && (l - length (o:os)) == 1 = [Tronco]
                                            | not(elem NenhumT (drop (length (o:os) - 5) (o:os))) && l == 1 + length(o:os) = [NenhumT] 
                                            | not(elem NenhumT (o:os)) = [NenhumT, Tronco]
                                            | otherwise = [NenhumT, Tronco]

proximosObstaculosValidos l (Relva, []) =  [Nenhum, Arvore, Pedra]
proximosObstaculosValidos l (Relva, (o:os)) | l == length (o:os) = []
                                            | not(elem Nenhum (o:os)) && (l - (length (o:os))) > 1  = [Nenhum, Arvore, Pedra] -- lista não completa da nenhum ou Arvore
                                            | not(elem Nenhum (o:os)) && (length (o:os)) < l = [Nenhum] -- tem de 1 ter Nenhum pelo menos
                                            | not(elem Nenhum (drop (length (o:os) - 5) (o:os))) && l == 1 + length(o:os) = [Nenhum] -- aqui tmb pode receber carro se for nenhum e deopis carro 
                                            | not(elem Nenhum (o:os)) = [Nenhum, Arvore, Pedra]
                                            | otherwise = [Nenhum, Arvore, Pedra]


{-| 
A função 'obstaculosValidosAux' é uma função auxiliar da função proximosObstaculosValidos, está funçao tem com objetivo agrupar elementos iguais e consecutivos de uma lista, recebendo uma lista de obstáculos e devolvendo uma lista de obstáculos organizados.


A função pode ser definida da seguinte forma:

@
obstaculosValidosAux :: [Obstaculo] -> [[Obstaculo]]
obstaculosValidosAux [] = []
obstaculosValidosAux [x] = [[x]]
obstaculosValidosAux l = u : obstaculosValidosAux (drop (length u) l)
    where u = uma_lista l
          uma_lista :: Eq a => [a] -> [a] 
          uma_lista [x] = [x]
          uma_lista (x:y:t) | x == y = x : uma_lista (y:t)
                            | otherwise = [x] 
@

== Exemplos de utilização
=== Exemplo 1 
>>> obstaculosValidosAux [Tronco,Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum]
[[Tronco,Tronco],[Nenhum],[Tronco],[Nenhum,Nenhum,Nenhum]]

-} 
obstaculosValidosAux :: [Obstaculo] -> [[Obstaculo]]
obstaculosValidosAux [] = []
obstaculosValidosAux [x] = [[x]]
obstaculosValidosAux l = u : obstaculosValidosAux (drop (length u) l)
    where u = uma_lista l
          uma_lista :: Eq a => [a] -> [a]
          uma_lista [x] = [x]
          uma_lista (x:y:t) | x == y = x : uma_lista (y:t)
                            | otherwise = [x] 