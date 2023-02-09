{- |
Module      : Tarefa3_2022li1g011
Description : Movimentação do personagem e obstáculos
Copyright   : Pedro Seabra Vieira <a104352@alunos.uminho.pt>
              Pedro Filipe Maneta Pinto <a104176@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g011 where

import LI12223
import Data.List
import Data.Maybe


-- * Função principal

{-| 
A função __animaJogo__ que recebe um Jogo e uma jogada que "dá" movimento ao "Jogo" criado pelas funcões auxiliares retornando um Jogo.

A função aplica animação ao Jogo através de 2 funções auxiliares principais, uma que movimenta o mapa e outra que faz moviementar o personagem, na função principal vai juntar as funções auxiliares de modo a criar um tipo de animação ao Jogo.

__O moviemnto do jogador não acontece se e só se o local para onde ele se for mover for ocupado por uma árvore.__

A função pode ser definida da seguinte forma:

@
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((t,(o:os)):r))) (Move d) | d == Cima = jogadorMove (Jogo (Jogador (x,y)) ( (Mapa l ((t,(o:os)):r)))) (Move Cima)
                                                                  | d == Baixo = jogadorMove (Jogo (Jogador (x,y)) ( (Mapa l ((t,(o:os)):r)))) (Move Baixo)
                                                                  | d == Direita = jogadorMove (Jogo (Jogador (x,y)) ( (Mapa l ((t,(o:os)):r)))) (Move Direita)
                                                                  | d == Esquerda = jogadorMove (Jogo (Jogador (x,y)) ( (Mapa l ((t,(o:os)):r)))) (Move Esquerda)
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((t,(o:os)):r))) (Parado) = jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
@

== Exemplos de utilização
=== Exemplo 1 - Movimenta Para Cima
>>> animaJogo (Jogo (Jogador (1,2)) (Mapa 4 ([(Rio 2,[Tronco,Nenhum,Tronco,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Rio 3,[Tronco,Nenhum,Tronco,Tronco])]))) (Move Cima)
Jogo (Jogador (1,1)) (Mapa 4 [(Rio 2,[Tronco,Nenhum,Tronco,Nenhum]),(Estrada (-2),[Nenhum,Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Rio 3,[Nenhum,Tronco,Tronco,Tronco])])

=== Exemplo 2 - Movimenta Para Baixo (Com Uma Árvore em Baixo)
>>> animaJogo (Jogo (Jogador (2,1)) (Mapa 4 ([(Rio (-3),[Tronco,Nenhum,Tronco,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Tronco])]))) (Move Baixo)
Jogo (Jogador (2,1)) (Mapa 4 [(Rio (-3),[Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-2),[Nenhum,Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Rio 1,[Tronco,Tronco,Nenhum,Tronco])])

-} 

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((t,(o:os)):r))) (Move d) | d == Cima = jogadorMove (Jogo (Jogador (x,y)) ( (Mapa l ((t,(o:os)):r)))) (Move Cima)
                                                                  | d == Baixo = jogadorMove (Jogo (Jogador (x,y)) ( (Mapa l ((t,(o:os)):r)))) (Move Baixo)
                                                                  | d == Direita = jogadorMove (Jogo (Jogador (x,y)) ( (Mapa l ((t,(o:os)):r)))) (Move Direita)
                                                                  | d == Esquerda = jogadorMove (Jogo (Jogador (x,y)) ( (Mapa l ((t,(o:os)):r)))) (Move Esquerda)
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((t,(o:os)):r))) (Parado) = jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
-- x = posição jogador na linha, y = linha em que o jogador está, l = largura, t = terreno, o = obstaculo, os = obstaculos, r = restantes linhas, d = direção do Movimento.

-- * Funções auxiliares

{-| 
A função __moverObstaculos__ é uma função auxiliar que requer 2 auxiliares a função __moverObstaculosLinha__ e a função __moverObstaculosVelocidade__, que gera movimentos dos obstaculos no mapa, e.g. Um Rio de velocidade 2 [Tronco,Nenhum,Tronco,Tronco] = [Tronco,Tronco,Tronco,Nenhum] (Se a velocidade for negativa o movimento será feito no sentido contrário.).

Quando v > 0 os obstaculos movem-se da esquerda para a direita, quando v < 0 os obstaculos movem-se da direita para a esquerda.

A função pode ser definida da seguinte forma:

@
moverObstaculos :: Mapa -> Mapa
moverObstaculos (Mapa l ((t, (o:os)):r)) = (Mapa l (moverObstaculosLinha((t,(o:os)):r)))
@

== Exemplos de utilização
=== Exemplo 1 - Mover Obstaculos Em direções opostas
>>> moverObstaculos (Mapa 3 ([(Rio 2,[Tronco,Nenhum,Tronco]),(Estrada (-2),[Carro,Carro,Nenhum ])]))
Mapa 3 [(Rio 2,[Nenhum,Tronco,Tronco]),(Estrada (-2),[Nenhum,Carro,Carro])]


=== Exemplo 2 - Mover Obstaculos na mesma direção 
>>> moverObstaculos (Mapa 3 ([(Rio 2,[Tronco,Nenhum,Tronco,Nenhum]),(Estrada 2,[Carro,Carro,Nenhum,Carro])]))
Mapa 3 [(Rio 2,[Tronco,Nenhum,Tronco,Nenhum]),(Estrada 2,[Nenhum,Carro,Carro,Carro])]

-} 

moverObstaculos :: Mapa -> Mapa
moverObstaculos (Mapa l ((t, (o:os)):r)) = (Mapa l (moverObstaculosLinha((t, (o:os)):r)))

{-| 
A função __atropelamento__ que recebe um Mapa "dá" um de novo um mapa com o resultado se o um jogador for atropelado o que resulta em a movimentação dos obstaculos neste caso carros pararem.

A função aplica uma paragem ao aos obstaculos jogo pelo jogador ser atropelado através de funções auxiliares, muitos semelhantes com a função mover obstaculos mas com objetivos diferentes.

A função pode ser definida da seguinte forma:

@
atropelamento :: Int -> Mapa -> Mapa
atropelamento x (Mapa l ((t, (o:os)):r)) = (Mapa l (atropelamentoLinha x ((t, (o:os)):r)))
@

== Exemplos de utilização
=== Exemplo 1 - Movimenta Para Cima
>>> atropelamento (Mapa 3 ([(Rio 2,[Tronco,Nenhum,Tronco,Nenhum]),(Estrada 2,[Carro,Carro,Nenhum,Carro])]))
Mapa 3 [(Rio 2,[Nenhum,Tronco,Nenhum,Tronco]),(Estrada 2,[Carro,Carro,Carro,Nenhum])]
-} 

atropelamento :: Int -> Mapa -> Mapa
atropelamento x (Mapa l ((t, (o:os)):r)) = (Mapa l (atropelamentoLinha x ((t, (o:os)):r)))

{-| 
A função __moverObstaculosLinha__ é uma função auxiliar da função __moverObstaculos__, uma função que funciona em conjunto com outra função auxiliar da função __moverObstaculos__, esta função utiliza a função __moverObstaculosVelocidade__ que funciona como um contador recebendo o INT 0 e adicinando 1 até ser o mesmo valor da velocidade do terreno que está a ser movido.

A função pode ser definida da seguinte forma:

@
moverObstaculosLinha :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
moverObstaculosLinha [] = []
moverObstaculosLinha ((t,(o:os)):r) | r == [] = [moverObstaculosVelocidade ((t,(o:os)):r) 0]
                                    | otherwise = (moverObstaculosVelocidade ((t,(o:os)):r) 0): moverObstaculosLinha r
@

== Exemplos de utilização
=== Exemplo 1 - Movimenta os obstaculos "v" vezes na direção pretendida
>>> moverObstaculosLinha [(Estrada 2,[Nenhum,Carro,Carro,Carro])]
[(Estrada 2,[Carro,Carro,Nenhum,Carro])]

-}

moverObstaculosLinha :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
moverObstaculosLinha [] = []
moverObstaculosLinha ((t,(o:os)):r) | r == [] = [moverObstaculosVelocidade ((t,(o:os)):r) 0]
                                    | otherwise = (moverObstaculosVelocidade ((t,(o:os)):r) 0): moverObstaculosLinha r
{-| 
A função __atropelamentoLinha__ é uma função auxiliar da função __atropelamento__, uma função que funciona em conjunto com outra função auxiliar da função __atropelamento__, esta função utiliza a função __moverObstaculosVelocidade__ que funciona como um contador recebendo o INT 0 e adicinando 1 até ser o mesmo valor da velocidade do terreno que está a ser movido.

A função pode ser definida da seguinte forma:

@
atropelamentoLinha :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
atropelamentoLinha [] = []
atropelamentoLinha ((t,(o:os)):r) | r == [] = [atropelamentoVelocidade ((t,(o:os)):r)]
                                  | otherwise = (atropelamentoVelocidade ((t,(o:os)):r)): atropelamentoLinha r
@

== Exemplos de utilização
=== Exemplo 1 - Movimenta os obstaculos "v" vezes na direção pretendida
>>> atropelamentoLinha [(Estrada 2,[Nenhum,Carro,Carro,Carro])]
[(Estrada 2,[Carro,Nenhum,Carro,Carro])]

-}


atropelamentoLinha :: Int -> [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
atropelamentoLinha _ [] = []
atropelamentoLinha x ((Estrada v, (o:os)):r) | r == [] = [atropelamentoAndar ((Estrada v,(o:os)):r) (localizarCarro x (Estrada v,(o:os)))]
                                             | otherwise = (atropelamentoAndar ((Estrada v,(o:os)):r) (localizarCarro x (Estrada v,(o:os)))): atropelamentoLinha x r 
atropelamentoLinha x ((t,(o:os)):r) | r == [] = [moverObstaculosVelocidade ((t,(o:os)):r) 0]
                                    | otherwise = (moverObstaculosVelocidade ((t,(o:os)):r) 0): moverObstaculosLinha r


{-| 
A função __moverObstaculosVelocidade__ é uma função auxiliar da função __moverObstaculos__, uma função que funciona em conjunto com a função __moverObstaculosLinha__ mencionada em cima, esta função é uma especie de contador que começa no 0 ate chegar ao valor v do terreno em questão.

A função pode ser definida da seguinte forma:

@
moverObstaculosVelocidade ::  [(Terreno,[Obstaculo])] -> Int ->  (Terreno,[Obstaculo])
moverObstaculosVelocidade ((Rio v, (o:os)):r) x | v > 0 && x /= v = (moverObstaculosVelocidade ((Rio v, (last os : o : init os)):r) (x+1))
                                                | v < 0 && x /= v = (moverObstaculosVelocidade ((Rio v, (os ++ [o])):r) (x-1))
                                                | otherwise =  ((Rio v, (o:os)))
moverObstaculosVelocidade ((Estrada v, (o:os)):r) x | v > 0 && x /= v = (moverObstaculosVelocidade ((Estrada v, (last os : o : init os)):r) (x+1))
                                                    | v < 0 && x /= v = (moverObstaculosVelocidade ((Estrada v, (os ++ [o])):r) (x-1))
                                                    | otherwise = ((Estrada v, (o:os)))
moverObstaculosVelocidade ((Relva,(o:os)):r) x = (Relva, (o:os))
@

== Exemplos de utilização
=== Exemplo 1 - Move os obstaculos 2x para a direita
>>> [(Estrada 2,[Nenhum,Carro,Carro,Carro])] 0
(Estrada 2,[Carro,Carro,Nenhum,Carro])

-}

moverObstaculosVelocidade ::  [(Terreno,[Obstaculo])] -> Int ->  (Terreno,[Obstaculo])
moverObstaculosVelocidade ((Rio v, (o:os)):r) x | v > 0 && x /= v = (moverObstaculosVelocidade ((Rio v, (last os : o : init os)):r) (x+1))
                                                | v < 0 && x /= v = (moverObstaculosVelocidade ((Rio v, (os ++ [o])):r) (x-1))
                                                | otherwise =  ((Rio v, (o:os)))
moverObstaculosVelocidade ((Estrada v, (o:os)):r) x | v > 0 && x /= v = (moverObstaculosVelocidade ((Estrada v, (last os : o : init os)):r) (x+1))
                                                    | v < 0 && x /= v = (moverObstaculosVelocidade ((Estrada v, (os ++ [o])):r) (x-1))
                                                    | otherwise = ((Estrada v, (o:os)))
moverObstaculosVelocidade ((Relva,(o:os)):r) x = (Relva, (o:os))

{-| 
A função __atropelamentoVelocidade__ é uma função auxiliar da função __atropelamento__, uma função que funciona em conjunto com a função __atropelamentoLinha__ mencionada em cima.

A função pode ser definida da seguinte forma:

@
atropelamentoVelocidade :: [(Terreno,[Obstaculo])] ->  (Terreno,[Obstaculo])
atropelamentoVelocidade ((Estrada v, (o:os)):r)   | v > 0 = (Estrada v, (last os : o : init os))
                                                  | v < 0 = (Estrada v, (os ++ [o]))
                                                  | otherwise = ((Estrada v, (o:os)))
atropelamentoVelocidade ((Rio v, (o:os)):r) | v > 0 = (Rio v, (last os : o : init os))
                                            | v < 0 = (Rio v, (os ++ [o]))
                                            | otherwise = ((Estrada v, (o:os)))
atropelamentoVelocidade ((Relva,(o:os)):r) = (Relva, (o:os))
@

== Exemplos de utilização
=== Exemplo 1 
>>> atropelamentoVelocidade [(Estrada 2,[Nenhum,Carro,Carro,Carro])]
(Estrada 2,[Carro,Nenhum,Carro,Carro])

-}

atropelamentoVelocidade :: [(Terreno,[Obstaculo])] ->  (Terreno,[Obstaculo])
atropelamentoVelocidade ((Estrada v, (o:os)):r)   | v > 0 = (Estrada v, (last os : o : init os))
                                                  | v < 0 = (Estrada v, (os ++ [o]))
                                                  | otherwise = ((Estrada v, (o:os)))
atropelamentoVelocidade ((Rio v, (o:os)):r) | v > 0 = (Rio v, (last os : o : init os))
                                            | v < 0 = (Rio v, (os ++ [o]))
                                            | otherwise = ((Estrada v, (o:os)))
atropelamentoVelocidade ((Relva,(o:os)):r) = (Relva, (o:os))

{-| 
A função __jogadorMove__ é uma função auxiliar, que movimenta o jogador no mapa , tendo em atenção que o jogador não conseguira mover-se para o local onde está uma arvóre.

Também nesta função temos o caso "Parado", que no caso de se encontrar num "Tronco" acompanhará o movimento do tronco.

A função pode ser definida da seguinte forma:

@
jogadorMove :: Jogo -> Jogada -> Jogo
jogadorMove (Jogo (Jogador (x,y)) (Mapa l linhas@((t,(o:os)):r))) (Move Baixo) | (y+1) >= (length (listaTerrenos ((t,(o:os)):r))) =jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                        | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y+1)) == Relva && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y+1))!!(x)) == Arvore = jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                          |(existeCarro x (linhas !! (y+1))) == True = (Jogo (Jogador (x,y+1)) (atropelamento x (Mapa l ((t,(o:os)):r))))
--                                                                         | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y+1)) == Estrada ((qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y+1)))) && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y+1))!!(x)) == Carro = (Jogo (Jogador (99,99)) (atropelamento (Mapa l ((t,(o:os)):r))))
                                                                        | otherwise = (Jogo (Jogador (x,y+1)) (moverObstaculos (Mapa l ((t,(o:os)):r))))

jogadorMove (Jogo (Jogador (x,y)) (Mapa l linhas@((t,(o:os)):r))) (Move Esquerda) | x == 0 || (x-1) < 0 =jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                           | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Relva && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!(x-1)) == Arvore = jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                           | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))>0 && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!x) == Tronco = (Jogo (Jogador (x+(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))-1,y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                           | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))<0 && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!x) == Tronco = (Jogo (Jogador (x-(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))-1,y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                           |(existeCarro (x-1) (linhas !! y)) == True = (Jogo (Jogador (x-1,y)) (atropelamento x (Mapa l ((t,(o:os)):r))))
--                                                                            | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Estrada (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!(x-1)) == Carro = (Jogo (Jogador (99,99)) (atropelamento (Mapa l ((t,(o:os)):r))))
                                                                           | otherwise = (Jogo (Jogador (x-1,y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))


jogadorMove (Jogo (Jogador (x,y)) (Mapa l linhas@((t,(o:os)):r))) (Move Direita) | x == 0 || (x+1) > l =jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                     --     | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Estrada (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!(x-1)) == Carro = (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r))))
                                                                          | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Relva && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!(x+1)) == Arvore = jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                          | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))>0 && (qualObs (qualTerrenoObs ((Mapa l ((t,(o:os)):r))) y)!!x) == Tronco = (Jogo (Jogador ((x+1+(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))),y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                          | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))<0 && (qualObs (qualTerrenoObs ((Mapa l ((t,(o:os)):r))) y)!!x) == Tronco = (Jogo (Jogador (x+1+(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)),y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                          |(existeCarro (x+1) (linhas !! y)) == True = (Jogo (Jogador (x+1,y)) (atropelamento x (Mapa l ((t,(o:os)):r))))
                                                                          | otherwise = (Jogo (Jogador (x+1,y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))

jogadorMove (Jogo (Jogador (x,y)) (Mapa l linhas@((t,(o:os)):r))) (Move Cima) | (y-1) < 0 =jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                      -- | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y-1)) == Estrada (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y-1))) && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y-1))!!x) == Carro = (Jogo (Jogador (x,y)) (atropelamento x (Mapa l ((t,(o:os)):r))))
                                                                       | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y-1)) == Relva && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y-1))!!(x)) == Arvore = jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                       |(existeCarro x (linhas !! (y-1))) == True = (Jogo (Jogador (x,y-1)) (atropelamento x (Mapa l ((t,(o:os)):r))))
                                                                       | otherwise = (Jogo (Jogador (x,y-1)) (moverObstaculos (Mapa l ((t,(o:os)):r))))

jogadorMove (Jogo (Jogador (x,y)) (Mapa l linhas@((t,(o:os)):r))) (Parado) | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))>0 && (qualObs (qualTerrenoObs ((Mapa l ((t,(o:os)):r))) y)!!x) == Tronco = (Jogo (Jogador (x+(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)),y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                           | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))<0 && (qualObs (qualTerrenoObs ((Mapa l ((t,(o:os)):r))) y)!!x) == Tronco = (Jogo (Jogador (x+(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)),y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                           | (existeCarro x (linhas !! y)) == True = (Jogo (Jogador (x,y)) (atropelamento x (Mapa l ((t,(o:os)):r))))
                                                                           | otherwise = (Jogo (Jogador (x,y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
@

== Exemplos de utilização
=== Exemplo 1 - Move Esquerda (Válido)
>>> jogadorMove (Jogo (Jogador (1,2)) (Mapa 4 [(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])])) (Move Esquerda)
Jogo (Jogador (0,2)) (Mapa 4 [(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])])

=== Exemplo 2 - Move Esquerda (Inválido)
>>> jogadorMove (Jogo (Jogador (2,1)) (Mapa 4 [(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])])) (Move Esquerda)
Jogo (Jogador (2,1)) (Mapa 4 [(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])])

-}

jogadorMove :: Jogo -> Jogada -> Jogo
jogadorMove (Jogo (Jogador (x,y)) (Mapa l linhas@((t,(o:os)):r))) (Move Baixo) | (y+1) >= (length (listaTerrenos ((t,(o:os)):r))) =jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                        | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y+1)) == Relva && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y+1))!!(x)) == Arvore = jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                          |(existeCarro x (linhas !! (y+1))) == True = (Jogo (Jogador (x,y+1)) (atropelamento x (Mapa l ((t,(o:os)):r))))
--                                                                         | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y+1)) == Estrada ((qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y+1)))) && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y+1))!!(x)) == Carro = (Jogo (Jogador (99,99)) (atropelamento (Mapa l ((t,(o:os)):r))))
                                                                        | otherwise = (Jogo (Jogador (x,y+1)) (moverObstaculos (Mapa l ((t,(o:os)):r))))

jogadorMove (Jogo (Jogador (x,y)) (Mapa l linhas@((t,(o:os)):r))) (Move Esquerda) | x == 0 || (x-1) < 0 =jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                           | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Relva && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!(x-1)) == Arvore = jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                           | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))>0 && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!x) == Tronco = (Jogo (Jogador (x+(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))-1,y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                           | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))<0 && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!x) == Tronco = (Jogo (Jogador (x-(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))-1,y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                           |(existeCarro (x-1) (linhas !! y)) == True = (Jogo (Jogador (x-1,y)) (atropelamento x (Mapa l ((t,(o:os)):r))))
--                                                                            | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Estrada (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!(x-1)) == Carro = (Jogo (Jogador (99,99)) (atropelamento (Mapa l ((t,(o:os)):r))))
                                                                           | otherwise = (Jogo (Jogador (x-1,y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))


jogadorMove (Jogo (Jogador (x,y)) (Mapa l linhas@((t,(o:os)):r))) (Move Direita) | x == 0 || (x+1) > l =jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                     --     | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Estrada (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!(x-1)) == Carro = (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r))))
                                                                          | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Relva && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)!!(x+1)) == Arvore = jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                          | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))>0 && (qualObs (qualTerrenoObs ((Mapa l ((t,(o:os)):r))) y)!!x) == Tronco = (Jogo (Jogador ((x+1+(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))),y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                          | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))<0 && (qualObs (qualTerrenoObs ((Mapa l ((t,(o:os)):r))) y)!!x) == Tronco = (Jogo (Jogador (x+1+(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)),y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                          |(existeCarro (x+1) (linhas !! y)) == True = (Jogo (Jogador (x+1,y)) (atropelamento x (Mapa l ((t,(o:os)):r))))
                                                                          | otherwise = (Jogo (Jogador (x+1,y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))

jogadorMove (Jogo (Jogador (x,y)) (Mapa l linhas@((t,(o:os)):r))) (Move Cima) | (y-1) < 0 =jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                      -- | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y-1)) == Estrada (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y-1))) && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y-1))!!x) == Carro = (Jogo (Jogador (x,y)) (atropelamento x (Mapa l ((t,(o:os)):r))))
                                                                       | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y-1)) == Relva && (qualObs (qualTerrenoObs (Mapa l ((t,(o:os)):r)) (y-1))!!(x)) == Arvore = jogadorMove (Jogo (Jogador (x,y)) ((Mapa l ((t,(o:os)):r)))) (Parado)
                                                                       |(existeCarro x (linhas !! (y-1))) == True = (Jogo (Jogador (x,y-1)) (atropelamento x (Mapa l ((t,(o:os)):r))))
                                                                       | otherwise = (Jogo (Jogador (x,y-1)) (moverObstaculos (Mapa l ((t,(o:os)):r))))

jogadorMove (Jogo (Jogador (x,y)) (Mapa l linhas@((t,(o:os)):r))) (Parado) | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))>0 && (qualObs (qualTerrenoObs ((Mapa l ((t,(o:os)):r))) y)!!x) == Tronco = (Jogo (Jogador (x+(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)),y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                           | qualTerreno (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y) == Rio (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)) && (qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y))<0 && (qualObs (qualTerrenoObs ((Mapa l ((t,(o:os)):r))) y)!!x) == Tronco = (Jogo (Jogador (x+(qualV (qualTerrenoObs (Mapa l ((t,(o:os)):r)) y)),y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))
                                                                           | (existeCarro x (linhas !! y)) == True = (Jogo (Jogador (x,y)) (atropelamento x (Mapa l ((t,(o:os)):r))))
                                                                           | otherwise = (Jogo (Jogador (x,y)) (moverObstaculos (Mapa l ((t,(o:os)):r))))

{-| 
A função __qualTerrenoObs__ é uma função auxiliar da função __jogadorMove__, uma função que funciona dando um Mapa e um Int (que é o y) dá a linha em que o jogador está usado para ver se se pode mover.

A função pode ser definida da seguinte forma:

@
qualTerrenoObs :: Mapa -> Int -> (Terreno,[Obstaculo])
qualTerrenoObs (Mapa l ((t,(o:os)):r)) n = (((t,(o:os)):r))!!(n)
@

== Exemplos de utilização
=== Exemplo 1 - Utilizando o Int 1 verificamos em linha está o personagem
>>> qualTerrenoObs (Mapa 4 [(Estrada 3,[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Arvore]),(Rio (-5),[Tronco,Nenhum,Tronco,Nenhum])]) 1
(Relva,[Arvore,Arvore,Nenhum,Arvore])

-}

qualTerrenoObs :: Mapa -> Int -> (Terreno,[Obstaculo])
qualTerrenoObs (Mapa l ((t,(o:os)):r)) n = (((t,(o:os)):r))!!(n)


{-| 
A função __qualTerreno__ é uma função auxiliar da função __jogadorMove__, uma função que funciona dando um Terreno e uma Lista de Obstaculos dá o terreno em que jogador está para ser usado como futura restrição.

A função pode ser definida da seguinte forma:

@
qualTerreno :: (Terreno,[Obstaculo]) -> Terreno
qualTerreno (t,(o:os)) = t 
@

== Exemplos de utilização
=== Exemplo 1 - Dá nos o nome do Terreno em que o personagem está
>>> qualTerreno (Estrada 3,[Carro,Carro,Nenhum,Nenhum])
Estrada 3

-}

qualTerreno :: (Terreno,[Obstaculo]) -> Terreno
qualTerreno (t,(o:os)) = t


{-| 
A função __qualObs__ é uma função auxiliar da função __jogadorMove__, uma função que funciona dando um Terreno e uma Lista de Obstaculos dá a lista de Obstaculos da linha em que jogador está para ser usado como futura restrição.

A função pode ser definida da seguinte forma:

@
qualObs :: (Terreno,[Obstaculo]) -> [Obstaculo]
qualObs (t,(o:os)) = (o:os)
@

== Exemplos de utilização
=== Exemplo 1 - Dá a lista de Obstaculos da linha em que jogador está
>>> qualObs (Estrada 3,[Carro,Carro,Nenhum,Nenhum])
[Carro,Carro,Nenhum,Nenhum]

-}

qualObs :: (Terreno,[Obstaculo]) -> [Obstaculo]
qualObs (t,(o:os)) = (o:os)

{-| 
A função __listaTerrenos__ é uma função auxiliar da função __jogadorMove__, uma função que funciona dando uma lista de Terreno e uma Lista de Obstaculos dá a lista de Terrenos para ser usado como futura restrição.

A função pode ser definida da seguinte forma:

@
listaTerrenos :: [(Terreno,[Obstaculo])] -> [Terreno]
listaTerrenos [] = []
listaTerrenos ((t,(o:os)):r) = t : listaTerrenos r
@

== Exemplos de utilização
=== Exemplo 1 - Dá a lista de Obstaculos da linha em que jogador está
>>> listaTerrenos [(Estrada 2,[Carro, Carro, Carro, Nenhum]),(Rio 3,[Tronco,Tronco,Nenhum,Nenhum])]
[Estrada 2,Rio 3]

-}

listaTerrenos :: [(Terreno,[Obstaculo])] -> [Terreno]
listaTerrenos [] = []
listaTerrenos ((t,(o:os)):r) = t : listaTerrenos r

{-| 
A função __qualV__ é uma função auxiliar da função __jogadorMove__, uma função que funciona dando um Terreno e uma Lista de Obstaculos dá a velocidade do terreno para ser usado como futura restrição,(esta função só é usada para rio pois só é para o caso do movimento parado em cima de um tronco.).

A função pode ser definida da seguinte forma:

@
qualV :: (Terreno,[Obstaculo]) -> Int
qualV (Rio v,(o:os)) = v
@

== Exemplos de utilização
=== Exemplo 1 - Dá a velocidade do RIo onde o jogador está 
>>> qualV (Rio 3,[Tronco, Tronco, Nenhum ])
3

-}

qualV :: (Terreno,[Obstaculo]) -> Int
qualV (Rio v,(o:os)) = v
qualV (Estrada v, (o:os)) = v
qualV (Relva, (o:os)) = 0

-- função se existe algum carro 
existeCarro :: Int -> (Terreno, [Obstaculo]) -> Bool -- existe carro atras ou à frente
existeCarro x (Estrada v,(o:os)) | v < 0 && elem Carro (take (-v) (drop (x+1) (cycle (o:os)))) = True
                                 | v > 0 && x >= v && elem Carro (take v (drop (x-v) (cycle (o:os)))) = True 
                                 | v > 0 && x == 0 && elem Carro (take v (drop (x-v) (reverse (take (2*length(o:os)) (cycle(o:os)))))) = True
                                 | v > 0 && x < v && elem Carro ((take (x+1) (o:os))++(take (v-x) (reverse (take (2*length(o:os)) (cycle(o:os)))))) = True
                                 | otherwise = False    
existeCarro _ _ = False

-- função apra localizar carro
localizarCarro :: Int -> (Terreno, [Obstaculo]) -> Int
localizarCarro x (Estrada v, (o:os)) | (o:os) !! x == Carro = 0
                                     | v < 0 = -(fromMaybe 0 (elemIndex Carro (take (-v) (drop (x+1) (cycle (o:os))))))
                                     | v > 0 && x == 0 = fromMaybe 0 (elemIndex Carro (take v (drop (x-v) (reverse (take (2*length(o:os)) (cycle(o:os))))))) + 1
                                     | v > 0 && x < v = fromMaybe 0 (elemIndex Carro ((take x (o:os))++(take (v-x) (reverse (take (2*length(o:os)) (cycle(o:os))))))) + 1
                                     | otherwise = 0
localizarCarro _ _ = 0


-- andar ate à pessoa
atropelamentoAndar ::  [(Terreno,[Obstaculo])] -> Int ->  (Terreno,[Obstaculo])
atropelamentoAndar ((Rio v, (o:os)):r) x | v > 0 && x /= v = (atropelamentoAndar ((Rio v, (last os : o : init os)):r) (x+1))
                                         | v < 0 && x /= v = (atropelamentoAndar ((Rio v, (os ++ [o])):r) (x-1))
                                         | otherwise =  ((Rio v, (o:os)))
atropelamentoAndar ((Estrada v, (o:os)):r) x | v > 0 && x /= v = (atropelamentoAndar ((Estrada v, (last os : o : init os)):r) (x+1))
                                             | v < 0 && x /= v = (atropelamentoAndar ((Estrada v, (os ++ [o])):r) (x-1))
                                             | otherwise = (Estrada v, (o:os))
atropelamentoAndar ((Relva,(o:os)):r) x = (Relva, (o:os))