{- |
Module      : Tarefa4_2022li1g011
Description : Determinar se o jogo terminou
Copyright   : Pedro Seabra Vieira <a104352@alunos.uminho.pt>
              Pedro Filipe Maneta Pinto <a104176@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g011_Gloss where

import LI12223
import Tarefa2_2022li1g011_Gloss

-- * Função principal

{-| 
A função 'deslizaJogo' recebe um inteiro (seed) e um 'Jogo' e entrega um 'Jogo', fazendo com que a  última linha do mapa desapareça, ao mesmo tempo que uma nova linha no topo do mapa seja criada, adicionando uma casa ao eixo dos y, uma vez que o mapa movimenta-se o jogador fica para traz.

A função pode ser definida da seguinte forma:

@
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) mapa@(Mapa l ((t,(o:os)):r))) = (Jogo (Jogador (x,y+1)) (estendeMapa (Mapa l (eliminarLinha mapa)) seed))
@

== Exemplos de utilização
=== Exemplo 1 
>>> deslizaJogo 12 (Jogo (Jogador (3,2)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Nenhum]),(Rio 1, [Nenhum, Tronco, Tronco]),(Estrada 0, [Nenhum, Nenhum, Nenhum]),(Rio 1, [Nenhum, Tronco, Tronco]),(Relva, [Nenhum, Nenhum, Nenhum])]))
Jogo (Jogador (3,3)) (Mapa 3 [(Rio (-1),[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Estrada 0,[Nenhum,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco])])

=== Exemplo 2 
>>> deslizaJogo 32 (Jogo (Jogador (3,2)) (Mapa 3 [(Estrada 2, [Nenhum, Carro, Nenhum]),(Rio 1, [Nenhum, Tronco, Tronco]),(Relva, [Nenhum, Arvore, Nenhum])]))
Jogo (Jogador (3,3)) (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Estrada 2,[Nenhum,Carro,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco])])

-}
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) mapa@(Mapa l ((t,(o:os)):r))) = (Jogo (Jogador (x,y-1)) (estendeMapa (Mapa l (eliminarLinha mapa)) seed))


-- * Função auxiliar

{-| 
A função 'eliminarLinha' recebe um 'Mapa' e elimina a última linha do mesmo, entregando um conjuntos de pares de um 'Terreno' e um conjuntos de 'Obstaculos'.

A função pode ser definida da seguinte forma:

@
eliminarLinha :: Mapa -> [(Terreno,[Obstaculo])]
eliminarLinha (Mapa l []) = []
eliminarLinha (Mapa l (linha:r)) | n <= 0 = []
                                 | otherwise = (linha : (eliminarLinha (Mapa (n-1) r)))
                                 where n = length(linha:r) - 1 
@

== Exemplos de utilização
=== Exemplo 1 
>>> eliminarLinha (Mapa 3 [(Estrada 2, [Nenhum, Carro, Nenhum]),(Rio 1, [Nenhum, Tronco, Tronco]),(Relva, [Nenhum, Arvore, Nenhum])])
[(Estrada 2,[Nenhum,Carro,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco])]

-}
eliminarLinha :: Mapa -> [(Terreno,[Obstaculo])]
eliminarLinha (Mapa l []) = []
eliminarLinha (Mapa l (linha:r)) | n <= 0 = []
                                 | otherwise = (linha : (eliminarLinha (Mapa (n-1) r)))
                                 where n = length(linha:r) - 1 
