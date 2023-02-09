{- |
Module      : Tarefa4_2022li1g011
Description : Determinar se o jogo terminou
Copyright   : Pedro Seabra Vieira <a104352@alunos.uminho.pt>
              Pedro Filipe Maneta Pinto <a104176@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g011_Gloss where

import LI12223

-- * Função principal

{-| 
A função 'jogoTerminou' que recebe um Jogo e uma jogada que entrega um Booleano sendo se for __True__ o jogo terá terminado e se for __False__ o jogo continua.

A função aplica animação ao Jogo através de 2 funções auxiliares principais, uma que movimenta o mapa e outra que faz moviementar o personagem, na função principal vai juntar as funções auxiliares de modo a criar um tipo de animação ao Jogo.

__O moviemnto do jogador não acontece se e só se o local para onde ele se for mover for ocupado por uma árvore.__

A função pode ser definida da seguinte forma:

@
jogoTerminou :: Jogo -> Bool
jogoTerminou jogo@(Jogo (Jogador (x,y)) (Mapa l ((t,(o:os)):r))) = jogadorForaMapa jogo || jogadorMortoObstaculo (Jogo (Jogador (x,y)) (Mapa l ((linhaY jogo):r)))
@

== Exemplos de utilização
=== Exemplo 1 - Movimenta Para Cima
>>> animaJogo (Jogo (Jogador (1,2)) (Mapa 4 ([(Rio 2,[Tronco,Nenhum,Tronco,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Rio 3,[Tronco,Nenhum,Tronco,Tronco])]))) (Move Cima)
Jogo (Jogador (1,1)) (Mapa 4 [(Rio 2,[Tronco,Nenhum,Tronco,Nenhum]),(Estrada (-2),[Nenhum,Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Rio 3,[Nenhum,Tronco,Tronco,Tronco])])

=== Exemplo 2 - Movimenta Para Baixo (Com Uma Árvore em Baixo)
>>> animaJogo (Jogo (Jogador (2,1)) (Mapa 4 ([(Rio (-3),[Tronco,Nenhum,Tronco,Nenhum]),(Estrada (-2),[Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Tronco])]))) (Move Baixo)
Jogo (Jogador (2,1)) (Mapa 4 [(Rio (-3),[Nenhum,Tronco,Nenhum,Tronco]),(Estrada (-2),[Nenhum,Nenhum,Carro,Carro]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Rio 1,[Tronco,Tronco,Nenhum,Tronco])])



-} 

jogoTerminou :: Jogo -> Bool
jogoTerminou jogo@(Jogo (Jogador (x,y)) (Mapa l ((t,(o:os)):r))) = jogadorForaMapa jogo || jogadorMortoObstaculo (Jogo (Jogador (x,y)) (Mapa l ((linhaY jogo):r))) 

jogadorForaMapa :: Jogo -> Bool
jogadorForaMapa (Jogo (Jogador (x,y)) (Mapa l ((t,(o:os)):r))) | x < 0 || x > l-1 || y > 0 || y < (-(length (listaTerrenos1 ((t,(o:os)):r)))+1) = True
                                                               | otherwise = False

jogadorMortoObstaculo :: Jogo -> Bool
jogadorMortoObstaculo (Jogo (Jogador (x,y)) (Mapa l ((Rio v,(o:os)):r))) | (o:os) !! x /= Tronco = True
                                                                         | otherwise = False
jogadorMortoObstaculo (Jogo (Jogador (x,y)) (Mapa l ((Estrada v,(o:os)):r))) | (o:os) !! x == Carro = True
                                                                             | (o:os) !! x == CarroVP = True
                                                                             | otherwise = False
jogadorMortoObstaculo (Jogo (Jogador (x,y)) (Mapa l ((Relva ,(o:os)):r))) = False

linhaY :: Jogo -> (Terreno,[Obstaculo])
linhaY (Jogo (Jogador (x,y)) (Mapa l ((t,(o:os)):r))) = ((t,(o:os)):r) !! (-y) -- menos porcausa do referencial no gloss 

listaTerrenos1 :: [(Terreno,[Obstaculo])] -> [Terreno]
listaTerrenos1 [] = []
listaTerrenos1 ((t,(o:os)):r) = t : listaTerrenos1 r