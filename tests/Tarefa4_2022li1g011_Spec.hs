module Tarefa4_2022li1g011_Spec where

import LI12223
import Tarefa4_2022li1g011
import Test.HUnit

test1 = TestCase(assertEqual "Atropelamento" True (jogoTerminou (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Estrada 2,[Nenhum,Carro,Nenhum])]))))
test2 = TestCase(assertEqual "Afogamento" True (jogoTerminou (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Rio 2,[Tronco,Nenhum,Nenhum])]))))
test3 = TestCase(assertEqual "No Rio em cima de um Tronco" False (jogoTerminou (Jogo(Jogador (3,1))(Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Tronco])]))))
test4 = TestCase(assertEqual "Na Relva em cima de «Nenhum»" False (jogoTerminou (Jogo(Jogador (1,1))(Mapa 3 [(Relva,[Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Arvore])]))))
test5 = TestCase(assertEqual "Na Estrada em cima de «Nenhum»" False (jogoTerminou (Jogo(Jogador (3,1))(Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Estrada 2,[Carro,Nenhum,Carro,Nenhum])]))))
test6 = TestCase(assertEqual "Fora do Mapa eixo dos X positivo" True (jogoTerminou (Jogo(Jogador (4,1))(Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Tronco])]))))
test7 = TestCase(assertEqual "Fora do Mapa eixo dos X negativo" True (jogoTerminou (Jogo(Jogador ((-1),1))(Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Tronco])]))))
test8 = TestCase(assertEqual "Fora do Mapa eixo dos Y positivo" True (jogoTerminou (Jogo(Jogador (3,2))(Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Tronco])]))))
test9 = TestCase(assertEqual "Fora do Mapa eixo dos Y negativo" True (jogoTerminou (Jogo(Jogador (3,(-2)))(Mapa 4 [(Relva,[Nenhum,Nenhum,Nenhum,Arvore]),(Rio 2,[Nenhum,Nenhum,Nenhum,Tronco])]))))


testsT4 = TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9]

