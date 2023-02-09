module Tarefa1_2022li1g011_Spec where

import LI12223
import Tarefa1_2022li1g011
import Test.HUnit

test1 = TestCase (assertEqual "Carro em Rio" False (mapaValido (Mapa 4 [(Rio 2,[Nenhum, Carro, Tronco, Nenhum])])))
test2 = TestCase (assertEqual "Arvore em Rio" False (mapaValido (Mapa 4 [(Rio 2,[Nenhum, Arvore, Tronco, Nenhum])])))
test3 = TestCase (assertEqual "Tronco em Estrada" False (mapaValido (Mapa 4 [(Estrada 2,[Nenhum, Carro, Tronco, Nenhum])])))
test4 = TestCase (assertEqual "Arvore em Estrada" False (mapaValido (Mapa 4 [(Estrada 2,[Nenhum, Arvore, Carro, Nenhum])])))
test5 = TestCase (assertEqual "Tronco em Relva" False (mapaValido (Mapa 4 [(Relva,[Nenhum, Arvore, Tronco, Nenhum])])))
test6 = TestCase (assertEqual "Carro em Relva" False (mapaValido (Mapa 4 [(Estrada 2,[Nenhum, Carro, Arvore, Nenhum])])))
test7 = TestCase (assertEqual "Terreno Válido" True (mapaValido (Mapa 4 [(Estrada 2,[Nenhum, Carro, Carro, Nenhum]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Relva,[Nenhum, Arvore, Arvore, Nenhum])])))
test8 = TestCase (assertEqual "Rio positivo seguido de Rio negativo" True (mapaValido (Mapa 4 [(Estrada 2,[Nenhum, Carro, Carro, Carro]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Rio (-2),[Nenhum, Tronco, Nenhum, Tronco]),(Relva,[Nenhum, Nenhum, Arvore, Nenhum])])))
test9 = TestCase (assertEqual "Rio negativo seguido de Rio positivo" True (mapaValido (Mapa 4 [(Estrada 2,[Nenhum, Carro, Carro, Carro]),(Rio (-2),[Nenhum, Tronco, Nenhum, Tronco]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Relva,[Nenhum, Nenhum, Arvore, Nenhum])])))
test10 = TestCase (assertEqual "Rio positivo seguido de Rio positivo" False (mapaValido (Mapa 4 [(Estrada 2,[Nenhum, Carro, Carro, Carro]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Relva,[Nenhum, Nenhum, Arvore, Nenhum])])))
test11 = TestCase (assertEqual "Rio negativo seguido de Rio negativo" False (mapaValido (Mapa 4 [(Estrada 2,[Nenhum, Carro, Carro, Carro]),(Rio (-2),[Nenhum, Tronco, Nenhum]),(Rio (-2),[Nenhum, Tronco, Nenhum]),(Relva,[Nenhum, Nenhum, Arvore, Nenhum])])))
test12 = TestCase (assertEqual "Rio negativo e Rio negativo não contiguos" True (mapaValido (Mapa 4 [(Rio (-2),[Nenhum, Tronco, Nenhum, Tronco]),(Estrada 2,[Nenhum, Carro, Carro, Carro]),(Rio (-2),[Nenhum, Tronco, Nenhum, Tronco]),(Relva,[Nenhum, Nenhum, Arvore, Nenhum])])))
test13 = TestCase (assertEqual "Rio negativo e Rio positivo não contiguos" True (mapaValido (Mapa 4 [(Rio (-2),[Nenhum, Tronco, Nenhum, Tronco]),(Estrada 2,[Nenhum, Carro, Carro, Carro]),(Rio (2),[Nenhum, Tronco, Nenhum, Tronco]),(Relva,[Nenhum, Nenhum, Arvore, Nenhum])])))
test14 = TestCase (assertEqual "Rios Contiguos (+-+-)" True (mapaValido (Mapa 4 [(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Rio (-2),[Nenhum, Tronco, Nenhum, Tronco]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Rio (-2),[Nenhum, Tronco, Nenhum, Tronco])])))
test15 = TestCase (assertEqual "Rios Contiguos(-+-+)" True (mapaValido (Mapa 4 [(Rio (-2),[Nenhum, Tronco, Nenhum, Tronco]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Rio (-2),[Nenhum, Tronco, Nenhum, Tronco]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco])])))
test16 = TestCase (assertEqual "Rios Contiguos (++++)" False (mapaValido (Mapa 4 [(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco]),(Rio 2,[Nenhum, Tronco, Nenhum, Tronco])])))
test17 = TestCase (assertEqual "Rios Contiguos (----)" False (mapaValido (Mapa 4 [(Rio (-2),[Nenhum, Tronco, Nenhum]),(Rio (-2),[Nenhum, Tronco, Nenhum]),(Rio (-2),[Nenhum, Tronco, Nenhum]),(Rio (-2),[Nenhum, Tronco, Nenhum])])))
test18 = TestCase (assertEqual "Pelo menos um Nenhum num Rio" False (mapaValido (Mapa 4 [(Rio 2,[Tronco, Tronco , Tronco , Tronco])])))
test19 = TestCase (assertEqual "Pelo menos um Nenhum numa Estrada" False (mapaValido (Mapa 4 [(Estrada 2,[Carro, Carro, Carro, Carro])])))
test20 = TestCase (assertEqual "Pelo menos um Nenhum numa Relva" False (mapaValido (Mapa 4 [(Relva,[Arvore, Arvore, Arvore, Arvore])])))
test21 = TestCase (assertEqual "Largura maior que numero de obstaculos" False (mapaValido (Mapa 5 [(Rio 2,[Tronco, Tronco , Nenhum, Nenhum])])))
test22 = TestCase (assertEqual "Carro mais 3 de comprimento" False (mapaValido (Mapa 5 [(Estrada 2,[Carro, Carro, Carro, Nenhum ,Carro])])))
test23 = TestCase (assertEqual "Carro mais 3 de comprimento1" False (mapaValido (Mapa 5 [(Estrada 2,[Nenhum, Carro, Carro, Carro, Carro ])])))
test24 = TestCase (assertEqual "Carro mais 3 de comprimento2" True (mapaValido (Mapa 4 [(Estrada 2,[Carro, Carro, Carro, Nenhum])])))
test25 = TestCase (assertEqual "Tronco mais 5 de comprimento" False (mapaValido (Mapa 8 [(Rio 2,[Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum, Nenhum ,Tronco])])))
test26 = TestCase (assertEqual "Tronco mais 5 de comprimento 1" True (mapaValido (Mapa 7 [(Rio 2,[Tronco, Tronco, Tronco, Tronco, Tronco, Nenhum, Nenhum])])))
test27 = TestCase (assertEqual "Tronco mais 5 de comprimento 2" False (mapaValido (Mapa 7 [(Rio 2,[Nenhum,Tronco, Tronco, Tronco, Tronco, Tronco, Tronco])])))
test28 = TestCase (assertEqual "4 Rios seguidos" True (mapaValido (Mapa 3 [(Rio (-2),[Nenhum,Tronco, Tronco]),(Rio (8),[Nenhum, Tronco, Nenhum]),(Rio (-6),[Nenhum, Tronco, Nenhum]),(Rio (6),[Nenhum, Tronco, Nenhum])])))
test29 = TestCase (assertEqual "5 Rios seguidos" False (mapaValido (Mapa 3 [(Rio (-2),[Nenhum,Tronco, Tronco]),(Rio (8),[Nenhum, Tronco, Nenhum]),(Rio (-6),[Nenhum, Tronco, Nenhum]),(Rio (6),[Nenhum, Tronco, Nenhum]),(Rio (-2),[Nenhum, Tronco, Nenhum])])))
test30 = TestCase (assertEqual "5 Estradas seguidos" True (mapaValido (Mapa 3 [(Estrada (-2),[Nenhum,Carro,Carro]),(Estrada (8),[Nenhum,Carro,Carro]),(Estrada (-6),[Nenhum,Carro,Carro]),(Estrada (6),[Nenhum,Carro,Carro]),(Estrada (-2),[Nenhum,Carro,Carro])])))
test31 = TestCase (assertEqual "6 Estradas seguidos" False (mapaValido (Mapa 3 [(Estrada (-2),[Nenhum,Carro,Carro]),(Estrada (8),[Nenhum,Carro,Carro]),(Estrada (-6),[Nenhum,Carro,Carro]),(Estrada (6),[Nenhum,Carro,Carro]),(Estrada (-2),[Nenhum,Carro,Carro]),(Estrada (6),[Nenhum,Carro,Carro])])))
test32 = TestCase (assertEqual "5 Relvas seguidos" True (mapaValido (Mapa 3 [(Relva,[Arvore, Nenhum , Arvore]),(Relva,[Arvore, Nenhum , Arvore]),(Relva,[Arvore, Nenhum , Arvore]),(Relva,[Arvore, Nenhum , Arvore]),(Relva,[Arvore, Nenhum , Arvore])])))
test33 = TestCase (assertEqual "6 Relvas seguidos" False (mapaValido (Mapa 3 [(Relva,[Arvore, Nenhum , Arvore]),(Relva,[Arvore, Nenhum , Arvore]),(Relva,[Arvore, Nenhum , Arvore]),(Relva,[Arvore, Nenhum , Arvore]),(Relva,[Arvore, Nenhum , Arvore]),(Relva,[Arvore, Nenhum , Arvore])])))
test34 = TestCase (assertEqual "Velocidade Diferente de 0" True (mapaValido (Mapa 3 [(Estrada (-2),[Nenhum,Carro,Carro]),(Rio (8),[Nenhum,Tronco,Tronco])])))
test35 = TestCase (assertEqual "Velocidade 0" False (mapaValido (Mapa 3 [(Estrada (-2),[Nenhum,Carro,Carro]),(Rio (0),[Nenhum,Tronco,Tronco])])))





testsT1 = TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21,test22,test23,test24,test25,test26,test27,test28,test29,test30,test31,test32,test33,test34,test35]