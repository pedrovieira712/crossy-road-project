module Tarefa2_2022li1g011_Spec where

import LI12223
import Tarefa2_2022li1g011
import Test.HUnit


test1 = TestCase (assertEqual "Lista Vazia" [Rio 0, Estrada 0, Relva] (proximosTerrenosValidos (Mapa 3 [])))
test2 = TestCase (assertEqual "Rios Contíguos" [Estrada 0, Relva] (proximosTerrenosValidos (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum]),(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Tronco,Nenhum])])))
test3 = TestCase (assertEqual "Relvas Contíguas" [Rio 0,Estrada 0] (proximosTerrenosValidos (Mapa 2 [(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore]),(Relva,[Arvore,Nenhum]),(Relva,[Nenhum,Arvore])])))
test4 = TestCase (assertEqual "Rios Contíguos" [Rio 0, Relva] (proximosTerrenosValidos (Mapa 2 [(Estrada 1,[Nenhum,Carro]),(Estrada 1,[Carro,Nenhum]),(Estrada 1,[Nenhum,Carro]),(Estrada 1,[Carro,Nenhum]),(Estrada 1,[Nenhum,Carro])])))
test5 = TestCase (assertEqual "Mapa Normal" [Rio 0, Estrada 0, Relva] (proximosTerrenosValidos (Mapa 3 [(Relva,[Nenhum,Nenhum,Arvore]),(Relva,[Arvore,Nenhum,Arvore]),(Estrada 1,[Carro,Nenhum,Carro]),(Estrada 1,[Carro,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco])])))
test6 = TestCase (assertEqual "Recebe lista vazia Estrada" [Nenhum,Carro] (proximosObstaculosValidos 3 (Estrada 1 , [] )))
test7 = TestCase (assertEqual "Recebe lista vazia Relva" [Nenhum,Arvore] (proximosObstaculosValidos 3 (Relva , [] )))
test8 = TestCase (assertEqual "Recebe lista vazia Rio" [Nenhum,Tronco] (proximosObstaculosValidos 3 (Rio 1 , [] )))
test9 = TestCase (assertEqual "Dá lista vazia Rio" [] (proximosObstaculosValidos 2 (Rio 1 , [Nenhum,Tronco] )))
test10 = TestCase (assertEqual "Dá lista vazia Relva" [] (proximosObstaculosValidos 2 (Relva , [Nenhum,Arvore] )))
test11 = TestCase (assertEqual "Dá lista vazia Estrada" [] (proximosObstaculosValidos 2 (Estrada 1 , [Nenhum,Carro] )))
test12 = TestCase (assertEqual "Ter pelo menos um «Nenhum» no Rio" [Nenhum] (proximosObstaculosValidos 4 (Rio 1 , [Tronco,Tronco,Tronco] )))
test22 = TestCase (assertEqual "Ter pelo menos um «Nenhum» na Relva" [Nenhum] (proximosObstaculosValidos 4 (Relva , [Arvore,Arvore,Arvore] )))
test13 = TestCase (assertEqual "Ter pelo menos um «Nenhum» na Estrada" [Nenhum] (proximosObstaculosValidos 4 (Estrada 1 , [Carro,Carro,Carro] )))
test14 = TestCase (assertEqual "Troncos com + de 5 uni de comprimento" [Nenhum] (proximosObstaculosValidos 6 (Rio 1 , [Tronco,Tronco,Tronco,Tronco,Tronco] )))
test15 = TestCase (assertEqual "Troncos com + de 5 uni de comprimento caso de Loop" [Nenhum] (proximosObstaculosValidos 7 (Rio 1 , [Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum] )))
test16 = TestCase (assertEqual "Haver pelo menos um Tronco no Rio" [Tronco] (proximosObstaculosValidos 4 (Rio 1 , [Nenhum,Nenhum,Nenhum] )))
test17 = TestCase (assertEqual "Carros com + de 3 uni de comprimento" [Nenhum] (proximosObstaculosValidos 4 (Estrada 1 , [Carro,Carro,Carro] )))
test18 = TestCase (assertEqual "Carros com + de 3 uni de comprimento caso de Loop" [Nenhum] (proximosObstaculosValidos 6 (Estrada 1 , [Carro,Carro,Nenhum,Nenhum,Car:ro] )))
test19 = TestCase (assertEqual "Caso Normal Rios" [Nenhum,Tronco] (proximosObstaculosValidos 6 (Rio 1 , [Tronco,Nenhum,Tronco] )))
test20 = TestCase (assertEqual "Caso Normal Estrada" [Nenhum,Carro] (proximosObstaculosValidos 6 (Estrada 1 , [Carro,Nenhum,Carro] )))
test21 = TestCase (assertEqual "Caso Normal Relva" [Nenhum,Arvore] (proximosObstaculosValidos 6 (Relva , [Arvore,Nenhum,Arvore] )))

testsT2 = TestList [test1,test2,test3,test4,test5,test6,test7,test8,test9,test10,test11,test12,test13,test14,test15,test16,test17,test18,test19,test20,test21]