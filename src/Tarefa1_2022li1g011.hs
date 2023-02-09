{- |
Module      : Tarefa1_2022li1g011
Description : Validação de um mapa
Copyright   : Pedro Seabra Vieira <a104352@alunos.uminho.pt>
              Pedro Filipe Maneta Pinto <a104176@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}

module Tarefa1_2022li1g011 where

import LI12223


-- * Função principal

{-| 
A função __mapaValido__ que recebe um mapa e verifica se o mesmo é válido retornando um booleano (/True/ se o mapa for válido ou /False/ se o mapa não for válido).

A função verifica se uma mapa é válido através de várias funções auxiliares, com certas restrições cada, estás funções auxiliares retornam um booleano, no qual a função principal vai juntar os resultados e comparar, sendo que quando uma função auxiliar retorna um valor booleano como /False/ a função principal retorna também um /False/.

__Um mapa é válido se e somente se não infringir nenhuma restrição.__


A função pode ser definida da seguinte forma:

@
mapaValido :: Mapa -> Bool
mapaValido (Mapa _ []) = False
mapaValido (Mapa _ ((_,[]):r)) = False
mapaValido mapa@(Mapa l ((t,(o:os)):r)) = terrenoValido mapa && velocidadeValida mapa && riosContiguosValido mapa && linhaValida mapa && comprimentoValido mapa && maxTerrenoValido mapa
@

== Exemplos de utilização
=== Exemplo 1 - Mapa válido
>>> mapaValido ((Mapa 8 [(Estrada 3,[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum]),(Estrada (-3),[Carro,Nenhum,Carro,Carro,Carro,Carro,Nenhum,Nenhum]),(Relva,[ Arvore, Nenhum, Arvore,Arvore, Arvore,  Nenhum,  Arvore, Arvore]),(Relva,[Arvore, Arvore, Arvore, Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),(Rio (-5),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Nenhum]),(Rio 2,[Nenhum,Nenhum,Nenhum,Tronco,Tronco,Nenhum,Tronco,Tronco])]))
True


=== Exemplo 2 - Mapa inválido
>>> mapaValido ((Mapa 8 [(Estrada 3,[Carro,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum]),(Estrada (-3),[Carro,Nenhum,Carro,Carro,Carro,Carro,Nenhum,Nenhum]),(Relva,[ Arvore, Nenhum, Arvore,Arvore, Arvore,  Nenhum,  Arvore, Arvore]),(Relva,[Arvore, Arvore, Arvore, Arvore,Nenhum,Nenhum,Arvore,Arvore]),(Relva,[Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum]),(Rio 5,[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Nenhum]),(Rio 2,[Tronco,Tronco,Nenhum,Tronco,Tronco,Nenhum,Tronco,Tronco])]))
False

-} 

mapaValido :: Mapa -> Bool
mapaValido (Mapa _ []) = False
mapaValido (Mapa _ ((_,[]):r)) = False
mapaValido mapa@(Mapa l ((t,(o:os)):r)) = terrenoValido mapa 
                                        && velocidadeValida mapa
                                        && riosContiguosValido mapa
                                        && linhaValida mapa
                                        && comprimentoValido mapa
                                        && maxTerrenoValido mapa
-- l = largura, t = terreno, o = obstaculo, os = obstaculos, r = restantes linhas.



-- * Funções auxiliares

{-| 
A função __terrenoValido__ é uma função auxiliar, que verifica se não existem obstáculos em terrenos impróprios, e.g. troncos em estradas ou relvas, árvores em rios ou estradas ou carros em rios ou relvas, etc.

A função pode ser definida da seguinte forma:

@
terrenoValido :: Mapa -> Bool
terrenoValido (Mapa l []) = True
terrenoValido (Mapa l (((Rio _),(o:os)):r)) | not(elem Carro (o:os)) && not(elem Arvore (o:os)) = True && terrenoValido (Mapa l r)
                                            | otherwise = False

terrenoValido (Mapa l (((Estrada _),(o:os)):r)) | not(elem Tronco (o:os)) && not(elem Arvore (o:os)) = True && terrenoValido (Mapa l r)
                                                | otherwise = False

terrenoValido (Mapa l ((Relva,(o:os)):r)) | not(elem Tronco (o:os)) && not(elem Carro (o:os)) = True && terrenoValido (Mapa l r)
                                          | otherwise = False
@

== Exemplos de utilização
=== Exemplo 1 - Terreno válido
>>> terrenoValido (Mapa 4 [(Estrada 3, [Carro, Carro, Nenhum, Carro]),(Relva, [Nenhum, Arvore, Nenhum, Arvore]),(Rio 3, [Tronco, Tronco, Nenhum, Tronco])]) 
True


=== Exemplo 2 - Terreno inválido
>>> terrenoValido (Mapa 4 [(Estrada 3, [Carro, Carro, Nenhum, Arvore]),(Relva, [Nenhum, Arvore, Nenhum, Arvore]),(Rio 3, [Tronco, Tronco, Nenhum, Tronco])]) 
False

-} 

terrenoValido :: Mapa -> Bool
terrenoValido (Mapa l []) = True
terrenoValido (Mapa l (((Rio _),(o:os)):r)) | not(elem Carro (o:os)) && not(elem Arvore (o:os)) = True && terrenoValido (Mapa l r)
                                            | otherwise = False

terrenoValido (Mapa l (((Estrada _),(o:os)):r)) | not(elem Tronco (o:os)) && not(elem Arvore (o:os)) = True && terrenoValido (Mapa l r)
                                                | otherwise = False

terrenoValido (Mapa l ((Relva,(o:os)):r)) | not(elem Tronco (o:os)) && not(elem Carro (o:os)) = True && terrenoValido (Mapa l r)
                                          | otherwise = False


{-| 
A função __riosContiguos__ é uma função auxiliar, na qual tem o intuito de verificar se existem 2 rios e se estes possuem direções opostas. Uma vez que ter dois rios contíguos a moverem se para o mesmo lado é uma restrição para o mapa ser válido.

A função pode ser definida da seguinte forma:

@
riosContiguosValido :: Mapa -> Bool
riosContiguosValido (Mapa l []) = True
riosContiguosValido (Mapa l (((Rio x),(o:os)):((Rio y),(o1:os1)):r)) | x * y < 0 = True && riosContiguosValido (Mapa l (((Rio y),(o1:os1)):r))
                                                                     | otherwise = False 
riosContiguosValido (Mapa l ((_,(o:os)):r)) = riosContiguosValido (Mapa l r)
@

== Exemplos de utilização
=== Exemplo 1 - Rios contíguos válido
>>> terrenoValido (Mapa 4 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Nenhum]),(Rio 1, [Tronco, Tronco, Nenhum, Tronco])]) 
True


=== Exemplo 2 - Rios contíguos inválido
>>> terrenoValido (Mapa 4 [(Rio 3, [Tronco, Nenhum, Nenhum, Tronco]),(Rio (-2), [Tronco, Tronco, Nenhum, Nenhum]),(Rio (-1), [Tronco, Tronco, Nenhum, Tronco])]) 
False

-} 

riosContiguosValido :: Mapa -> Bool
riosContiguosValido (Mapa l []) = True
riosContiguosValido (Mapa l (((Rio x),(o:os)):((Rio y),(o1:os1)):r)) | x * y < 0 = True && riosContiguosValido (Mapa l (((Rio y),(o1:os1)):r))
                                                                     | otherwise = False 
riosContiguosValido (Mapa l ((_,(o:os)):r)) = riosContiguosValido (Mapa l r)


{-| 
A função __linhaValida__ é uma função auxiliar, que possui o intuito de verificar se a linha é valida, ou seja, verifica se a linha possui o mesmo número que a largura e se possui pelos menos um “obstáculo” Nenhum, ou seja, uma linha não pode ser composta exclusivamente por obstáculos, precisando de haver pelo menos um espaço livre para o jogador conseguir passar.

A função pode ser definida da seguinte forma:

@
linhaValida :: Mapa -> Bool
linhaValida (Mapa l ((t,(o:os)):r)) | r /= [] && length (o:os) == l && elem Nenhum (o:os) = True && linhaValida (Mapa l r)
                                    | r == [] && length (o:os) == l && elem Nenhum (o:os) = True 
                                    | otherwise = False
linhaValida (Mapa l []) = False
@

== Exemplos de utilização
=== Exemplo 1 - Linha válido
>>> linhaValida (Mapa 3 [(Relva, [Arvore,Nenhum,Nenhum]),(Rio 2,[Tronco,Tronco,Nenhum])])
True

=== Exemplo 2 - Linha inválido
>>>  linhaValida (Mapa 3 [(Relva, [Arvore,Nenhum,Nenhum,Arvore]),(Rio 2,[Tronco,Tronco,Nenhum])])
False
-} 

linhaValida :: Mapa -> Bool
linhaValida (Mapa l ((t,(o:os)):r)) | r /= [] && length (o:os) == l && elem Nenhum (o:os) = True && linhaValida (Mapa l r)
                                    | r == [] && length (o:os) == l && elem Nenhum (o:os) = True 
                                    | otherwise = False
linhaValida (Mapa l []) = False


{-| 
A função __comprimentoValido__ é uma função auxiliar, que tem o intuito de verificar se a linha a possui mais troncos ou carros que o permitido, troncos têm, no máximo, 5 unidades de comprimento e carros têm, no máximo, 3 unidades de comprimento.

A função pode ser definida da seguinte forma:

@
comprimentoValido :: Mapa -> Bool
comprimentoValido (Mapa l ((Estrada x, (o:os)):r)) | (length(last(comprimentoValidoAux(o:os))))+length(head(comprimentoValidoAux (o:os))) > 3 && Carro == o && Carro == (head(last(comprimentoValidoAux(o:os)))) = False
                                                   | Carro == head(head(comprimentoValidoAux(o:os))) && length (head(comprimentoValidoAux(o:os))) > 3 = False
                                                   | Carro == head(last(comprimentoValidoAux(o:os))) && length (last(comprimentoValidoAux(o:os))) > 3 = False
                                                   | r == [] = True 
                                                   | otherwise = True && comprimentoValido (Mapa l r)
comprimentoValido (Mapa l ((Relva,(o:os)):r)) | r == [] = True
                                              | otherwise = True && comprimentoValido (Mapa l r)
comprimentoValido (Mapa l ((Rio x,(o:os)):r)) | Tronco == head(head(comprimentoValidoAux(o:os))) && Tronco == head(last(comprimentoValidoAux(o:os))) && (length(last(comprimentoValidoAux(o:os))))+length(head(comprimentoValidoAux (o:os))) > 5 = False
                                              | Tronco == head(head(comprimentoValidoAux(o:os))) && length (head(comprimentoValidoAux(o:os))) > 5 = False
                                              | Tronco == head(last(comprimentoValidoAux(o:os))) && length (last(comprimentoValidoAux(o:os))) > 5 = False
                                              | r == [] = True 
                                              | otherwise = True && comprimentoValido (Mapa l r)
@

== Exemplos de utilização
=== Exemplo 1 - Linha válida (3 carros)
>>> comprimentoValido (Mapa 5 [(Estrada 2 ,[Carro,Carro,Carro,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum])])
True

=== Exemplo 2 - Linha inválida (4 carros)
>>> comprimentoValido (Mapa 5 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum]),(Estrada 2 ,[Carro,Carro,Carro,Carro,Nenhum])])
False

=== Exemplo 3 - Linha válida (5 troncos)
>>> comprimentoValido (Mapa 7 [(Rio 2 ,[Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum,Tronco]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore])]) 
True

=== Exemplo 4 - Linha inválida (6 troncos)
>>> comprimentoValido (Mapa 7 [(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore]),(Rio 2 ,[Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco])])
False
-} 

comprimentoValido :: Mapa -> Bool
comprimentoValido (Mapa l ((Estrada x, (o:os)):r)) | (length(last(comprimentoValidoAux(o:os))))+length(head(comprimentoValidoAux (o:os))) > 3 && Carro == o && Carro == (head(last(comprimentoValidoAux(o:os)))) = False
                                                   | Carro == head(head(comprimentoValidoAux(o:os))) && length (head(comprimentoValidoAux(o:os))) > 3 = False
                                                   | Carro == head(last(comprimentoValidoAux(o:os))) && length (last(comprimentoValidoAux(o:os))) > 3 = False
                                                   | r == [] = True 
                                                   | otherwise = True && comprimentoValido (Mapa l r)
comprimentoValido (Mapa l ((Relva,(o:os)):r)) | r == [] = True
                                              | otherwise = True && comprimentoValido (Mapa l r)
comprimentoValido (Mapa l ((Rio x,(o:os)):r)) | Tronco == head(head(comprimentoValidoAux(o:os))) && Tronco == head(last(comprimentoValidoAux(o:os))) && (length(last(comprimentoValidoAux(o:os))))+length(head(comprimentoValidoAux (o:os))) > 5 = False
                                              | Tronco == head(head(comprimentoValidoAux(o:os))) && length (head(comprimentoValidoAux(o:os))) > 5 = False
                                              | Tronco == head(last(comprimentoValidoAux(o:os))) && length (last(comprimentoValidoAux(o:os))) > 5 = False
                                              | r == [] = True 
                                              | otherwise = True && comprimentoValido (Mapa l r)


{-| 
A função __comprimentoValidoAux__ é uma função auxiliar da função comprimentoValido, uma função que funciona como a função (pré-definida) group que agrupa elementos iguais e consecutivos de uma lista.

A função pode ser definida da seguinte forma:

@
comprimentoValidoAux :: [Obstaculo] -> [[Obstaculo]]
comprimentoValidoAux [] = []
comprimentoValidoAux [x] = [[x]]
comprimentoValidoAux l = u : comprimentoValidoAux (drop (length u) l)
    where u = uma_lista l
          uma_lista :: Eq a => [a] -> [a]
          uma_lista [x] = [x]
          uma_lista (x:y:t) | x == y = x : uma_lista (y:t)
                            | otherwise = [x] 
@

== Exemplos de utilização
=== Exemplo 1 - Linha válido
>>> comprimentoValidoAux [Tronco,Tronco,Tronco, Nenhum, Nenhum, Tronco, Nenhum]
[[Tronco,Tronco,Tronco],[Nenhum,Nenhum],[Tronco],[Nenhum]]

-}

comprimentoValidoAux :: [Obstaculo] -> [[Obstaculo]]
comprimentoValidoAux [] = []
comprimentoValidoAux [x] = [[x]]
comprimentoValidoAux l = u : comprimentoValidoAux (drop (length u) l)
    where u = uma_lista l
          uma_lista :: Eq a => [a] -> [a]
          uma_lista [x] = [x]
          uma_lista (x:y:t) | x == y = x : uma_lista (y:t)
                            | otherwise = [x] 


{-| 
A função __maxTerrenoValido__ é uma função auxiliar, que verifica se o mapa possui contiguamente, não existe mais do que 4 rios, nem 5 estradas ou relvas.

A função pode ser definida da seguinte forma:

@
maxTerrenoValido :: Mapa -> Bool
maxTerrenoValido (Mapa l ((Rio x,(o:os)):r)) | (1 + maxTerrenoValidoAux (Mapa l ((Rio x,(o:os)):r))) > 4 = False
                                             | otherwise = True && maxTerrenoValido (Mapa l r) 
maxTerrenoValido (Mapa l ((Estrada x,(o:os)):r)) | (1 + maxTerrenoValidoAux (Mapa l ((Estrada x,(o:os)):r))) > 5 = False 
                                                 | otherwise = True && maxTerrenoValido (Mapa l r) 
maxTerrenoValido (Mapa l ((Relva,(o:os)):r)) | (1 + maxTerrenoValidoAux (Mapa l ((Relva,(o:os)):r))) > 5 = False 
                                             | otherwise = True && maxTerrenoValido (Mapa l r) 
maxTerrenoValido (Mapa l []) = True
@

== Exemplos de utilização
=== Exemplo 1 - Terreno válido
>>> maxTerrenoValido (Mapa 2 [(Estrada 1,[Nenhum,Tronco]),(Estrada (-1),[Nenhum,Tronco]),(Estrada 1,[Nenhum,Tronco]),(Estrada (-1),[Nenhum,Tronco]),(Estrada 1,[Nenhum,Tronco])])
True

=== Exemplo 2 - Terreno inválido
>>> maxTerrenoValido (Mapa 2 [(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Nenhum,Tronco]),(Rio (-1),[Nenhum,Tronco]),(Rio 1,[Nenhum,Tronco])])
False

-}
maxTerrenoValido :: Mapa -> Bool
maxTerrenoValido (Mapa l ((Rio x,(o:os)):r)) | (1 + maxTerrenoValidoAux (Mapa l ((Rio x,(o:os)):r))) > 4 = False
                                             | otherwise = True && maxTerrenoValido (Mapa l r) 
maxTerrenoValido (Mapa l ((Estrada x,(o:os)):r)) | (1 + maxTerrenoValidoAux (Mapa l ((Estrada x,(o:os)):r))) > 5 = False 
                                                 | otherwise = True && maxTerrenoValido (Mapa l r) 
maxTerrenoValido (Mapa l ((Relva,(o:os)):r)) | (1 + maxTerrenoValidoAux (Mapa l ((Relva,(o:os)):r))) > 5 = False 
                                             | otherwise = True && maxTerrenoValido (Mapa l r) 
maxTerrenoValido (Mapa l []) = True


{-| 
A função __maxTerrenoValidoAux__ é uma função auxiliar da função maxTerrenoValidoAux, uma função que soma sempre que encontra um terreno igual na linha a seguir.

A função pode ser definida da seguinte forma:

@
maxTerrenoValidoAux :: Mapa -> Int  
maxTerrenoValidoAux (Mapa l []) = 0
maxTerrenoValidoAux (Mapa l ((Rio x,(o:os)):(Rio y,(o1:os1)):r)) = 1 + maxTerrenoValidoAux(Mapa l ((Rio y,(o1:os1)):r))
maxTerrenoValidoAux (Mapa l ((Estrada x,(o:os)):(Estrada y,(o1:os1)):r)) = 1 + maxTerrenoValidoAux(Mapa l ((Estrada y,(o1:os1)):r))
maxTerrenoValidoAux (Mapa l ((Relva,(o:os)):(Relva,(o1:os1)):r)) = 1 + maxTerrenoValidoAux(Mapa l ((Relva,(o1:os1)):r))
maxTerrenoValidoAux (Mapa l ((_,(o:os)):r)) = 0
@

== Exemplos de utilização
=== Exemplo 1 - Linha válido
>>> maxTerrenoValidoAux (Mapa 2 [(Estrada 1,[Nenhum,Tronco]),(Estrada (-1),[Nenhum,Tronco]),(Estrada 1,[Nenhum,Tronco]),(Estrada (-1),[Nenhum,Tronco]),(Estrada 1,[Nenhum,Tronco])])
4

-}
maxTerrenoValidoAux :: Mapa -> Int  
maxTerrenoValidoAux (Mapa l []) = 0
maxTerrenoValidoAux (Mapa l ((Rio x,(o:os)):(Rio y,(o1:os1)):r)) = 1 + maxTerrenoValidoAux(Mapa l ((Rio y,(o1:os1)):r))
maxTerrenoValidoAux (Mapa l ((Estrada x,(o:os)):(Estrada y,(o1:os1)):r)) = 1 + maxTerrenoValidoAux(Mapa l ((Estrada y,(o1:os1)):r))
maxTerrenoValidoAux (Mapa l ((Relva,(o:os)):(Relva,(o1:os1)):r)) = 1 + maxTerrenoValidoAux(Mapa l ((Relva,(o1:os1)):r))
maxTerrenoValidoAux (Mapa l ((_,(o:os)):r)) = 0


{-| 
A função __velocidadeValida__ é uma função auxiliar, que verifica se o mapa possui contiguamente, não existe mais do que 4 rios, nem 5 estradas ou relvas.

A função pode ser definida da seguinte forma:

@
velocidadeValida :: Mapa -> Bool
velocidadeValida (Mapa l []) = True
velocidadeValida (Mapa l ((Estrada x,(o:os)):r)) | x /= 0 = True && velocidadeValida (Mapa l r)
                                                 | otherwise = False 
velocidadeValida (Mapa l ((Rio x,(o:os)):r)) | x /= 0 = True && velocidadeValida (Mapa l r)
                                             | otherwise = True 
velocidadeValida (Mapa l ((Relva,(o:os)):r)) = True && velocidadeValida (Mapa l r)
@

== Exemplos de utilização
=== Exemplo 1 - Terreno com velocidade válida
>>> velocidadeValida (Mapa 2 [(Estrada 1,[Nenhum,Carro]),(Estrada 2,[Nenhum,Carro]),(Rio 1,[Nenhum,Tronco]),(Relva,[Nenhum,Nenhum])])
True

=== Exemplo 2 - Terreno com velocidade inválida
>>> velocidadeValida (Mapa 2 [(Estrada 1,[Nenhum,Carro]),(Estrada 0,[Nenhum,Carro]),(Rio 1,[Nenhum,Tronco]),(Relva,[Nenhum,Nenhum])])
False

-}

velocidadeValida :: Mapa -> Bool
velocidadeValida (Mapa l []) = True
velocidadeValida (Mapa l ((Estrada x,(o:os)):r)) | x /= 0 = True && velocidadeValida (Mapa l r)
                                                 | otherwise = False 
velocidadeValida (Mapa l ((Rio x,(o:os)):r)) | x /= 0 = True && velocidadeValida (Mapa l r)
                                             | otherwise = False 
velocidadeValida (Mapa l ((Relva,(o:os)):r)) = True && velocidadeValida (Mapa l r)