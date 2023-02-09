{- |
Module      : Main
Description : Gloss
Copyright   : Pedro Seabra Vieira <a104352@alunos.uminho.pt>
              Pedro Filipe Maneta Pinto <a104176@alunos.uminho.pt>

Módulo Main, implimentação Gloss, do projeto de LI1 em 2022/23.
-}
module Main where

import LI12223
import Tarefa2_2022li1g011_Gloss
import Tarefa3_2022li1g011_Gloss
import Tarefa4_2022li1g011_Gloss
import Tarefa5_2022li1g011_Gloss

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe

import System.Random
import System.IO.Unsafe  

---------------------------------------------------------------------------------
{-| 
A função 'frames' serve para definir a framerate do jogo, conhecido tambem como a velocidade do jogo.
A função pode ser definida da seguinte forma:

@
frames :: Int
frames = 150
@

-} 
frames :: Int
frames = 150

{-| 
A função 'altura' serve para definir a altura do jogo, ou seja da janela de visualização.
A função pode ser definida da seguinte forma:

@
altura :: Float
altura = 390
@

-} 
altura :: Float
altura = 390

{-| 
A função 'comprimento' serve para definir o comprimento do jogo, ou seja da janela de visualização.
A função pode ser definida da seguinte forma:

@
comprimento :: Float 
comprimento = 0
@

-} 
comprimento :: Float 
comprimento = 0

{-| 
A função 'janela' serve para definir o display do jogo, ou seja da janela de visualização podia ser Fullscreen ou em janela.
A função pode ser definida da seguinte forma:

@
janela :: Display
janela = FullScreen 
@

-} 
janela :: Display
janela = FullScreen 

---------------------------------------------------------------------------------

data Personagens
  = Galinha
  | Camelo
  | BonecoNeve
  deriving (Show, Read, Eq, Enum)

data Botoes
  = JogarB
  | JogarBS
  | SairB
  | SairBS
  | EscolherB
  | EscolherBS
  | DesafioB
  | DesafioBS
  | DesafioD1
  | DesafioD1S
  | DesafioD2
  | DesafioD2S
  | DesafioD3
  | DesafioD3S
  | DireitaB
  | EsquerdaB
  deriving (Show, Read, Eq, Enum)

type Texturas = [(Obstaculo, (Picture, (Float, Float)))]

type Skins = [(Personagens, Picture)]
type Skin = Picture

type BotoesSkins = [(Botoes, Picture)]

type Fundos = [Picture]


type TempoInicial = Float
type TempoMover = Float
type EstadoJogo = Bool -- saber se jogo terminou
type TempoRecorde = Float -- tempo de jogo
type YRecorde = Int
type YAuxRecorde = Int

type EstadoGloss = (Menu, -- tela que vou ver, incial, jogo, esoclher personagens, ou tela perdeu
                    Jogo, -- posicao, mapa...
                    Texturas, -- imagens dos objetos
                    Texturas, -- imagens dos objetos que vamos usar no jogo
                    Skins, -- diferentes tipos de personagens
                    Skin, -- personagem que vamos usar no jogo
                    Fundos, -- diferentes fundos
                    BotoesSkins, -- botoes da tela incial e tela escolher personagens
                    TempoInicial, -- tempo em que o jogo vai começar, é utilizada para controlarmos o tempo reage
                    TempoRecorde, -- tempo para ver quanto tempo jogamos
                    TempoMover, -- tempo para controlar o mover do mapa
                    EstadoJogo, -- para saber se o jogo acabou ou nao
                    YRecorde, -- recorde y
                    YAuxRecorde) -- auxiliar para o recorde quando o mapa desliza

data Opcao = Jogar
            | Sair
            | Escolher
            | Desafio
            | P
            | P1
            | P2
            | D
            | D1
            | D2

data Menu = Opcoes Opcao
          | InGame 
          | PerdeuJogo
          | GanhouJogo
          | MenuEscolher Opcao
          | ModoDesafio
          | MenuDesafio Opcao


---------------------------------------------------------------------------------
{-| 
A função 'l' serve para definir a distancia entre imagens para nao ficarem sobrepostas.
A função pode ser definida da seguinte forma:

@
l :: Float
l = 60
@

-} 
l :: Float 
l = 60

{-| 
A função 'estadoGlossInicial' é a função que desenha o estado inicial.
A função pode ser definida da seguinte forma:

@
estadoGlossInicial :: Texturas -> Skins -> Fundos -> BotoesSkins -> EstadoGloss
estadoGlossInicial texturas skins fundos botoes = (Opcoes Jogar, (Jogo (Jogador (5,(-12)))  (completarMapaLinhas mapaFinal)), texturas, (texturasNormal texturas), skins, skin, fundos, botoes, 0, 0, 0, False, 0, 0)
  where skin = snd(skins!!0)
@

-} 
estadoGlossInicial :: Texturas -> Skins -> Fundos -> BotoesSkins -> EstadoGloss
estadoGlossInicial texturas skins fundos botoes = (Opcoes Jogar, (Jogo (Jogador (5,(-12)))  (completarMapaLinhas mapaFinal)), texturas, (texturasNormal texturas), skins, skin, fundos, botoes, 0, 0, 0, False, 0, 0)
  where skin = snd(skins!!0)


-------------------------
{-| 
A função 'texturasNormal' é a função que "dá" as texturas usado no mapa quando o personagem escolhido é a __Galinha__.
A função pode ser definida da seguinte forma:

@
texturasNormal :: Texturas -> Texturas
texturasNormal texturas = take 8 texturas
@

-} 
texturasNormal :: Texturas -> Texturas
texturasNormal texturas = take 8 texturas

{-| 
A função 'texturasDeserto' é a função que "dá" as texturas usado no mapa quando o personagem escolhido é a __Camelo__.
A função pode ser definida da seguinte forma:

@
texturasDeserto :: Texturas -> Texturas
texturasDeserto texturas = take 8 (drop 8 texturas)
@

-} 
texturasDeserto :: Texturas -> Texturas
texturasDeserto texturas = take 8 (drop 8 texturas)

{-| 
A função 'texturasNatal' é a função que "dá" as texturas usado no mapa quando o personagem escolhido é a __Boneco de Neve__.
A função pode ser definida da seguinte forma:

@
texturasNatal :: Texturas -> Texturas
texturasNatal texturas = take 8 (drop 16 texturas)
@

-} 
texturasNatal :: Texturas -> Texturas
texturasNatal texturas = take 8 (drop 16 texturas)

---------------------------------------------------------------------------------
{-| 
A função 'reageTempoGloss' é a função que reage enquanto o tempo passa que é um Int dado pelo acumulador __xTempo__.
A função pode ser definida da seguinte forma:

@
reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss xTempo (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = if estadoJogo == True
                                                                                                                                                                  then (PerdeuJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
                                                                                                                                                                  else if tempoJogo > 5
                                                                                                                                                                       then if tempoMover > 2 
                                                                                                                                                                            then (InGame, ((deslizaJogo (round xTempo)  (jogadorMove (Jogo (Jogador (x,y))  mapa) (Parado)))), texturas, textura, skins, skin, fundos, botoes, 0, tempoFinal, 0, (jogoTerminou jogo), yRecorde, yAux+1)
                                                                                                                                                                            else if tempoAcumulador > 0.75
                                                                                                                                                                                 then (InGame, (((jogadorMove (Jogo (Jogador (x,y))  mapa) (Parado)))), texturas, textura, skins, skin, fundos, botoes, 0, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                                                 else (InGame,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                                       else if tempoAcumulador > 0.75
                                                                                                                                                                           then (InGame, (((jogadorMove (Jogo (Jogador (x,y))  mapa) (Parado)))), texturas, textura, skins, skin, fundos, botoes, 0, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                                           else (InGame,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                     where tempoAcumulador = xTempo + tempoInicial
                                                                                                                                                           tempoFinal = tempoJogo + xTempo
                                                                                                                                                           tempoFMover = tempoMover + xTempo

reageTempoGloss xTempo (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = if y == 0
                                                                                                                                                                           then (GanhouJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
                                                                                                                                                                           else if estadoJogo == True
                                                                                                                                                                                then (PerdeuJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
                                                                                                                                                                                else if tempoAcumulador > 0.75
                                                                                                                                                                                     then (ModoDesafio, (((jogadorMove (Jogo (Jogador (x,y))  mapa) (Parado)))), texturas, textura, skins, skin, fundos, botoes, 0, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                                                     else (ModoDesafio,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                     where tempoAcumulador = xTempo + tempoInicial
                                                                                                                                                           tempoFinal = tempoJogo + xTempo
                                                                                                                                                           tempoFMover = tempoMover + xTempo

reageTempoGloss _ estado = estado
@

-}

reageTempoGloss :: Float -> EstadoGloss -> EstadoGloss
reageTempoGloss xTempo (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = if estadoJogo == True
                                                                                                                                                                  then (PerdeuJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
                                                                                                                                                                  else if tempoJogo > 5
                                                                                                                                                                       then if tempoMover > 2 
                                                                                                                                                                            then (InGame, ((deslizaJogo (round xTempo)  (jogadorMove (Jogo (Jogador (x,y))  mapa) (Parado)))), texturas, textura, skins, skin, fundos, botoes, 0, tempoFinal, 0, (jogoTerminou jogo), yRecorde, yAux+1)
                                                                                                                                                                            else if tempoAcumulador > 0.75
                                                                                                                                                                                 then (InGame, (((jogadorMove (Jogo (Jogador (x,y))  mapa) (Parado)))), texturas, textura, skins, skin, fundos, botoes, 0, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                                                 else (InGame,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                                       else if tempoAcumulador > 0.75
                                                                                                                                                                           then (InGame, (((jogadorMove (Jogo (Jogador (x,y))  mapa) (Parado)))), texturas, textura, skins, skin, fundos, botoes, 0, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                                           else (InGame,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                     where tempoAcumulador = xTempo + tempoInicial
                                                                                                                                                           tempoFinal = tempoJogo + xTempo
                                                                                                                                                           tempoFMover = tempoMover + xTempo

reageTempoGloss xTempo (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = if y == 0
                                                                                                                                                                           then (GanhouJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
                                                                                                                                                                           else if estadoJogo == True
                                                                                                                                                                                then (PerdeuJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
                                                                                                                                                                                else if tempoAcumulador > 0.75
                                                                                                                                                                                     then (ModoDesafio, (((jogadorMove (Jogo (Jogador (x,y))  mapa) (Parado)))), texturas, textura, skins, skin, fundos, botoes, 0, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                                                     else (ModoDesafio,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoAcumulador, tempoFinal, tempoFMover, (jogoTerminou jogo), yRecorde, yAux)
                                                                                                                                                     where tempoAcumulador = xTempo + tempoInicial
                                                                                                                                                           tempoFinal = tempoJogo + xTempo
                                                                                                                                                           tempoFMover = tempoMover + xTempo

reageTempoGloss _ estado = estado

---------------------------------------------------------------------------------
{-| 
A função 'desenharMapa' é a função que desenha um mapa como o nome diz.
A função pode ser definida da seguinte forma:

@
desenharMapa :: Float -> Float -> [[Obstaculo]] -> Texturas -> [Picture]
desenharMapa x y (o:os) texturas = linha ++ resto
  where linha = desenharLinha x y o texturas
        resto = desenharMapa x (y-l) os texturas
desenharMapa _ _ _ _ = []
@

-} 

desenharMapa :: Float -> Float -> [[Obstaculo]] -> Texturas -> [Picture]
desenharMapa x y (o:os) texturas = linha ++ resto
  where linha = desenharLinha x y o texturas
        resto = desenharMapa x (y-l) os texturas
desenharMapa _ _ _ _ = []

{-| 
A função 'desenharLinha' é a função que desenha uma linha do mapa como o nome diz, função auxiliar da função __desenharMapa__.
A função pode ser definida da seguinte forma:

@
desenharLinha :: Float -> Float -> [Obstaculo] -> Texturas -> [Picture]
desenharLinha x y (o:os) texturas = obstaculo : resto
  where obstaculo = (desenharObstaculo x y o texturas)
        resto     = desenharLinha (x+l) y os texturas
desenharLinha _ _ _ _ = []
@

-} 
desenharLinha :: Float -> Float -> [Obstaculo] -> Texturas -> [Picture]
desenharLinha x y (o:os) texturas = obstaculo : resto
  where obstaculo = (desenharObstaculo x y o texturas)
        resto     = desenharLinha (x+l) y os texturas
desenharLinha _ _ _ _ = []

{-| 
A função 'desenharObstaculo' é a função que desenha os obstáculos do mapa como o nome diz, função auxiliar da função __desenharObstaculo__.
A função pode ser definida da seguinte forma:

@
desenharObstaculo :: Float -> Float -> Obstaculo -> Texturas -> Picture
desenharObstaculo x y obstaculo texturas = Translate realX realY textura
  where tupulo  = (fromJust . lookup obstaculo) texturas
        textura = fst tupulo
        realX   = ((+x) . fst . snd) tupulo
        realY   = ((+y) . snd . snd) tupulo
@

-} 
desenharObstaculo :: Float -> Float -> Obstaculo -> Texturas -> Picture
desenharObstaculo x y obstaculo texturas = Translate realX realY textura
  where tupulo  = (fromJust . lookup obstaculo) texturas
        textura = fst tupulo
        realX   = ((+x) . fst . snd) tupulo
        realY   = ((+y) . snd . snd) tupulo
{-| 
A função 'desenharEstadoGloss' é a função que atribui as "imagens" escolhidas aos determinados obstaculos,fundos etc..
A função pode ser definida da seguinte forma:

@
desenharEstadoGloss :: EstadoGloss -> Picture
desenharEstadoGloss (PerdeuJogo, _, _, _, _, _, fundos, _, _, tempoJogo,_, _, yRecorde, _) = Pictures ([(fundos!!3)] ++ [Translate (-250) 0 $ Color red $ scale 0.4 0.4 $ Text  ("Duraste " ++ show (round tempoJogo) ++ " segundos")] ++ [Translate (-250) (-75) $ Color red $ scale 0.4 0.4 $ Text  ("Andaste " ++ (show yRecorde) ++ " blocos")])
desenharEstadoGloss (Opcoes Jogar, _, _, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(fundos!!1)] ++ [Translate 0 200 ((fundos!!2))] ++ (desenharBotoesJogarS botoes))
desenharEstadoGloss (Opcoes Escolher, _, _, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(fundos!!1)] ++ [Translate 0 200 ((fundos!!2))] ++ (desenharBotoesES botoes))
desenharEstadoGloss (Opcoes Desafio, _, _, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(fundos!!1)] ++ [Translate 0 200 ((fundos!!2))] ++ (desenharBotoesDS botoes))
desenharEstadoGloss (Opcoes Sair, _, _, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(fundos!!1)] ++ [Translate 0 200 ((fundos!!2))] ++ (desenharBotoesSairS botoes))


desenharEstadoGloss (MenuEscolher P, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(desenharEscolher skins)] ++ (desenharBotoesDSair botoes) ++ [Translate (-300) (-150) $ scale 0.2 0.2 $ Text  ("Para selecionar a personagem clique ENTER")])
desenharEstadoGloss (MenuEscolher P1, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(desenharEscolher1 skins)] ++ (desenharBotoesEDSair botoes) ++ [Translate (-300) (-150) $ scale 0.2 0.2 $ Text  ("Para selecionar a personagem clique ENTER")])
desenharEstadoGloss (MenuEscolher P2, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(desenharEscolher2 skins)] ++ (desenharBotoesESair botoes) ++ [Translate (-300) (-150) $ scale 0.2 0.2 $ Text  ("Para selecionar a personagem clique ENTER")])
desenharEstadoGloss (MenuEscolher Sair, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ((desenharBotoesEDSairS botoes) ++ (desenharEscolherT skins) ++ [Translate (-475) (-300) $ scale 0.15 0.15 $ Text  ("Para voltar para escolher as personagens clique na SETA para CIMA se pretende sair clique ENTER")])


desenharEstadoGloss (MenuDesafio D, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ((desenharDesafiosD1S botoes) ++ [desenharBotaoSair botoes] ++ [Translate (-250) (-150) $ scale 0.2 0.2 $ Text ("Para entrar no desafio clique ENTER")])
desenharEstadoGloss (MenuDesafio D1, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ((desenharDesafiosD2S botoes) ++ [desenharBotaoSair botoes] ++ [Translate (-250) (-150) $ scale 0.2 0.2 $ Text ("Para entrar no desafio clique ENTER")])
desenharEstadoGloss (MenuDesafio D2, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ((desenharDesafiosD3S botoes) ++ [desenharBotaoSair botoes] ++ [Translate (-250) (-150) $ scale 0.2 0.2 $ Text ("Para entrar no desafio clique ENTER")])
desenharEstadoGloss (MenuDesafio Sair, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([desenharBotaoSairS botoes] ++ (desenharDesafios botoes) ++ [Translate (-400) (-275) $ scale 0.15 0.15 $ Text  ("Para escolher o desafio clique na SETA para CIMA se pretende sair clique ENTER")])

desenharEstadoGloss (GanhouJogo, _, _, _, _, _, fundos, _, _, tempoJogo, _, _, _, _) = Pictures ([(fundos!!1)] ++ [Translate (-250) 0 $ Color red $ scale 0.3 0.3 $ Text  ("Ganhaste em " ++ show (round tempoJogo) ++ " segundos.")] ++ [Translate (-300) (-50) $ scale 0.2 0.2 $ Text  ("E possivel completar o desafio em 5 segundos.")])

desenharEstadoGloss (ModoDesafio, (Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = Pictures (desenho ++  [Translate (-200) 110 $ scale 0.5 0.5 $ Text  (show (round tempoJogo))] ++ [Translate (-175) (-30) $ scale 0.5 0.5 $ Text  (show (yRecorde))])
        where desenho        =  [(fundos!!0)] ++ desenhoDoMapa ++ [desenhoJogador]
              desenhoDoMapa  = desenharMapa comprimento altura (mapaParaObstaculos  mapa) textura 
              desenhoJogador = desenharPlayer (Jogador (x,y)) skin

desenharEstadoGloss (InGame, (Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = Pictures (desenho ++  [Translate (-200) 110 $ scale 0.5 0.5 $ Text  (show (round tempoJogo))] ++  [Translate (-175) (-30) $ scale 0.5 0.5 $ Text  (show (yRecorde))])
        where desenho        =  [(fundos!!0)] ++ desenhoDoMapa ++ [desenhoJogador]
              desenhoDoMapa  = desenharMapa comprimento altura (mapaParaObstaculos  mapa) textura 
              desenhoJogador = desenharPlayer (Jogador (x,y)) skin
@

-} 
desenharEstadoGloss :: EstadoGloss -> Picture
desenharEstadoGloss (PerdeuJogo, _, _, _, _, _, fundos, _, _, tempoJogo,_, _, yRecorde, _) = Pictures ([(fundos!!3)] ++ [Translate (-250) 0 $ Color red $ scale 0.4 0.4 $ Text  ("Duraste " ++ show (round tempoJogo) ++ " segundos")] ++ [Translate (-250) (-75) $ Color red $ scale 0.4 0.4 $ Text  ("Andaste " ++ (show yRecorde) ++ " blocos")])
desenharEstadoGloss (Opcoes Jogar, _, _, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(fundos!!1)] ++ [Translate 0 200 ((fundos!!2))] ++ (desenharBotoesJogarS botoes))
desenharEstadoGloss (Opcoes Escolher, _, _, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(fundos!!1)] ++ [Translate 0 200 ((fundos!!2))] ++ (desenharBotoesES botoes))
desenharEstadoGloss (Opcoes Desafio, _, _, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(fundos!!1)] ++ [Translate 0 200 ((fundos!!2))] ++ (desenharBotoesDS botoes))
desenharEstadoGloss (Opcoes Sair, _, _, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(fundos!!1)] ++ [Translate 0 200 ((fundos!!2))] ++ (desenharBotoesSairS botoes))


desenharEstadoGloss (MenuEscolher P, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(desenharEscolher skins)] ++ (desenharBotoesDSair botoes) ++ [Translate (-300) (-150) $ scale 0.2 0.2 $ Text  ("Para selecionar a personagem clique ENTER")])
desenharEstadoGloss (MenuEscolher P1, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(desenharEscolher1 skins)] ++ (desenharBotoesEDSair botoes) ++ [Translate (-300) (-150) $ scale 0.2 0.2 $ Text  ("Para selecionar a personagem clique ENTER")])
desenharEstadoGloss (MenuEscolher P2, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([(desenharEscolher2 skins)] ++ (desenharBotoesESair botoes) ++ [Translate (-300) (-150) $ scale 0.2 0.2 $ Text  ("Para selecionar a personagem clique ENTER")])
desenharEstadoGloss (MenuEscolher Sair, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ((desenharBotoesEDSairS botoes) ++ (desenharEscolherT skins) ++ [Translate (-475) (-300) $ scale 0.15 0.15 $ Text  ("Para voltar para escolher as personagens clique na SETA para CIMA se pretende sair clique ENTER")])


desenharEstadoGloss (MenuDesafio D, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ((desenharDesafiosD1S botoes) ++ [desenharBotaoSair botoes] ++ [Translate (-250) (-150) $ scale 0.2 0.2 $ Text ("Para entrar no desafio clique ENTER")])
desenharEstadoGloss (MenuDesafio D1, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ((desenharDesafiosD2S botoes) ++ [desenharBotaoSair botoes] ++ [Translate (-250) (-150) $ scale 0.2 0.2 $ Text ("Para entrar no desafio clique ENTER")])
desenharEstadoGloss (MenuDesafio D2, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ((desenharDesafiosD3S botoes) ++ [desenharBotaoSair botoes] ++ [Translate (-250) (-150) $ scale 0.2 0.2 $ Text ("Para entrar no desafio clique ENTER")])
desenharEstadoGloss (MenuDesafio Sair, _, texturas, _, skins, skin, fundos, botoes, _, _, _, _, _, _) = Pictures ([desenharBotaoSairS botoes] ++ (desenharDesafios botoes) ++ [Translate (-400) (-275) $ scale 0.15 0.15 $ Text  ("Para escolher o desafio clique na SETA para CIMA se pretende sair clique ENTER")])

desenharEstadoGloss (GanhouJogo, _, _, _, _, _, fundos, _, _, tempoJogo, _, _, _, _) = Pictures ([(fundos!!1)] ++ [Translate (-250) 0 $ Color red $ scale 0.3 0.3 $ Text  ("Ganhaste em " ++ show (round tempoJogo) ++ " segundos.")] ++ [Translate (-300) (-50) $ scale 0.2 0.2 $ Text  ("E possivel completar o desafio em 5 segundos.")])

desenharEstadoGloss (ModoDesafio, (Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = Pictures (desenho ++  [Translate (-200) 110 $ scale 0.5 0.5 $ Text  (show (round tempoJogo))] ++ [Translate (-175) (-30) $ scale 0.5 0.5 $ Text  (show (yRecorde))])
        where desenho        =  [(fundos!!0)] ++ desenhoDoMapa ++ [desenhoJogador]
              desenhoDoMapa  = desenharMapa comprimento altura (mapaParaObstaculos  mapa) textura 
              desenhoJogador = desenharPlayer (Jogador (x,y)) skin

desenharEstadoGloss (InGame, (Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = Pictures (desenho ++  [Translate (-200) 110 $ scale 0.5 0.5 $ Text  (show (round tempoJogo))] ++  [Translate (-175) (-30) $ scale 0.5 0.5 $ Text  (show (yRecorde))])
        where desenho        =  [(fundos!!0)] ++ desenhoDoMapa ++ [desenhoJogador]
              desenhoDoMapa  = desenharMapa comprimento altura (mapaParaObstaculos  mapa) textura 
              desenhoJogador = desenharPlayer (Jogador (x,y)) skin

{-| 
A função 'mapaParaObstaculos' é uma função auxiliar para a criação de um mapa, que recebe um mapa e devolve os obstaculos desse mapa.
A função pode ser definida da seguinte forma:

@
mapaParaObstaculos :: Mapa -> [[Obstaculo]]
mapaParaObstaculos (Mapa largura []) = []
mapaParaObstaculos (Mapa largura ((_, obs):t)) = obs : mapaParaObstaculos (Mapa largura t)
@

-} 
mapaParaObstaculos :: Mapa -> [[Obstaculo]]
mapaParaObstaculos (Mapa largura []) = []
mapaParaObstaculos (Mapa largura ((_, obs):t)) = obs : mapaParaObstaculos (Mapa largura t)

--------------------------------------------------------------------------------------------------------
{-| 
A função 'desenharPlayer' é uma função que desenha o jogador.
A função pode ser definida da seguinte forma:

@
desenharPlayer :: Jogador -> Skin -> Picture
desenharPlayer (Jogador (x,y)) skin = (Translate (posicaoX x) (posicaoY y) skin)
@

-} 
desenharPlayer :: Jogador -> Skin -> Picture
desenharPlayer (Jogador (x,y)) skin = (Translate (posicaoX x) (posicaoY y) skin)

{-| 
A função 'desenharPlayer' é uma função que dá a posição no eixo dos X do jogador.
A função pode ser definida da seguinte forma:

@
posicaoX :: Int -> Float
posicaoX = (+ comprimento) . (*l) . realToFrac
@

-} 
posicaoX :: Int -> Float
posicaoX = (+ comprimento) . (*l) . realToFrac

{-| 
A função 'posicaoY' é uma função que dá a posição no eixo dos y do jogador.
A função pode ser definida da seguinte forma:

@
posicaoY :: Int -> Float
posicaoY = (+ altura) . (*l) . realToFrac 
@

-} 
posicaoY :: Int -> Float
posicaoY = (+ altura) . (*l) . realToFrac 
--------------------------------------------------------------------------------------------------------
{-| 
A função 'desenharBotaoJogar' desenha o botão jogar.
A função pode ser definida da seguinte forma:

@
desenharBotaoJogar :: BotoesSkins -> Picture
desenharBotaoJogar botoesSkins = (Translate 0 0 image)
  where image = (fromJust . lookup JogarB) botoesSkins
@

-} 
desenharBotaoJogar :: BotoesSkins -> Picture
desenharBotaoJogar botoesSkins = (Translate 0 0 image)
  where image = (fromJust . lookup JogarB) botoesSkins

desenharBotaoJogarS :: BotoesSkins -> Picture
desenharBotaoJogarS botoesSkins = (Translate 0 0 image)
  where image = (fromJust . lookup JogarBS) botoesSkins
------------
{-| 
A função 'desenharBotaoEscolher' desenha o botão escolher.
A função pode ser definida da seguinte forma:

@
desenharBotaoEscolher :: BotoesSkins -> Picture
desenharBotaoEscolher botoesSkins = (Translate 0 (-110) image)
  where image = (fromJust . lookup EscolherB) botoesSkins
@

-} 
desenharBotaoEscolher :: BotoesSkins -> Picture
desenharBotaoEscolher botoesSkins = (Translate 0 (-110) image)
  where image = (fromJust . lookup EscolherB) botoesSkins

desenharBotaoEscolherS :: BotoesSkins -> Picture
desenharBotaoEscolherS botoesSkins = (Translate 0 (-110) image)
  where image = (fromJust . lookup EscolherBS) botoesSkins
--------------
{-| 
A função 'desenharBotaoDesafio' desenha o botão desafio.
A função pode ser definida da seguinte forma:

@
desenharBotaoDesafio :: BotoesSkins -> Picture
desenharBotaoDesafio botoesSkins = (Translate 0 (-220) image)
  where image = (fromJust . lookup DesafioB) botoesSkins
@

-} 
desenharBotaoDesafio :: BotoesSkins -> Picture
desenharBotaoDesafio botoesSkins = (Translate 0 (-220) image)
  where image = (fromJust . lookup DesafioB) botoesSkins

desenharBotaoDesafioS :: BotoesSkins -> Picture
desenharBotaoDesafioS botoesSkins = (Translate 0 (-220) image)
  where image = (fromJust . lookup DesafioBS) botoesSkins
---------
{-| 
A função 'desenharBotaoSair' desenha o botão Sair.
A função pode ser definida da seguinte forma:

@
desenharBotaoSair :: BotoesSkins -> Picture
desenharBotaoSair botoesSkins = (Translate 0 (-330) image)
  where image = (fromJust . lookup SairB) botoesSkins
@

-} 
desenharBotaoSair :: BotoesSkins -> Picture
desenharBotaoSair botoesSkins = (Translate 0 (-330) image)
  where image = (fromJust . lookup SairB) botoesSkins

desenharBotaoSairS :: BotoesSkins -> Picture
desenharBotaoSairS botoesSkins = (Translate 0 (-330) image)
  where image = (fromJust . lookup SairBS) botoesSkins
------------
{-| 
A função 'desenharBotaoDireita' desenha o botão Direita.
A função pode ser definida da seguinte forma:

@
desenharBotaoDireita :: BotoesSkins -> Picture
desenharBotaoDireita botoesSkins = (Translate 200 0 image)
  where image = (fromJust . lookup DireitaB) botoesSkins
@

-} 
desenharBotaoDireita :: BotoesSkins -> Picture
desenharBotaoDireita botoesSkins = (Translate 200 0 image)
  where image = (fromJust . lookup DireitaB) botoesSkins
{-| 
A função 'desenharBotaoEsquerda' desenha o botão Esquerda.
A função pode ser definida da seguinte forma:

@
desenharBotaoEsquerda :: BotoesSkins -> Picture
desenharBotaoEsquerda botoesSkins = (Translate (-200) 0 image)
  where image = (fromJust . lookup EsquerdaB) botoesSkins
@

-} 
desenharBotaoEsquerda :: BotoesSkins -> Picture
desenharBotaoEsquerda botoesSkins = (Translate (-200) 0 image)
  where image = (fromJust . lookup EsquerdaB) botoesSkins
------------
{-| 
__Estas funções seguidas servem apenas para atribuir imagens  e de desenhar os botões do jogo.__

-} 

desenharBotoesJogarS :: BotoesSkins -> [Picture]
desenharBotoesJogarS botoes = [desenharBotaoJogarS botoes] ++ [desenharBotaoSair botoes] ++ [desenharBotaoEscolher botoes] ++ [desenharBotaoDesafio botoes] 

desenharBotoesES :: BotoesSkins -> [Picture] -- botao escolher selecionado desenhado
desenharBotoesES botoes = [desenharBotaoJogar botoes] ++ [desenharBotaoSair botoes] ++ [desenharBotaoEscolherS botoes] ++ [desenharBotaoDesafio botoes] 

desenharBotoesDS :: BotoesSkins -> [Picture] -- botao desafio selecionado desenhado
desenharBotoesDS botoes = [desenharBotaoJogar botoes] ++ [desenharBotaoSair botoes] ++ [desenharBotaoEscolher botoes] ++ [desenharBotaoDesafioS botoes] 

desenharBotoesSairS :: BotoesSkins -> [Picture]
desenharBotoesSairS botoes = [desenharBotaoJogar botoes] ++ [desenharBotaoSairS botoes] ++ [desenharBotaoEscolher botoes] ++ [desenharBotaoDesafio botoes] 

------ esquerda e direita
desenharBotoesEDSair :: BotoesSkins -> [Picture]
desenharBotoesEDSair botoes = [desenharBotaoDireita botoes] ++ [desenharBotaoEsquerda botoes] ++ [(Translate 0 (-40) (desenharBotaoSair botoes))]
--- direita
desenharBotoesDSair :: BotoesSkins -> [Picture]
desenharBotoesDSair botoes = [desenharBotaoDireita botoes] ++ [(Translate 0 (-40) (desenharBotaoSair botoes))]
-- esquerda
desenharBotoesESair :: BotoesSkins -> [Picture]
desenharBotoesESair botoes = [desenharBotaoEsquerda botoes] ++ [(Translate 0 (-40) (desenharBotaoSair botoes))]
--- sair
desenharBotoesEDSairS :: BotoesSkins -> [Picture]
desenharBotoesEDSairS botoes = [(Translate 0 (-40) (desenharBotaoSairS botoes))]

desenharEscolherT :: Skins -> [Picture]
desenharEscolherT skins = ([Translate (-300) 0 (desenharEscolher skins)] ++ [(desenharEscolher1 skins)] ++ [Translate (300) 0 (desenharEscolher2 skins)])

------------
desenharD1 :: BotoesSkins -> Picture
desenharD1 botoesSkins = (Translate (-300) 0 image)
  where image = (fromJust . lookup DesafioD1) botoesSkins

desenharD1S :: BotoesSkins -> Picture
desenharD1S botoesSkins = (Translate (-300) 0 image)
  where image = (fromJust . lookup DesafioD1S) botoesSkins

desenharD2 :: BotoesSkins -> Picture
desenharD2 botoesSkins = (Translate 0 0 image)
  where image = (fromJust . lookup DesafioD2) botoesSkins

desenharD2S :: BotoesSkins -> Picture
desenharD2S botoesSkins = (Translate 0 0 image)
  where image = (fromJust . lookup DesafioD2S) botoesSkins

desenharD3 :: BotoesSkins -> Picture
desenharD3 botoesSkins = (Translate 300 0 image)
  where image = (fromJust . lookup DesafioD3) botoesSkins

desenharD3S :: BotoesSkins -> Picture
desenharD3S botoesSkins = (Translate 300 0 image)
  where image = (fromJust . lookup DesafioD3S) botoesSkins

desenharDesafiosD1S :: BotoesSkins -> [Picture]
desenharDesafiosD1S desafios = [desenharD1S desafios] ++ [desenharD2 desafios] ++ [desenharD3 desafios]

desenharDesafiosD2S :: BotoesSkins -> [Picture]
desenharDesafiosD2S desafios = [desenharD1 desafios] ++ [desenharD2S desafios] ++ [desenharD3 desafios]

desenharDesafiosD3S :: BotoesSkins -> [Picture]
desenharDesafiosD3S desafios = [desenharD1 desafios] ++ [desenharD2 desafios] ++ [desenharD3S desafios]

desenharDesafios :: BotoesSkins -> [Picture]
desenharDesafios desafios = [desenharD1 desafios] ++ [desenharD2 desafios] ++ [desenharD3 desafios]
---------------------------------------------------------------------------------------------------------

desenharEscolher :: Skins -> Picture
desenharEscolher skins = (scale 2 2 image)
  where image = (fromJust . lookup Galinha) skins

desenharEscolher1 :: Skins -> Picture
desenharEscolher1 skins = (scale 2 2 image)
  where image = (fromJust . lookup Camelo) skins

desenharEscolher2 ::  Skins -> Picture
desenharEscolher2 skins = (scale 2 2 image)
  where image = (fromJust . lookup BonecoNeve) skins
--------------------------------------------------------------------------------------------------------
{-| 
A função 'reageEventoGloss' é a função que recebe outputs do jogador como comandos através do teclado e aplica no jogo.
A função pode ser definida da seguinte forma:

@
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
----- TELA INCIAL
-- BOTAO JOGAR
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Sair, jogo, texturas, textura, skins, skin,fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Escolher, jogo, texturas, textura, skins, skin,fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (InGame, jogo, texturas, textura, skins, snd(head(skins)), fundos, botoes, 0, 0, 0, False, 0, 0)

-- BOTAO ESCOLHER
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Escolher, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, 0, 0, 0, False, 0, 0)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Escolher, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Desafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Escolher, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher P1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)

-- BOTAO DESAFIO
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Desafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Escolher, jogo, texturas, textura, skins, skin, fundos, botoes, 0, 0, 0, False, 0, 0)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Desafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Desafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)


-- BOTAO SAIR
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Desafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, _, _, _, _, _, _, _, _, _, _, _, yRecorde, _) = error "FIM"


------ TELA PERDEU
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, _, texturas, textura, skins, _, fundos, botoes, _, _, _, _, _, _) = estadoGlossInicial texturas skins fundos botoes

---- Tela Ganhou
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (GanhouJogo, _, texturas, textura, skins, _, fundos, botoes, _, _, _, _, _, _) = estadoGlossInicial texturas skins fundos botoes
        
-- ver melhor

---- TELA ESCOLHER Personagemn
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuEscolher P, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher Sair,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuEscolher P, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuEscolher P1,   jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuEscolher P, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (InGame, (Jogo (Jogador (5,(-12)))  (completarMapaLinhas mapaFinal)), texturas, (texturasNormal texturas), skins, snd(skins !! 0), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuEscolher P1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuEscolher P1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher P,    jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuEscolher P1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuEscolher P2,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuEscolher P1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (InGame, (Jogo (Jogador (5,(-12)))  (completarMapaLinhas mapaFinal)), texturas, (texturasDeserto texturas), skins, snd(skins !! 1), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuEscolher P2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuEscolher P2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher P1,   jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuEscolher P2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (InGame, (Jogo (Jogador (5,(-12)))  (completarMapaLinhas mapaFinal)), texturas, (texturasNatal texturas), skins, snd(skins !! 2), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (MenuEscolher Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher P1,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuEscolher Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Jogar,     jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)


---- TELA DESAFIO
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuDesafio D, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuDesafio Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuDesafio D, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuDesafio D1,   jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuDesafio D, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (ModoDesafio, (Jogo (Jogador (5,(-13)))  desafio1), texturas, (texturasNormal texturas), skins, snd(skins !! 0), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuDesafio Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuDesafio D, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuDesafio D2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (ModoDesafio, (Jogo (Jogador (5,(-13)))  desafio2), texturas, (texturasDeserto texturas), skins, snd(skins !! 1), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuDesafio D2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuDesafio Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuDesafio D2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuDesafio D2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (ModoDesafio, (Jogo (Jogador (5,(-13)))  desafio3), texturas, (texturasNatal texturas), skins, snd(skins !! 2), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (MenuDesafio Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuDesafio D1,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuDesafio Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)


---- Tela Mododesafio  
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =(ModoDesafio, (jogadorMove jogo (Move Cima)), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+13+1)), yAux)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (ModoDesafio, (jogadorMove jogo (Move Cima)), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+13+1)), yAux)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) | y == -13 = (PerdeuJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
                                                                                                                                                                                                                     | otherwise = (ModoDesafio, jogadorMove jogo (Move Baixo), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (ModoDesafio, jogadorMove jogo (Move Esquerda), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (ModoDesafio, jogadorMove jogo (Move Direita), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)

reageEventoGloss _ (ModoDesafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (ModoDesafio , jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, (jogoTerminou jogo), yRecorde, yAux) 



----- TELA JOGO
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) | (y+2) == 0 = (InGame, (deslizaJogo (round tempoInicial) (jogadorMove jogo (Move Cima))), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+12+1)), yAux+1)
                                                                                                                                                                                                                 | otherwise = (InGame, (jogadorMove jogo (Move Cima)), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+12+1)), yAux)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) | (y+2) == 0 = (InGame, (deslizaJogo (round tempoInicial) (jogadorMove jogo (Move Cima))), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+12+1)), yAux+1)
                                                                                                                                                                                                              | otherwise = (InGame, (jogadorMove jogo (Move Cima)), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+12+1)), yAux)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) | y == -13 = (PerdeuJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
                                                                                                                                                                                                                | otherwise = (InGame, jogadorMove jogo (Move Baixo), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (InGame, jogadorMove jogo (Move Esquerda), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (InGame, jogadorMove jogo (Move Direita), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)

reageEventoGloss _ (InGame, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, True, yRecorde, yAux) = (PerdeuJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, True, yRecorde, yAux)
reageEventoGloss _ (InGame, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (InGame , jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, (jogoTerminou jogo), yRecorde, yAux) 
reageEventoGloss _ estado = 
@

-} 
reageEventoGloss :: Event -> EstadoGloss -> EstadoGloss
----- TELA INCIAL
-- BOTAO JOGAR
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Sair, jogo, texturas, textura, skins, skin,fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Escolher, jogo, texturas, textura, skins, skin,fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (InGame, jogo, texturas, textura, skins, snd(head(skins)), fundos, botoes, 0, 0, 0, False, 0, 0)

-- BOTAO ESCOLHER
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Escolher, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, 0, 0, 0, False, 0, 0)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Escolher, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Desafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Escolher, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher P1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)

-- BOTAO DESAFIO
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Desafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Escolher, jogo, texturas, textura, skins, skin, fundos, botoes, 0, 0, 0, False, 0, 0)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Desafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Desafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)


-- BOTAO SAIR
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Desafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, _, _, _, _, _, _, _, _, _, _, _, yRecorde, _) = error "FIM"


------ TELA PERDEU
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, _, texturas, textura, skins, _, fundos, botoes, _, _, _, _, _, _) = estadoGlossInicial texturas skins fundos botoes

---- Tela Ganhou
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (GanhouJogo, _, texturas, textura, skins, _, fundos, botoes, _, _, _, _, _, _) = estadoGlossInicial texturas skins fundos botoes
        
-- ver melhor

---- TELA ESCOLHER Personagemn
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuEscolher P, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher Sair,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuEscolher P, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuEscolher P1,   jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuEscolher P, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (InGame, (Jogo (Jogador (5,(-12)))  (completarMapaLinhas mapaFinal)), texturas, (texturasNormal texturas), skins, snd(skins !! 0), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuEscolher P1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuEscolher P1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher P,    jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuEscolher P1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuEscolher P2,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuEscolher P1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (InGame, (Jogo (Jogador (5,(-12)))  (completarMapaLinhas mapaFinal)), texturas, (texturasDeserto texturas), skins, snd(skins !! 1), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuEscolher P2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuEscolher P2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher P1,   jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuEscolher P2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (InGame, (Jogo (Jogador (5,(-12)))  (completarMapaLinhas mapaFinal)), texturas, (texturasNatal texturas), skins, snd(skins !! 2), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (MenuEscolher Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuEscolher P1,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuEscolher Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Jogar,     jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)


---- TELA DESAFIO
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuDesafio D, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuDesafio Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuDesafio D, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuDesafio D1,   jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuDesafio D, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (ModoDesafio, (Jogo (Jogador (5,(-13)))  desafio1), texturas, (texturasNormal texturas), skins, snd(skins !! 0), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuDesafio Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuDesafio D, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuDesafio D2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (ModoDesafio, (Jogo (Jogador (5,(-13)))  desafio2), texturas, (texturasDeserto texturas), skins, snd(skins !! 1), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (MenuDesafio D2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuDesafio Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (MenuDesafio D2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (MenuDesafio D1, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuDesafio D2, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (ModoDesafio, (Jogo (Jogador (5,(-13)))  desafio3), texturas, (texturasNatal texturas), skins, snd(skins !! 2), fundos, botoes, 0, 0, 0, False, 0, 0)

reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _)    (MenuDesafio Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (MenuDesafio D1,  jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyEnter) Down _ _) (MenuDesafio Sair, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (Opcoes Jogar, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)


---- Tela Mododesafio  
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =(ModoDesafio, (jogadorMove jogo (Move Cima)), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+13+1)), yAux)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (ModoDesafio, (jogadorMove jogo (Move Cima)), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+13+1)), yAux)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) | y == -13 = (PerdeuJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
                                                                                                                                                                                                                     | otherwise = (ModoDesafio, jogadorMove jogo (Move Baixo), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (ModoDesafio, jogadorMove jogo (Move Esquerda), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (ModoDesafio, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (ModoDesafio, jogadorMove jogo (Move Direita), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)

reageEventoGloss _ (ModoDesafio, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (ModoDesafio , jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, (jogoTerminou jogo), yRecorde, yAux) 



----- TELA JOGO
reageEventoGloss (EventKey (SpecialKey KeySpace) Down _ _) (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) | (y+2) == 0 = (InGame, (deslizaJogo (round tempoInicial) (jogadorMove jogo (Move Cima))), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+12+1)), yAux+1)
                                                                                                                                                                                                                 | otherwise = (InGame, (jogadorMove jogo (Move Cima)), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+12+1)), yAux)
reageEventoGloss (EventKey (SpecialKey KeyUp) Down _ _) (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) | (y+2) == 0 = (InGame, (deslizaJogo (round tempoInicial) (jogadorMove jogo (Move Cima))), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+12+1)), yAux+1)
                                                                                                                                                                                                              | otherwise = (InGame, (jogadorMove jogo (Move Cima)), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, (max yRecorde (y+yAux+12+1)), yAux)
reageEventoGloss (EventKey (SpecialKey KeyDown) Down _ _) (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) | y == -13 = (PerdeuJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
                                                                                                                                                                                                                | otherwise = (InGame, jogadorMove jogo (Move Baixo), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyLeft) Down _ _) (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (InGame, jogadorMove jogo (Move Esquerda), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)
reageEventoGloss (EventKey (SpecialKey KeyRight) Down _ _) (InGame, jogo@(Jogo (Jogador (x,y)) mapa), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) =  (InGame, jogadorMove jogo (Move Direita), texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux)

reageEventoGloss _ (InGame, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, True, yRecorde, yAux) = (PerdeuJogo, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, True, yRecorde, yAux)
reageEventoGloss _ (InGame, jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, estadoJogo, yRecorde, yAux) = (InGame , jogo, texturas, textura, skins, skin, fundos, botoes, tempoInicial, tempoJogo, tempoMover, (jogoTerminou jogo), yRecorde, yAux) 
reageEventoGloss _ estado = estado


{-| 
A função 'lerCoordenadas' é uma função auxiliar que lê coordenadas.
A função pode ser definida da seguinte forma:

@
lerCoordenadas :: Jogo -> (Int,Int)
lerCoordenadas (Jogo (Jogador (x,y)) mapa) = (x,y)
@

-} 
lerCoordenadas :: Jogo -> (Int,Int)
lerCoordenadas (Jogo (Jogador (x,y)) mapa) = (x,y)

---------------------------------------------------------------------

{-| 
A função 'main' é uma funçao que aplica um jogo em uma janela, usando ações IO para construir as imagens..
A função pode ser definida da seguinte forma:

@
main :: IO ()
main = do   
    arvore  <- loadBMP "img/arvore.bmp"
    pedra  <- loadBMP "img/pedra.bmp"
    nenhum  <- loadBMP "img/nenhum.bmp"
    tronco  <- loadBMP "img/tronco.bmp"
    nenhumt <- loadBMP "img/nenhumt.bmp"
    carro   <- loadBMP "img/carro.bmp"
    carroInvertido <- loadBMP "img/carro1.bmp"
    nenhumc <- loadBMP "img/nenhumc.bmp"

    arvoreDeserto  <- loadBMP "img/cactus.bmp"
    pedraDeserto  <- loadBMP "img/pedraDeserto.bmp"
    nenhumDeserto <- loadBMP "img/nenhumDeserto.bmp"
    troncoDeserto  <- loadBMP "img/troncoDeserto.bmp"
    nenhumtDeserto <- loadBMP "img/nenhumtDeserto.bmp"
    carroDeserto   <- loadBMP "img/carroDeserto.bmp"
    carroInvertidoDeserto <- loadBMP "img/carroInvertidoDeserto.bmp"
    nenhumcDeserto <- loadBMP "img/nenhumcDeserto.bmp"

    arvoreNatal  <- loadBMP "img/arvoreNatal.bmp"
    pedraNatal  <- loadBMP "img/arvoreNatal.bmp"
    nenhumNatal  <- loadBMP "img/nenhumNatal.bmp"
    troncoNatal  <- loadBMP "img/troncoNatal.bmp"
    nenhumtNatal <- loadBMP "img/nenhumtNatal.bmp"
    carroNatal   <- loadBMP "img/treno.bmp"
    carroInvertidoNatal <- loadBMP "img/trenoInvertido.bmp"
    nenhumcNatal <- loadBMP "img/nenhumcNatal.bmp"

    skin <- loadBMP "img/galinha.bmp"
    skin1 <- loadBMP "img/camelo.bmp"
    skin2 <- loadBMP "img/bonecodeneve.bmp"

    botaoJogar <- loadBMP "img/botaoJogar.bmp"
    botaoJogarS <- loadBMP "img/botaoJogarS.bmp"
    botaoSair <- loadBMP "img/botaoSair.bmp"
    botaoSairS <- loadBMP "img/botaoSairS.bmp"
    botaoEscolher <- loadBMP "img/botaoEscolher.bmp"
    botaoEscolherS <- loadBMP "img/botaoEscolherS.bmp"
    botaoDireita <- loadBMP "img/setaDireita.bmp"
    botaoEsquerda <- loadBMP "img/setaEsquerda.bmp"

    botaoDesafio <- loadBMP "img/botaoDesafio.bmp"
    botaoDesafioS <- loadBMP "img/botaoDesafioS.bmp"
    desafio1 <- loadBMP "img/desafio1.bmp"
    desafio1S <- loadBMP "img/desafio1S.bmp"
    desafio2 <- loadBMP "img/desafio2.bmp"
    desafio2S <- loadBMP "img/desafio2S.bmp"
    desafio3 <- loadBMP "img/desafio3.bmp"
    desafio3S <- loadBMP "img/desafio3S.bmp"

    fundoInGame <- loadBMP "img/fundoInGame.bmp"
    fundoOpcoes <- loadBMP "img/fundoOpcoes.bmp"
    logo <- loadBMP "img/logo.bmp"
    fundoPerdeuJogo <- loadBMP "img/fundoPerdeuJogo.bmp"


    play janela       -- janela onde irá decorrer o jogo
      (greyN 0.75)            -- cor do fundo da janela
      frames              -- frame rate
      (estadoGlossInicial
      [
      (Arvore,  ((scale 0.2 0.2 arvore), (0,0))),
      (Pedra,   ((scale 0.2 0.2 pedra), (0,0))),
      (Nenhum,  ((scale 0.1 0.1 nenhum), (0,0))),
      (Tronco,  ((scale 0.2 0.2 tronco), (0,0))),
      (NenhumT, ((scale 0.2 0.2 nenhumt), (0,0))),
      (Carro,   ((scale 0.2 0.2 carro), (0,0))),
      (CarroVP, ((scale 0.2 0.2 carroInvertido), (0,0))),
      (NenhumC, ((scale 0.2 0.2 nenhumc), (0,0))),
      
      (Arvore,  ((scale 0.2 0.2 arvoreDeserto), (0,0))),
      (Pedra,   ((scale 0.2 0.2 pedraDeserto), (0,0))),
      (Nenhum,  ((scale 0.2 0.2 nenhumDeserto), (0,0))),
      (Tronco,  ((scale 0.2 0.2 troncoDeserto), (0,0))),
      (NenhumT, ((scale 0.2 0.2 nenhumtDeserto), (0,0))),
      (Carro,   ((scale 0.2 0.2 carroDeserto), (0,0))),
      (CarroVP, ((scale 0.2 0.2 carroInvertidoDeserto), (0,0))),
      (NenhumC, ((scale 0.2 0.2 nenhumcDeserto), (0,0))),

      (Arvore,  ((scale 0.2 0.2 arvoreNatal), (0,0))),
      (Pedra,   ((scale 0.2 0.2 pedraNatal), (0,0))),
      (Nenhum,  ((scale 0.2 0.2 nenhumNatal), (0,0))),
      (Tronco,  ((scale 0.2 0.2 troncoNatal), (0,0))),
      (NenhumT, ((scale 0.2 0.2 nenhumtNatal), (0,0))),
      (Carro,   ((scale 0.2 0.2 carroNatal), (0,0))),
      (CarroVP, ((scale 0.2 0.2 carroInvertidoNatal), (0,0))),
      (NenhumC, ((scale 0.2 0.2 nenhumcNatal), (0,0)))
      ]
      [
        (Galinha,    (scale 0.075 0.075 skin)),
        (Camelo,     (scale 0.2 0.2 skin1)),
        (BonecoNeve, (scale 0.15 0.15 skin2))
      ]
      [
       fundoInGame, 
       fundoOpcoes, 
       logo, 
       fundoPerdeuJogo
      ]
      [
        (JogarB, (scale 0.2 0.2 botaoJogar)),
        (JogarBS, (scale 0.2 0.2 botaoJogarS)),
        (SairB, (scale 0.2 0.2 botaoSair)),
        (SairBS, (scale 0.2 0.2 botaoSairS)),
        (EscolherB, (scale 0.2 0.2 botaoEscolher)),
        (EscolherBS, (scale 0.2 0.2 botaoEscolherS)),

        (DesafioB, (scale 0.2 0.2 botaoDesafio)),
        (DesafioBS, (scale 0.2 0.2 botaoDesafioS)),
        (DesafioD1, (scale 0.3 0.3 desafio1)),
        (DesafioD1S, (scale 0.3 0.3 desafio1S)),
        (DesafioD2, (scale 0.3 0.3 desafio2)),
        (DesafioD2S, (scale 0.3 0.3 desafio2S)),
        (DesafioD3, (scale 0.3 0.3 desafio3)),
        (DesafioD3S, (scale 0.3 0.3 desafio3S)),

        (DireitaB, (scale 0.2 0.2 botaoDireita)),
        (EsquerdaB, (scale 0.2 0.2 botaoEsquerda))
      ]
        )  -- define estado inicial do jogo
      desenharEstadoGloss  -- desenha o estado do jogo
      reageEventoGloss    -- reage a um evento
      reageTempoGloss     -- reage ao passar do tempo

@

-} 

main :: IO ()
main = do   
    arvore  <- loadBMP "img/arvore.bmp"
    pedra  <- loadBMP "img/pedra.bmp"
    nenhum  <- loadBMP "img/nenhum.bmp"
    tronco  <- loadBMP "img/tronco.bmp"
    nenhumt <- loadBMP "img/nenhumt.bmp"
    carro   <- loadBMP "img/carro.bmp"
    carroInvertido <- loadBMP "img/carro1.bmp"
    nenhumc <- loadBMP "img/nenhumc.bmp"

    arvoreDeserto  <- loadBMP "img/cactus.bmp"
    pedraDeserto  <- loadBMP "img/pedraDeserto.bmp"
    nenhumDeserto <- loadBMP "img/nenhumDeserto.bmp"
    troncoDeserto  <- loadBMP "img/troncoDeserto.bmp"
    nenhumtDeserto <- loadBMP "img/nenhumtDeserto.bmp"
    carroDeserto   <- loadBMP "img/carroDeserto.bmp"
    carroInvertidoDeserto <- loadBMP "img/carroInvertidoDeserto.bmp"
    nenhumcDeserto <- loadBMP "img/nenhumcDeserto.bmp"

    arvoreNatal  <- loadBMP "img/arvoreNatal.bmp"
    pedraNatal  <- loadBMP "img/arvoreNatal.bmp"
    nenhumNatal  <- loadBMP "img/nenhumNatal.bmp"
    troncoNatal  <- loadBMP "img/troncoNatal.bmp"
    nenhumtNatal <- loadBMP "img/nenhumtNatal.bmp"
    carroNatal   <- loadBMP "img/treno.bmp"
    carroInvertidoNatal <- loadBMP "img/trenoInvertido.bmp"
    nenhumcNatal <- loadBMP "img/nenhumcNatal.bmp"

    skin <- loadBMP "img/galinha.bmp"
    skin1 <- loadBMP "img/camelo.bmp"
    skin2 <- loadBMP "img/bonecodeneve.bmp"

    botaoJogar <- loadBMP "img/botaoJogar.bmp"
    botaoJogarS <- loadBMP "img/botaoJogarS.bmp"
    botaoSair <- loadBMP "img/botaoSair.bmp"
    botaoSairS <- loadBMP "img/botaoSairS.bmp"
    botaoEscolher <- loadBMP "img/botaoEscolher.bmp"
    botaoEscolherS <- loadBMP "img/botaoEscolherS.bmp"
    botaoDireita <- loadBMP "img/setaDireita.bmp"
    botaoEsquerda <- loadBMP "img/setaEsquerda.bmp"

    botaoDesafio <- loadBMP "img/botaoDesafio.bmp"
    botaoDesafioS <- loadBMP "img/botaoDesafioS.bmp"
    desafio1 <- loadBMP "img/desafio1.bmp"
    desafio1S <- loadBMP "img/desafio1S.bmp"
    desafio2 <- loadBMP "img/desafio2.bmp"
    desafio2S <- loadBMP "img/desafio2S.bmp"
    desafio3 <- loadBMP "img/desafio3.bmp"
    desafio3S <- loadBMP "img/desafio3S.bmp"

    fundoInGame <- loadBMP "img/fundoInGame.bmp"
    fundoOpcoes <- loadBMP "img/fundoOpcoes.bmp"
    logo <- loadBMP "img/logo.bmp"
    fundoPerdeuJogo <- loadBMP "img/fundoPerdeuJogo.bmp"


    play janela       -- janela onde irá decorrer o jogo
      (greyN 0.75)            -- cor do fundo da janela
      frames              -- frame rate
      (estadoGlossInicial
      [
      (Arvore,  ((scale 0.2 0.2 arvore), (0,0))),
      (Pedra,   ((scale 0.2 0.2 pedra), (0,0))),
      (Nenhum,  ((scale 0.1 0.1 nenhum), (0,0))),
      (Tronco,  ((scale 0.2 0.2 tronco), (0,0))),
      (NenhumT, ((scale 0.2 0.2 nenhumt), (0,0))),
      (Carro,   ((scale 0.2 0.2 carro), (0,0))),
      (CarroVP, ((scale 0.2 0.2 carroInvertido), (0,0))),
      (NenhumC, ((scale 0.2 0.2 nenhumc), (0,0))),
      
      (Arvore,  ((scale 0.2 0.2 arvoreDeserto), (0,0))),
      (Pedra,   ((scale 0.2 0.2 pedraDeserto), (0,0))),
      (Nenhum,  ((scale 0.2 0.2 nenhumDeserto), (0,0))),
      (Tronco,  ((scale 0.2 0.2 troncoDeserto), (0,0))),
      (NenhumT, ((scale 0.2 0.2 nenhumtDeserto), (0,0))),
      (Carro,   ((scale 0.2 0.2 carroDeserto), (0,0))),
      (CarroVP, ((scale 0.2 0.2 carroInvertidoDeserto), (0,0))),
      (NenhumC, ((scale 0.2 0.2 nenhumcDeserto), (0,0))),

      (Arvore,  ((scale 0.2 0.2 arvoreNatal), (0,0))),
      (Pedra,   ((scale 0.2 0.2 pedraNatal), (0,0))),
      (Nenhum,  ((scale 0.2 0.2 nenhumNatal), (0,0))),
      (Tronco,  ((scale 0.2 0.2 troncoNatal), (0,0))),
      (NenhumT, ((scale 0.2 0.2 nenhumtNatal), (0,0))),
      (Carro,   ((scale 0.2 0.2 carroNatal), (0,0))),
      (CarroVP, ((scale 0.2 0.2 carroInvertidoNatal), (0,0))),
      (NenhumC, ((scale 0.2 0.2 nenhumcNatal), (0,0)))
      ]
      [
        (Galinha,    (scale 0.075 0.075 skin)),
        (Camelo,     (scale 0.2 0.2 skin1)),
        (BonecoNeve, (scale 0.15 0.15 skin2))
      ]
      [
       fundoInGame, 
       fundoOpcoes, 
       logo, 
       fundoPerdeuJogo
      ]
      [
        (JogarB, (scale 0.2 0.2 botaoJogar)),
        (JogarBS, (scale 0.2 0.2 botaoJogarS)),
        (SairB, (scale 0.2 0.2 botaoSair)),
        (SairBS, (scale 0.2 0.2 botaoSairS)),
        (EscolherB, (scale 0.2 0.2 botaoEscolher)),
        (EscolherBS, (scale 0.2 0.2 botaoEscolherS)),

        (DesafioB, (scale 0.2 0.2 botaoDesafio)),
        (DesafioBS, (scale 0.2 0.2 botaoDesafioS)),
        (DesafioD1, (scale 0.3 0.3 desafio1)),
        (DesafioD1S, (scale 0.3 0.3 desafio1S)),
        (DesafioD2, (scale 0.3 0.3 desafio2)),
        (DesafioD2S, (scale 0.3 0.3 desafio2S)),
        (DesafioD3, (scale 0.3 0.3 desafio3)),
        (DesafioD3S, (scale 0.3 0.3 desafio3S)),

        (DireitaB, (scale 0.2 0.2 botaoDireita)),
        (EsquerdaB, (scale 0.2 0.2 botaoEsquerda))
      ]
        )  -- define estado inicial do jogo
      desenharEstadoGloss  -- desenha o estado do jogo
      reageEventoGloss    -- reage a um evento
      reageTempoGloss     -- reage ao passar do tempo



desafio1 = Mapa 11 [(Estrada 3,[CarroVP,CarroVP,CarroVP,NenhumC,NenhumC,CarroVP,CarroVP,CarroVP,NenhumC,NenhumC,NenhumC]),(Rio 1,[Tronco,Tronco,Tronco,NenhumT,NenhumT,NenhumT,Tronco,Tronco,Tronco,NenhumT,NenhumT]),(Rio (-2),[NenhumT,Tronco,Tronco,Tronco,NenhumT,NenhumT,Tronco,Tronco,Tronco,NenhumT,NenhumT]),(Rio 2,[NenhumT,Tronco,NenhumT,NenhumT,NenhumT,NenhumT,Tronco,Tronco,Tronco,Tronco,NenhumT]),(Relva,[Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum]),(Rio 1,[NenhumT,Tronco,Tronco,Tronco,NenhumT,Tronco,Tronco,Tronco,Tronco,NenhumT,NenhumT]),(Rio (-1),[Tronco,NenhumT,NenhumT,Tronco,NenhumT,NenhumT,Tronco,Tronco,Tronco,NenhumT,NenhumT]),(Rio 2,[Tronco,NenhumT,Tronco,Tronco,NenhumT,Tronco,NenhumT,NenhumT,Tronco,NenhumT,Tronco]),(Relva,[Nenhum,Nenhum,Pedra,Arvore,Nenhum,Pedra,Nenhum,Nenhum,Arvore,Arvore,Arvore]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Pedra,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Pedra]),(Rio (-1),[Tronco,NenhumT,Tronco,Tronco,NenhumT,Tronco,Tronco,Tronco,Tronco,NenhumT,Tronco]),(Rio 3,[NenhumT,NenhumT,Tronco,Tronco,Tronco,NenhumT,Tronco,Tronco,Tronco,NenhumT,NenhumT]),(Estrada (-3),[Carro,Carro,NenhumC,NenhumC,NenhumC,Carro,Carro,Carro,NenhumC,Carro,NenhumC]),(Relva,[Nenhum,Nenhum,Arvore,Pedra,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Pedra,Arvore])]
desafio2 = Mapa 11 [(Rio 2,[Tronco,NenhumT,Tronco,Tronco,NenhumT,Tronco,NenhumT,NenhumT,Tronco,NenhumT,Tronco]),(Estrada (-3),[Carro,Carro,NenhumC,NenhumC,NenhumC,Carro,Carro,Carro,NenhumC,Carro,NenhumC]),(Estrada 1,[CarroVP,CarroVP,NenhumC,NenhumC,NenhumC,NenhumC,CarroVP,NenhumC,NenhumC,CarroVP,NenhumC]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Arvore]),(Rio (-1),[Tronco,NenhumT,NenhumT,Tronco,NenhumT,NenhumT,Tronco,Tronco,Tronco,NenhumT,NenhumT]),(Rio 1,[NenhumT,Tronco,Tronco,Tronco,NenhumT,Tronco,Tronco,Tronco,Tronco,NenhumT,NenhumT]),(Estrada (-2),[NenhumC,Carro,NenhumC,Carro,NenhumC,Carro,Carro,Carro,NenhumC,Carro,NenhumC]),(Estrada 2,[NenhumC,CarroVP,NenhumC,CarroVP,NenhumC,CarroVP,CarroVP,CarroVP,NenhumC,CarroVP,NenhumC]),(Relva,[Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Estrada 2,[NenhumC,CarroVP,CarroVP,CarroVP,NenhumC,NenhumC,NenhumC,CarroVP,NenhumC,CarroVP,NenhumC]),(Estrada (-1),[NenhumC,Carro,Carro,Carro,NenhumC,NenhumC,Carro,Carro,Carro,NenhumC,NenhumC]),(Estrada 3,[NenhumC,CarroVP,NenhumC,CarroVP,NenhumC,NenhumC,CarroVP,NenhumC,CarroVP,NenhumC,NenhumC]),(Estrada (-3),[Carro,Carro,NenhumC,NenhumC,NenhumC,Carro,Carro,Carro,NenhumC,Carro,NenhumC]),(Relva,[Nenhum,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore])]
desafio3 = Mapa 11 [(Rio 2,[Tronco,NenhumT,Tronco,Tronco,NenhumT,Tronco,NenhumT,NenhumT,Tronco,NenhumT,Tronco]),(Estrada (-3),[Carro,NenhumC,NenhumC,NenhumC,NenhumC,Carro,Carro,Carro,NenhumC,Carro,NenhumC]),(Estrada 1,[CarroVP,CarroVP,NenhumC,NenhumC,NenhumC,NenhumC,CarroVP,NenhumC,NenhumC,CarroVP,NenhumC]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore]),(Rio 1,[NenhumT,Tronco,Tronco,Tronco,NenhumT,Tronco,Tronco,Tronco,Tronco,NenhumT,NenhumT]),(Rio (-1),[NenhumT,Tronco,Tronco,Tronco,NenhumT,Tronco,NenhumT,Tronco,Tronco,NenhumT,NenhumT]),(Rio 2,[NenhumT,Tronco,NenhumT,Tronco,Tronco,Tronco,NenhumT,Tronco,Tronco,NenhumT,NenhumT]),(Relva,[Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore]),(Estrada 2,[NenhumC,CarroVP,CarroVP,CarroVP,NenhumC,NenhumC,NenhumC,CarroVP,NenhumC,CarroVP,NenhumC]),(Estrada (-1),[NenhumC,Carro,Carro,Carro,NenhumC,NenhumC,Carro,Carro,Carro,NenhumC,NenhumC]),(Rio 2,[NenhumT,Tronco,Tronco,NenhumT,Tronco,Tronco,NenhumT,Tronco,Tronco,NenhumT,Tronco]),(Rio (-2),[NenhumT,Tronco,Tronco,NenhumT,NenhumT,Tronco,Tronco,NenhumT,Tronco,NenhumT,Tronco]),(Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum])]

mapaTeste = Mapa 11 [(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore])]

mapaFinal = Mapa 11 [(Relva, [Arvore,Pedra,Pedra,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Arvore,Arvore]), (Relva, [Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Pedra,Arvore]),(Relva, [Arvore,Pedra,Arvore,Arvore,Pedra,Arvore,Pedra,Pedra,Arvore,Arvore,Arvore])]

completarMapaLinhas :: Mapa -> Mapa
completarMapaLinhas (Mapa l []) = (Mapa l [])
completarMapaLinhas mapa@(Mapa l ((t,(o:os)):r)) | 14 == terrenos mapa = mapa
                                                 | otherwise = completarMapaLinhas (estendeMapa mapa seed)
                                                where seed = unsafePerformIO (getStdRandom (randomR (0, 100)))

terrenos :: Mapa -> Int 
terrenos (Mapa l []) = 0
terrenos (Mapa l (t:r)) = 1 + terrenos (Mapa l r)