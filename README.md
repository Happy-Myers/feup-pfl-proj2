# Moxie Prolog

Trabalho realizado por: 
* Gonçalo Carvalho Marques -up202006874
* David Oliveira Espinha Marques -up201905574

## Descrição do projeto
O objetivo deste projeto era fazer um jogo em prolog. O nosso grupo decidiu escolher o jogo Moxie.

As regras do jogo são simples: <br>
O jogo é jogado num tabuleiro de 4 por 4 e cada jogador tem inicialmente 8 peças. Na sua jogado o jogador pode fazer uma de três coisas, mover uma das peças para uma posição adjacente que esteja vazia, por uma nova peça no tabuleiro ou capturar uma peça do adversário fazendo a sua peça saltar por cima da do adversário e cair numa casa vazia imediatamente a a seguir a peça capturada. Caso o jogador possa executar uma catpura esta é obrigatória e é possível executar multiplas capturas numa so jogado. <br>
O objetivo do jogo é conseguir fazer três em linha em qualquer direção ou capturar 6 peças do adversário. <br>
Mais informação sobre o jogo pode ser encontrada aqui: https://www.di.fc.ul.pt/~jpn/gv/moxie.htm

## Instalação e Execução

Para executar o program é necessário a utilizacão do compilador de prolog SICStus Prolog. <br>
Para instalar o jogo baste consultar o ficheiro moxie.pl, todas as dependências necesssárias ao programa serão carregadas. <br>
Para executar o jogo tem de se executar o predicado start/0.

## Lógica do jogo

### Representação interna do estado do jogo

Durante a execução do jogo são guardades internamente vários dados importatnes ao jogo, no predicado game_state(-turnNum,-P1State,-P2State,-Board) são guardados o número do turno, a pontuação de ambos os jogadores e o estado do tabuleiro. <br>
O tabuleiro é representado por uma lista de listas de dimensão size x size, cada elemento desta matriz representa uma casa do tabuleiro sendo que se o elemento for 0 a casa encontra-se vazia, 1 tem uma peça do jogador 1 e 2 tem uma peça do jogador 2.

### Visualização do estado de jogo

O visualização do jogo está ao encargo do predicado display_game(+Gamestate) que recebe o estado do tabuleiro e imprime na consola um versão modificada desta para a leitura do mesmo ser mais facil. Assim caso o predicado enontre um 0 imorime um espaço vazio, se encontrar um 1 imprime uma cruz e se encontrar um 2 imprime um circulo. <br>
Quando os jogadores iniciam o jogo encontrão o menu do mesmo, este tem várias opções disponiveis:
* Play: Começar a jogar
* Change Gamemode: permite escoclher o modo de jogo entre pvp e pve para escolher o modo de jogo o jogador tem que escrever que tipo de jogador é que quer h para humano, pc1 para o bot no modo facil e pc2 para o no modo díficil bot ex: h/h seria o modo de jogo de humano vs humano
* Change board size: O jogador pode escolher o tamanho do tabuleiro
* Leave: Sair do jogo

### Incio do Jogo

Para dar início ao jogo em si o predicado play/0 é chamado este chama os predicados gamemode/1 size/1 que podem ter sido modifcados anteriormente no menu e inicializa um tabuleiro com as caracteristicas definidas. Se o jogador não definir gamemode nem board size os valores default de h/h e 4 são utilizados.

### Execução de Jogadas

Quando começa a jogado de algum dos utilizadores o predicado player_turn(+Player, +PlayerType) é chamado este predicado verifica se se trata do turno do primeiro o segundo jogador e o tipo de do jogador (se é um bot ou um humano), e dependo destes dois valores chama os predicados adequados.
* Humano: Primeiro faz-se a verificação de que jogados é que o jogador pode executar e mostra-se esta lista ao mesmo, em seguida é lido o input do jogador e caso este seja valido a jogada é executada caso contrario pede-se para o jogador repitor o input 
* Bot: Primeiro computa-se todas as jogadas possiveis que o bot pode executar e caso se trate do bot facíl uma jogada aleatória é escolhida recorrendo ao módulo random, caso se trate do bot dificil as jogadas são avaliadas e a melhor é escolhida baseado na pontuação das mesmas, caso haja várias jogados com a mesma pontuação é escolhida uma aleatóriamente. 

### Avaliação do Estado do Jogo

### Final do Jogo

## Conclusões