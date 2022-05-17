# GEEQ - Grupo de Estudos em Ecologia Quantitativa

## Workshop "Construindo redes de interações no R"

<img src="https://github.com/fblpalmeira/GEEQ/blob/main/data/impresso.jpg" align="right" width = "500px"/>

Contruindo redes de interação ecológica utilizando o CarniDIET (version 1.0), uma base contendo dados quantitativos sobre a dieta de mamíferos terrestres [(Middleton et al 2021).](https://doi.org/10.1111/geb.13296) O objetivo desta prática será construir uma rede de interação bipartida utilizado o pacote ‘bipartite’ [(Dormann et al 2021).](http://cran.r-project.org/web/packages/bipartite/bipartite.pdf) Também será possível estimar o padrão de conectividade entre predadores e presas (interações observadas e possíveis), o grau de conectância e a sua distribuição, o quanto as espécies interagem, qual a força desta interação, como as interações se sobrepõem e se agrupam, qual o papel das espécies (espécies hub da rede, hub do módulo, conectoras e/ou periféricas) e quais os aspectos ecológicos mais importantes para a estrutura e a dinâmica da rede. 

## Link do workshop no YouTube, dia 17/05/2022 das 19h00 às 21h00.

[Canal do GEEQ no YouTube](https://youtu.be/VTemIrqlsqA)

## Material utilizado

- [Código 1 `.R`](https://github.com/fblpalmeira/GEEQ/blob/main/data/Bipartite_carnidiet.R)

- [Código 2 `.R`](https://github.com/fblpalmeira/GEEQ/blob/main/data/Ordernet.R)

- [Código 3 `.R`](https://github.com/fblpalmeira/GEEQ/blob/main/data/Entropy.R)

- [Database 1 `.csv`](https://github.com/fblpalmeira/GEEQ/blob/main/data/geb13296-sup-0011-carnidiet.csv)

- [Database 2 `.csv`](https://github.com/fblpalmeira/GEEQ/blob/main/data/geb13296-sup-0011-carnidiet2.csv)

## Prática

Esta prática tem três etapas principais: 

- Preparando os dados (Carregar, manipular e limpar o arquivo de dados);

- Construindo redes (Extrair métricas e visualizar as redes de interações); e,
 
- Inserindo silhuetas (Carregar as silhuetas das espécies e incorporar no gráfico).  

## Preparando os dados

Carregar, manipular e limpar o arquivo de dados no `R`:

``` r
#######################
# Preparando os dados #
#######################

#Middleton et al 2021. CarniDIET 1.0: A database of terrestrial carnivorous mammal diets. Global Ecology and Biogeography 30:1175-1182. https://doi.org/10.1111/geb.13296
y1<-read.csv("https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fgeb.13296&file=geb13296-sup-0011-CarniDIET.csv", sep = ",")
y1            #Ver a tabela (omite as ultimas linhas quando a tabela e muito grande)
head(y1)      #Ver o cabecalho da tabela - mostra o cabecalho e as seis primeiras linhas
View(y1)      #Ver a tabela inteira
summary(y1)   #Ver o resumo dos resultados
str(y1)       #Ver a estrutura das variaveis (uma alternativa ao "summary")
names(y1)     #Ver o nome das colunas 
class(y1)     #Confirmar qual é classe de dados do objeto (ex.: data frame, matriz, vetor, etc...)

library(dplyr) #Abrir pacote 'dplyr' para manipular os dados

y2<-y1%>%
  select(familyCarni, scientificNameCarni, commonNameCarni, 
         foodType, orderPrey, familyPrey, genusPrey, scientificNamePrey, commonNamePrey,
         percentage, country) %>% 
  count(familyCarni, scientificNameCarni, commonNameCarni, 
        foodType, orderPrey, familyPrey, genusPrey, scientificNamePrey, commonNamePrey,
        percentage, country, sort=T)
View(y2)

y2%>%
  filter(familyCarni=="Felidae")%>%
  select(commonNameCarni, orderPrey)%>% 
  count(commonNameCarni, orderPrey, sort=T)

y3<-y2%>%
  filter(commonNameCarni %in% c("Geoffroy's cat","Jaguar","Jaguarundi","Margay","Ocelot","Oncilla","Puma"))%>%
  select(commonNameCarni, orderPrey)%>% 
  count(commonNameCarni, orderPrey, sort=T)
y3

library(reshape2) #Abrir o pacote 'reshape2' para remodelar a tabela de dados
y4=dcast(y3, orderPrey ~ commonNameCarni, value.var="n")
y4
class(y4)

library(janitor) #Abrir o pacote 'janitor' para limpar algumas linhas de dados
y5<-y4[-c(14), ]

library(tidyr) #Abrir o pacote 'tidyr' para trabalhar com valores/dados faltantes (NA's)
y6<-mutate_all(y5, ~replace_na(.,0)) #Substituir os NA's por zeros
y6
class(y6)

colnames(y6)<-NULL
colnames(y6)<-c("","Geoffroy's cat","Jaguar","Jaguarundi","Margay","Ocelot","Oncilla","Puma")
y6<-noquote(y6)

web<-as.matrix(y6, row.names=1)
print(web, row.names = F)
write.csv(web, "geb13296-sup-0011-carnidiet2.csv", row.names=FALSE)
```

## Redes bipartidas

Construir e visualizar as redes de interação no `R`:

``` r
######################
# Construindo redes  #
######################

library (bipartite)
library (sna)
library (igraph)

web<-as.matrix(read.csv("geb13296-sup-0011-carnidiet2.csv", row.names=1))

#Kawada Kawai - melhor para ver nos centrais
gplot(web, gmode="twomode", usearrows="FALSE", mode="kamadakawai", main="Visualizar nohs centrais", object.scale = 0.03)

#Fruchterman Reignold - melhor para ver modulos
gplot(web, gmode="twomode", usearrows="FALSE", mode="fruchtermanreingold", main="Visualizar modulos", object.scale = 0.03)

source("Ordernet.R") #Carrega a funcao

order.web=order.net(mat=web) #Organiza a matriz de acordo com o grau das especies

#Grau decrescente - melhor para ver aninhamento
plotweb(web, method="normal",arrow="both",bor.col.interaction="grey", y.width.low=0.02, y.width.high=0.02, col.high="red", col.low="blue", high.lablength=0, low.lablength=0)
mtext("", side=3, line=1, font=1)

#------------------#
# Conectividade    #
#------------------#

#Grau 
K.web.Pred=apply(web,1,sum) #Exemplo de como calcular o grau dos predadores
K.web.Pred
K.web.Prey=apply(web,2,sum) #Exemplo de como calcular o grau das presas
K.web.Prey

#Grau medio 
KMed.web.Pred=mean(apply(web,1,sum)) #Calculo do grau medio de predadores
KMed.web.Pred
KMed.web.Prey=mean(apply(web,2,sum)) #Calculo do grau medio de presas
KMed.web.Prey

#Conectancia
K.web=apply(web,2,sum) 				        #Grau das colunas
E.web=apply(as.matrix(K.web), 2, sum)	#Numero de arestas (interacoes) na rede
R.web=dim(web)[1]				             	#Numero de linhas
C.web=L=dim(web)[2]					          #Numero de colunas
Conec.web=E.web/(R.web*C.web) 			  #Calculo da conectancia
Conec.web

#Visualizando os resultados de conectancia da rede
Connectance=data.frame(c(round(Conec.web,2)))
colnames(Connectance)=c("Connectance")
Connectance

#Distribuicao do grau
d.web = degreedistr(web) #Calcula graus para cada espécie e constroi uma distribuicao cumulativa.
d.web #Retorna os coeficientes e os ajustes de três funções diferentes de distribuições (exponencial, lei de potência e lei de potência truncada). 

#Entropia
source("Entropy.R") #Chama o código com a função que calcula a entropia
Entropy.web=entropy(web)
Entropy.web

#Visualizando os resultados de entropia
Entropy=data.frame(c(Entropy.web$Row.Entropy),c(Entropy.web$Col.Entropy),row.names=c("Entropia"))
colnames(Entropy)=c("Predator.Entropy","Prey.Entropy")
Entropy
                   
#-----------------------------#
# Aninhamento e distancias    #
#-----------------------------#
                   
#Aninhamento
web.NODF=nested(web, method = "NODF2") #Calcula NODF das matrizes
web.NODF

#Visualizando os resultados de aninhamento
Nestedness=data.frame(c(round(web.NODF,2)), row.names=c(""))
colnames(Nestedness)=c("NODF")
Nestedness
                   
#Distancias 
edgelist.web=web2edges(web, return=TRUE) #Criando listas de interaca de uma matriz n x m. Nohs das linhas vao de 0 a n-1. Nohs das colunas vao de n a m-1 
edgelist.web

write.table(edgelist.web[,1:2]-1,file="web.edge.txt",row.names=F, col.names=F, append=T, quote=F) #cria o arquivo de edgelist
                   
web.igraph=read.graph("web.edge.txt", format="edgelist", direct=FALSE) #le arquivo de edgelist e grava em variavel da classe igraph
web.igraph

#Matriz de distancia do noh da linha ao noh da coluna
web.paths=shortest.paths(web.igraph)  
web.paths
                   
#Caminho medio
path1.len.web=average.path.length(web.igraph) #media dos caminhos da rede
path1.len.web

Path.1=data.frame(c(round(path1.len.web,2)), row.names=c(""))
colnames(Path.1)=c("Caminho.medio1")
Path.1
                   
# Plotar e salvar o grafico bipartido
png(file="carnidiet_bipartite.png", width = 900, height = 600)
plotweb(web, labsize=1.3, text.rot=90, col.high="#6699CC", 
col.low="#999933", col.interaction="#999999")
dev.off()
                   
visweb(web, type="diagonal") 
                   
networklevel(web) #Calcula uma variedade de indices e valores para uma rede bipartida
specieslevel(web) #Outros indices simples sao calculados e retornados
nodespec(web) #Calcula um indice de especializacao com base nas posicoes dos nohs para todas as especies e separadamente para os niveis troficos superior e inferior
(ex<-second.extinct(web, participant="lower", method="random", nrep=50, details=F))
slope.bipartite(ex)
                   
nullmodel(web, N=1000, method=4)
nullmodel(web, N=1000, method=5)
                   
png(file="carnidiet_grid.png", width = 800, height = 700)
res<-computeModules(web)
plotModuleWeb(res)
dev.off()
                   
PAC(web) #Quantifica para cada par de especies de nivel trofico inferior, o potencial de competicao aparente com outra especie, mediada pelo nivel trofico superior
                   
degreedistr(web, plot.it=T)
V.ratio(web)
```

## Inserindo as silhouetas das espécies no  `R`:

``` r
#######################
# Inserindo silhuetas #
#######################
                   
library(magick)
                   
#Chamar o gráfico
plot<-image_read("carnidiet_grid.png")

#Inserir cabecalho/titulo
plot2<-image_annotate(plot, "Neotropical felids food web as a grid:
square is used to indicate number of interactions", 
       color = "blue", size = 18,
       location = "10+50", gravity = "north")

#Inserir rodape
plot3<-image_annotate(plot2, "Visualization by @fblpalmeira
       Data: Middleton et al 2021 (doi.org/10.1111/geb.13296)
       Image credit: Microbiotheria (Zimices), Other spp (Public domain) @PhyloPic", 
       color = "gray", size = 12, 
       location = "10+50", gravity = "southeast")

#Chama as silhuetas dos animais
chiroptera<-image_read("http://www.phylopic.org/assets/images/submissions/18bfd2fc-f184-4c3a-b511-796aafcc70f6.512.png") 
out1<-image_composite(plot3,image_scale(chiroptera,"x30"), offset = "+135+90")
                   
didelphis<-image_read("http://www.phylopic.org/assets/images/submissions/96ed3343-20e6-4bea-90e3-06dd157d2447.512.png") 
out2<-image_composite(out1,image_scale(didelphis,"x20"), offset = "+135+135")
                   
bradypus<-image_read("http://www.phylopic.org/assets/images/submissions/dbc172df-a4c7-4d0d-9388-c337d01aca52.512.png") 
out3<-image_composite(out2,image_scale(bradypus,"x40"), offset = "+135+160")
                   
cebidae<-image_read("http://www.phylopic.org/assets/images/submissions/6264cf2b-4f34-4b3f-ba65-cc5565db2de5.512.png") 
out4<-image_composite(out3,image_scale(cebidae,"x40"), offset = "+135+210")
                   
rodentia<-image_read("http://www.phylopic.org/assets/images/submissions/b6c7dab1-a2f8-43f6-98d5-ab6000f58957.512.png") 
out5<-image_composite(out4,image_scale(rodentia,"x30"), offset = "+135+255")
                   
carnivora<-image_read("http://www.phylopic.org/assets/images/submissions/040da77c-f13e-4962-ab09-4e4d0474e204.512.png") 
carnivora2<-image_flop(carnivora)
out6<-image_composite(out5,image_scale(carnivora2,"x30"), offset = "+135+310")
                   
pecari<-image_read("http://www.phylopic.org/assets/images/submissions/44fb7d4f-6d59-432b-9583-a87490259789.512.png") 
out7<-image_composite(out6,image_scale(pecari,"x35"), offset = "+130+345")
                   
armadillo<-image_read("http://www.phylopic.org/assets/images/submissions/5d59b5ce-c1dd-40f6-b295-8d2629b9775e.512.png") 
out8<-image_composite(out7,image_scale(armadillo,"x30"), offset = "+135+385")
                   
lagomorpha<-image_read("http://www.phylopic.org/assets/images/submissions/c3987570-9c82-41b9-bc72-cb5d3de1ff87.original.png") 
out9<-image_composite(out8,image_scale(lagomorpha,"x40"), offset = "+140+415")
                   
micro<-image_read("http://www.phylopic.org/assets/images/submissions/33eb7221-53b6-4d16-b79a-ce96e24e3754.512.png") 
out10<-image_composite(out9,image_scale(micro,"x30"), offset = "+120+465")
                   
tapirus<-image_read("http://www.phylopic.org/assets/images/submissions/8f6b8802-52f9-4f16-8429-0b86ea4a4aa8.512.png") 
out11<-image_composite(out10,image_scale(tapirus,"x45"), offset = "+120+500")

#Visualizar a imagem                  
image_browse(out11)
                   
#Salvar o gráfico com as silhuetas inseridas
image_write(out11, "carnidiet_grid2.png")
```

## Vamos construir uma rede de interação bipartida. 

<img src="https://github.com/fblpalmeira/GEEQ/blob/main/data/carnidiet_bipartite.png">

## Também vamos construir uma rede de interação em forma de grid.

<img src="https://github.com/fblpalmeira/CarniDIET/blob/main/data/felids_diet2.png">

## Referências: 

[Dormann CF, Fruend J, Gruber B. 2021.](http://cran.r-project.org/web/packages/bipartite/bipartite.pdf) Package ‘bipartite’. 

Middleton O, Svensson H, Scharlemann JP, Faurby S, Sandom C. 2021. [CarniDIET 1.0: A database of terrestrial carnivorous mammal diets.](https://doi.org/10.1111/geb.13296) Global Ecology and Biogeography, 30(6):1175-1182. https://doi.org/10.1111/geb.13296
