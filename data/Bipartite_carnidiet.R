y1 <- read.csv("geb13296-sup-0011-carnidiet.csv", sep = ",")
str(y1)

library (dplyr)
library (vcd)

y2 <- y1 %>%
  select(familyCarni, scientificNameCarni , commonNameCarni, 
         foodType, orderPrey, familyPrey, genusPrey, scientificNamePrey, commonNamePrey,
         percentage, country ) %>% 
  count(familyCarni, scientificNameCarni , commonNameCarni, 
        foodType, orderPrey, familyPrey, genusPrey, scientificNamePrey, commonNamePrey,
        percentage, country, sort=T)
y2
class(y2)
y2 %>%
  filter(familyCarni == "Felidae") %>%
  select(commonNameCarni,  orderPrey) %>% 
  count(commonNameCarni,  orderPrey, sort=T)

y3<-y2%>%
  filter(commonNameCarni %in% c("Geoffroy's cat","Jaguar","Jaguarundi","Margay","Ocelot","Oncilla","Puma"))%>%
  select(commonNameCarni, orderPrey) %>% 
  count(commonNameCarni, orderPrey, sort=T)

library(reshape2)
y4=dcast(y3,  orderPrey ~ commonNameCarni , value.var = "n")
class(y4)

library(janitor)
y5<-y4[-c(5,14), ]

library(tidyr)
y6<-mutate_all(y5, ~replace_na(.,0))
web<-as.matrix(y6, row.names=1)
class(y6)

colnames(y6)<-NULL
colnames(y6)<-c("","Geoffroy's cat","Jaguar","Jaguarundi","Margay","Ocelot","Oncilla","Puma")
y6 <- noquote(y6)

web<-as.matrix(y6, row.names=1)
print(web, row.names = F)
write.csv(web, "geb13296-sup-0011-carnidiet3.csv", row.names=FALSE)

#####################

web<-as.matrix(read.csv("geb13296-sup-0011-carnidiet3.csv", row.names=1))

# Kawada Kawai - melhor para ver nos centrais
gplot(web,gmode="twomode",usearrows="FALSE",mode="kamadakawai",main="",object.scale = 0.03)

# Fruchterman Reignold - melhor para ver modulos
gplot(web,gmode="twomode",usearrows="FALSE",mode="fruchtermanreingold",main="",object.scale = 0.03)

source("Ordernet.R") # carregando a funcao

order.web=order.net(mat=web) # Organiza a matriz de acordo com o grau das especies

# Decreasing degree - melhor para ver aninhamento
plotweb(web, method="normal",arrow="both",bor.col.interaction="grey", y.width.low=0.02, y.width.high=0.02, col.high="red", col.low="blue", high.lablength=0, low.lablength=0)
mtext("",side=3 , line=1, font=1)

#------------------#
# Conectividade    #
#------------------#

## Grau e Grau Medio ##
K.web.Pred=apply(web,1,sum) #Exemplo de como calcular o grau dos predadores
K.web.Pred
K.web.Prey=apply(web,2,sum) #Exemplo de como calcular o grau das presas
K.web.Prey

KMed.web.Pred=mean(apply(web,1,sum)) #Calculo do grau medio de predadores
KMed.web.Prey=mean(apply(web,2,sum)) #Calculo do grau medio de presas

## Conectancia ##

K.web=apply(web,2,sum) 				        #grau das colunas
E.web=apply(as.matrix(K.web), 2, sum)	#numero de arestas (interacoes) na rede
R.web=dim(web)[1]				             	#numero de linhas
C.web=L=dim(web)[2]					          #numero de colunas
Conec.web=E.web/(R.web*C.web) 			  #calculo da conectancia
Conec.web

## Visualizando os resultados de conectancia da rede:

Connectance=data.frame(c(round(Conec.web,2)))
colnames(Connectance)=c("Connectance")
Connectance

## Distribuicao do grau ##

d.web = degreedistr(web) # Menos que 5 pontos

## Entropias ##

source("Entropy.R")
Entropy.web=entropy(web)

## Visualizando os resultados de entropia:

Entropy=data.frame(c(Entropy.web$Row.Entropy, c(Entropy.web$Col.Entropy), row.names=c(""))
                   colnames(Entropy)=c("Animal.Entropy", "Plant.Entropy")
                   Entropy
                   
                   
                   #-----------------------------#
                   # Aninhamento e distancias    #
                   #-----------------------------#
                   
                   ## Aninahmento ##
                   
                   web.NODF=nested(web, method = "NODF2") # Calcula NODF das matrizes
                   
                   # Visualizando os resultados de aninhamento:
                   
                   Nestedness=data.frame(c(round(web.NODF,2)), row.names=c(""))
                   colnames(Nestedness)=c("NODF")
                   Nestedness
                   
                   ## Distancias ##
                   
                   # Criando listas de interaca de uma matriz n x m. Nohs das linhas vao de 0 a n-1. Nohs das colunas vao de n a m-1.
                   
                   edgelist.web=web2edges(web, return=TRUE) 
                   
                   write.table(edgelist.web[,1:2]-1,file="web.edge.txt",row.names=F, col.names=F, append=T, quote=F) #cria o arquivo de edgelist
                   
                   web.igraph=read.graph("web.edge.txt", format="edgelist", direct=FALSE) #le arquivo de edgelist e grava em variavel da classe igraph
                   
                   # Matriz de distancia do noh da linha ao noh da coluna:
                   web.paths=shortest.paths(web.igraph)  
                   web.paths
                   
                   # Caminho medio:
                   path1.len.web=average.path.length(web.igraph) #media dos caminhos da rede
                   
                   Path.1=data.frame(c(round(path1.len.web,2)), row.names=c(""))
                   colnames(Path.1)=c("Caminho.medio1")
                   Path.1
                   
                   # Plot and save the bipartite chart 
                   png(file = "carnidiet_bipartite.png", width = 900, height = 600)
                   plotweb(web, labsize=1.3, text.rot=90, col.high="#6699CC", 
                           col.low="#999933", col.interaction="#999999")
                   dev.off()
                   
                   visweb(web, type="diagonal") 
                   
                   networklevel(web) # Calculates a variety of indices and values for a bipartite network
                   specieslevel(web) # Apart from the properties of the entire web, also its participants can be described specifically. Various simple numbers and indices are calculated and returned
                   nodespec(web) # Calculates a specialisation index based on the node positions for all species in a bipartite network, separately for the higher and lower trophic level
                   (ex <- second.extinct(web, participant="lower", method="random", nrep=50, details=F))
                   slope.bipartite(ex)
                   
                   nullmodel(web, N=1000, method=4)
                   nullmodel(web, N=1000, method=5)
                   
                   res <- computeModules(web)
                   plotModuleWeb(res)
                   PAC(web) # Quantifies, for each pair of lower trophic level species, the potential for showing apparent competition with another species, mediated through the higher trophic level.
                   
                   degreedistr(web, plot.it=T)
                   V.ratio(web)



#####################

