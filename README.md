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

## Vamos construir uma rede de interação bipartida. 

<img src="https://github.com/fblpalmeira/GEEQ/blob/main/data/carnidiet_bipartite.png">

## Também vamos construir uma rede de interação em forma de grid.

<img src="https://github.com/fblpalmeira/CarniDIET/blob/main/data/felids_diet2.png">

## Referências: 

[Dormann CF, Fruend J, Gruber B. 2021.](http://cran.r-project.org/web/packages/bipartite/bipartite.pdf) Package ‘bipartite’. 

[Middleton, O., Svensson, H., Scharlemann, J. P., Faurby, S., & Sandom, C. (2021).](https://doi.org/10.1111/geb.13296) CarniDIET 1.0: A database of terrestrial carnivorous mammal diets. Global Ecology and Biogeography, 30(6), 1175-1182. https://doi.org/10.1111/geb.13296
