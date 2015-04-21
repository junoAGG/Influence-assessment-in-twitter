###                                         
###     _____     ___ _                     
###    |     |___|  _| |_ _ ___ ___ ___ ___ 
###    |-   -|   |  _| | | | -_|   |  _| -_|
###    |_____|_|_|_| |_|___|___|_|_|___|___|
###                                         
###                                                          
###     _____                                   _      _     
###    |  _  |___ ___ ___ ___ ___ _____ ___ ___| |_   |_|___ 
###    |     |_ -|_ -| -_|_ -|_ -|     | -_|   |  _|  | |   |
###    |__|__|___|___|___|___|___|_|_|_|___|_|_|_|    |_|_|_|
###                                                          
###                                   
###     _____       _ _   _           
###    |_   _|_ _ _|_| |_| |_ ___ ___ 
###      | | | | | | |  _|  _| -_|  _|
###      |_| |_____|_|_| |_| |___|_|  
###
###
###   by Sergey Kirgizov, Lobna Azaza, Nicolas Gastineau and Marinette Savonnet
###   2015, Le2i, Université de Bourgogne
###
###   The Source code for paper "Influence Assessment in Twitter Multi-Relational Network"
###        written by Lobna Azaza, Sergey Kirgizov, Marinette Savonnet, Eric Leclercq

###                                                                    
###     _____                              _   _         _   _         
###    |     |___ ___ ___    ___ ___ _____| |_|_|___ ___| |_|_|___ ___ 
###    | | | | .'|_ -|_ -|  |  _| . |     | . | |   | .'|  _| | . |   |
###    |_|_|_|__,|___|___|  |___|___|_|_|_|___|_|_|_|__,|_| |_|___|_|_|
###

### Read Azaza's table of influence marker compositions
azaza <- read.table ("azaza.txt")

### Define m1 ⊗ m2
### alpha  is a table of 'multiplications'
combination <- function (alpha, m1, m2) {
  names <- names(alpha)
  m1.m2 <- NULL
  for (z in names) {
    alpha.z = alpha == z
    m1.m2.z <- m1  %*%  alpha.z  %*%  m2
    m1.m2 <- c(m1.m2, m1.m2.z)
  }
  names (m1.m2) <- names
  m1.m2 
}


###    m ⊗ m ⊗ .... ⊗ m 
### \—————— n times ———–———/
iteration <- function(alpha, m, n) {
    if (n == 1) return (m)
    
    res <- m
    for (i in 2:n) {
        res <- combination (alpha, res, m)
    }
    res
}

### EXAMPLES OF MASS COMBINATION

## retweets
m.rt <- rep(0.0,dim(azaza)[1])
names(m.rt) <- names(azaza)
m.rt[2] <- 0.4
m.rt[9] <- 0.6

## mentions
m.mention <- rep(0.0,dim(azaza)[1])
names(m.mention) <- names(azaza)
m.mention[1] <- 0.3
m.mention[9] <- 0.7

## 2 retweets + mention
combination(azaza, m.rt, m.rt) -> m.2rt
combination(azaza, m.2rt ,m.mention) 

### We also can define an operator 
'%comb%' <- function (m1, m2) combination(azaza,m1,m2)

### and use it as follows:
m.rt %comb% m.mention %comb% m.rt %comb% m.mention



###                                 
###     _____         _   _         
###    | __  |___ ___| |_|_|___ ___ 
###    |    -| .'|   | '_| |   | . |
###    |__|__|__,|_|_|_,_|_|_|_|_  |
###                            |___|


### READ THE RESULTS OF MASS CALCULATIONS
original.data <- read.table ('./results.txt')


### BEFORE PIGNISTIC DATA
before.pig.data <- original.data[,c(12,4:11)]


### Sort by maximal influence
### and then by the belief mass of that influence
###
### candidate    marker with maximal mass   mass of next marker
###  c₁                          E.Strong   m(E.Strong) = 0.8
###  c₂                          E.Strong   m(E.Strong) = 0.7
###  c₃                          V.Strong   m(V.Strong) = 0.9
###  c₄                          V.Strong   m(V.Strong) = 0.6
###  c₅                          Average    m(Average) = 0.4
###  c₆                          Average    m(Average) = 0.3
###  …
### 
rank.maximal.influence <- function(data) {
  rownames(data) -> influence.markers

  ## calcualte maximal column for each row
  ## (columns represent influence markers
  ##  rows represents candidates)
  max.col (data) -> maximal.cols

  ## sort by influence markers
  ## The order is Ω < V.Weak < Weak < Average.E < Average < Strong.E < Strong < V.Strong < E.Strong 
  sort (maximal.cols, decreasing=TRUE) -> sorted.influence

  
  ## second level sort by belief mass of maximal influence
  cbind(1:length(sorted.influence), sorted.influence) -> indexes
  data[order(sorted.influence,data[indexes],decreasing=TRUE),] -> result

  result
}

### Sort by maximal influence
### and then by the belief mass of the _next_ influence marker.
###
### The ranking will look like
###
### candidate    marker with maximal mass   mass of next marker
###  c₁                          E.Strong   m(E.Strong) = 0.9
###  c₂                          E.Strong   m(E.Strong) = 0.8
###  c₃                          V.Strong   m(E.Strong) = 0.7
###  c₄                          V.Strong   m(E.Strong) = 0.65
###  c₅                          Average    m(Strong.E) = 0.4
###  c₆                          Average    m(Strong.E) = 0.3
###  …
###
rank.maximal.influence2 <- function(data) {
  rownames(data) -> influence.markers

  ## calcualte maximal column for each row
  ## (columns represent influence markers
  ##  rows represents candidates)
  max.col (data) -> maximal.cols

  ## sort by influence markers
  ## The order is Ω < V.Weak < Weak < Average.E < Average < Strong.E < Strong < V.Strong < E.Strong 
  sort (maximal.cols, decreasing=TRUE) -> sorted.influence
  
  ## second level sort by belief mass of maximal influence
  cbind(1:length(sorted.influence), sorted.influence) -> indexes
  ## take the mass of next influence marker

  indexes[indexes[,2]<9,2] <- indexes[indexes[,2]<9,2] + 1
  data[order(sorted.influence,data[indexes],decreasing=TRUE),] -> result

  result
}


### PIGNISTIC PROBA
### ad-hoc method, need to be generalised
pig.data <- before.pig.data[,2:9]
pig.data <- before.pig.data[,2:9] + before.pig.data$Ω / 8


## PLOT
plt <- function(data) {
  par(mar=c(12, 4.1, 4.1, 2.1))
  candidate.names <- rownames(data)
  influence.marker = names(data)
  colors = rainbow(13)
  plot(data$E.Strong, xaxt = 'n', xlab='', type='n', ylim=c(0,1), ylab='Belief mass')
  axis(1, at=1:length(candidate.names), labels=candidate.names, las=2)
  
  for (i in 1:length(influence.marker)) {
    influence = influence.marker[i]
    points(data[[influence]], col=colors[i], type='b')
  }
  
  legend("topright",  pch = 1,
         legend=influence.marker,
         col=colors)
}


### produce 2 plots
cairo_pdf("ranking20.pdf", height=6, width=10)
plt(rank.maximal.influence2(before.pig.data))
dev.off()

cairo_pdf("ranking10.pdf", height=6, width=10)
plt(rank.maximal.influence2(before.pig.data)[1:10,])
dev.off()

### TODO RANKING WITH PIGNISTIC DATA


### Just a 3d plot
## E.Forte vs T.Forte vs Fote
# data <- before.pig.data
# library(rgl)
# plot3d(data$E.Forte, data$T.Forte, data$Forte)
# text3d(data$E.Forte, data$T.Forte, data$Forte, texts = rownames(data))


## live long and prosper 
