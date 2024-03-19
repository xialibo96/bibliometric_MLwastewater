library(bibliometrix)
load("D:/Desktop/project/bibli/machine_wastewater/Bibliometrix-Export-File-2023-03-06.RData")

results <- biblioAnalysis(M, sep = ";")
options(width=100)
S <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)

CR <- citations(M, field = "article", sep = ";")
cbind(CR$Cited[1:10])

CR <- citations(M, field = "author", sep = ";")
cbind(CR$Cited[1:10])

CR <- localCitations(M, sep = ";")
CR$Authors[1:10,]
CR$Papers[1:10,]

DF <- dominance(results, k = 10)
DF

indices <- Hindex(M, field = "author", elements="LIU YQ", sep = ";", years = 10)

# Bornmann's impact indices:
indices$H

indices$CitationList

authors=gsub(","," ",names(results$Authors)[1:10])

indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 50)

indices$H

###Top-Authors’ Productivity over the Time
topAU <- authorProdOverTime(M, k = 10, graph = TRUE)
head(topAU$dfAU)
head(topAU$dfPapersAU)

L <- lotka(results)
# Author Productivity. Empirical Distribution
L$AuthorProd
L
# Observed distribution
Observed=L$AuthorProd[,3]
# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))

plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty = c(1,1,1),cex=0.6,bty="n")

####来源网络
A <- cocMatrix(M, Field = "SO", sep = ";")
sort(Matrix::colSums(A), decreasing = TRUE)[1:5]
####引用网络
A <- cocMatrix(M, Field = "CR", sep = ".  ")
####作者网络
A <- cocMatrix(M, Field = "AU", sep = ";")


#####将M改为M1，因为M中没有作者国家这个属性
M1 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
####作者国家网络
A <- cocMatrix(M1, Field = "AU_CO", sep = ";")

####关键词网络
A <- cocMatrix(M, Field = "DE", sep = ";")
#####关键词plus网络
A <- cocMatrix(M, Field = "ID", sep = ";")


####  Bibliographic coupling
NetMatrix <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ".  ")


NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
netstat <- networkStat(NetMatrix)
names(netstat$network)

summary(netstat, k=10)

NetMatrix <- biblioNetwork(M1, analysis = "collaboration", network = "countries", sep = ";")
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", 
                type = "circle", size=TRUE, remove.multiple=FALSE,labelsize=0.7,cluster="none")

####共引网络
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

net=networkPlot(NetMatrix, n = 30, Title = "Co-Citation Network", type = "fruchterman", 
                size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)

#####关键词共现

# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", 
                type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)
####按照biblioshiny设置参数

net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", 
                type = "auto", cluster = 'walktrap',size=T,edgesize = 5,labelsize=0.7)



histResults <- histNetwork(M, sep = ";")
net <- histPlot(histResults, n=20, size = 5, labelsize = 4)





