
library(bibliometrix)
file <- c("D:\\Desktop\\project\\bibli\\machine_wastewater\\machinelearning_wqastewater.txt")

M <- convert2df(file = file, dbsource = "wos", format = "plaintext")

results <- biblioAnalysis(M, sep = ";")

S <- summary(object = results, k = 10, pause = FALSE)

plot(x = results, k = 10, pause = FALSE)

#####网络分析
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ";")

# Create a country collaboration network

M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "auto", size=TRUE, remove.multiple=FALSE,labelsize=0.8)

# Create a co-citation network

NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", n=30, sep = ";")

# Plot the network
net=networkPlot(NetMatrix, Title = "Co-Citation Network", type = "fruchterman", size=T, remove.multiple=FALSE, labelsize=0.7,edgesize = 5)

# Create keyword co-occurrences network

NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# Plot the network
net=networkPlot(NetMatrix, normalize="association", weighted=T, n = 30, Title = "Keyword Co-occurrences", type = "fruchterman", size=T,edgesize = 5,labelsize=0.7)


# Conceptual Structure using keywords (method="CA")

CS <- conceptualStructure(M,field="ID", method="MCA", minDegree=10, clust=5, stemming=FALSE, labelsize=15, documents=20, graph=FALSE)
plot(CS$graph_terms)

plot(CS$graph_dendogram)

####历史
histResults <- histNetwork(M, sep = ";")

net <- histPlot(histResults, n=20, size = FALSE,label="short")



net <- networkPlot(NetMatrix, n = 30, type = "kamada", Title = "Co-Citation",labelsize=1)
net2VOSviewer(net)
#####三个领域的图
threeFieldsPlot(M, fields=c("DE","AU","SO"),n=c(10,10,20))
threeFieldsPlot(M, fields=c("AU_CO","AU","DE"),n=c(10,10,10))
threeFieldsPlot(M, fields=c("AU_UN","AU","SO"),n=c(10,10,20))
threeFieldsPlot(M, fields=c("DE","AU","ID"),n=c(10,10,10))

#####将viewer中的图保存为html，然后转为pdf
library(webshot)
webshot("1111.html" , "1111.pdf")
webshot("2222.html" , "2222.pdf")
webshot("3333.html" , "3333.pdf")
webshot("4444.html" , "4444.pdf")
####作者的文章随时间变化图
res <- authorProdOverTime(M, k=20)
print(res$dfAU)
plot(res$graph)
####布劳德图
BR <- bradford(M)
BR
###画不同年份的进化图
years=c(2014,2017,2020)
nexus <- thematicEvolution(M,field="ID", years=years, n=250,minFreq=5,cluster = "walktrap")
plotThematicEvolution(nexus$Nodes,nexus$Edges)

#### Field Tag distribution by Year 画词随时间变化
timespan=c(2005,2015)
res <- fieldByYear(M, field = "ID", min.freq = 5, n.items = 2,graph = TRUE)


res <- couplingMap(M, analysis = "authors", field = "ID", n = 150,label.term="ID", impact.measure="local",
                   minfreq = 10, size = 0.5,n.labels=4, repel = TRUE)

plot(res$map)

#####
res <- thematicMap(M, field = "ID", n = 100, minfreq = 20, size = 0.5, repel = TRUE, n.labels=3)
plot(res$map)

####plotting conceptual structure map of a scientific field
CS <- conceptualStructure(M, method="MCA", field="ID", minDegree=15, clust='auto', stemming=FALSE, labelsize=15,documents=5)

CS <- conceptualStructure(M, method="CA", field="ID", minDegree=15, clust='auto', stemming=FALSE, labelsize=15,documents=5)

CS <- conceptualStructure(M, method="MDS", field="ID", minDegree=15, clust='auto', stemming=FALSE, labelsize=15,documents=5)



net <- collabByRegionPlot(NetMatrix, edgesize = 4, label.cex = TRUE, labelsize=2.5,
                          weighted = TRUE, size=0.5, size.cex=TRUE, community.repulsion = 0,
                          verbose=FALSE)
cbind(names(net))
plot(net[[4]]$graph)




