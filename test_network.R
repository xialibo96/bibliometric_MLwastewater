
library(bibliometrix)
load("D:/Desktop/project/bibli/machine_wastewater/Bibliometrix-Export-File-2023-03-06.RData")

NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")
net=networkPlot(NetMatrix, n = 30, Title = "collaboration", type = "auto", 
                size.cex=TRUE, size=15, remove.multiple=FALSE, labelsize=1,edgesize = 10, edges.min=5)

NetMatrix <- biblioNetwork(M, analysis = "coupling", 
                           network = "references", sep = ";")
net <- networkPlot(NetMatrix, n = 20, type = "sphere", Title = "Co-occurrence Network",labelsize=1) 

NetMatrix <- biblioNetwork(M, analysis = "coupling", 
                           network = "authors", sep = ";")
net <- networkPlot(NetMatrix, n = 20, type = "sphere", Title = "Co-occurrence Network",labelsize=1) 

NetMatrix <- biblioNetwork(M, analysis = "coupling", 
                           network = "sources", sep = ";")
net <- networkPlot(NetMatrix, n = 20, type = "sphere", Title = "Co-occurrence Network",labelsize=1) 

NetMatrix <- biblioNetwork(M, analysis = "coupling", 
                           network = "countries", sep = ";")
net <- networkPlot(NetMatrix, n = 20, type = "sphere", Title = "Co-occurrence Network",labelsize=1) 






