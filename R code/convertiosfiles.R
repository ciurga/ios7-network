library(rjson)
library(rCharts)

setwd("C:/Users/debora.nozza/Google Drive/PhD/Big Data Summit/Graph/interactive_network_html_daicazzo/data/csv")

sizes <- c("small","medium","large")
clus <- c("kmeans","network","red")
dataset <- "ios7"

for (s in sizes){
  for (c in clus){
    print(s)
    csvtoJSON(s,dataset,c)
  }
}



csvtoJSON <- function(size,dataset,clu){

    url  <- paste0(size,"_",dataset,"_",clu)
    urljson <- paste0("jsonNet\\",url,".json")
    url <- paste0(url,"_opleads")
    print(url)
    urlEdge <- paste0(size,"_",dataset)


    iosEdge <- read.csv(paste0(urlEdge,"_edges.csv"), sep = "\t")
    names(iosEdge) <- c("source","target","value")
    iosNode <- read.csv(paste0(url,"_nodes.csv"), sep = "\t")
    #iosNode <- iosNode[,1:2]
    #names(iosNode) <- c("name","group")
    names(iosNode)[1] <- c("name")
    iosEdgeNew <- convertiosfiles(iosEdge,iosNode)
    names(iosEdgeNew) <- c("source","target","count")
    names(iosNode)[1] <- c("id")
    iosNode$IsOpinionLeader[iosNode$IsOpinionLeader==1] <- "square"
    iosNode$IsOpinionLeader[iosNode$IsOpinionLeader==0] <- "circle"
    indexOp <- (which(names(iosNode) == "IsOpinionLeader"))
    names(iosNode)[indexOp] <- c("type")
    jsonEdge <- toJSONArray(iosEdgeNew)
    jsonNode <- toJSONArray(iosNode)
    beginJson <- "{\n \"graph\": [], \n \"nodes\":"
    middleJson <- ", \"links\" :"
    endJson <- ",\n \"directed\": false, \n \"multigraph\": false \n}"
    write(beginJson,urljson)
    write(jsonNode,urljson,append = TRUE)
    write(middleJson,urljson,append = TRUE)
    write(jsonEdge,urljson,append = TRUE)
    write(endJson,urljson,append = TRUE)
    
    #write.csv(iosEdgeNew,"small_ios7_idtorow.csv")
}


convertiosfiles <- function(sourceData, targetData){
  
  output <- sourceData
  n <- nrow(sourceData)
  for (i in 1:(n)) {
    #mins <- min(sourceData$source)
    #mint <- min(sourceData$target)
    s <- sourceData$source[i]
    t <- sourceData$target[i]
    news <- which(targetData$name == s) - 1
    newt <- which(targetData$name == t) - 1
    #   print(i)
     #  print(news)
    #print(s )
    output$source[i] <- news#- mins
    output$target[i] <- newt#- mint
    
  }
  
  #print(output$source)
  sortedOutput <- output[order(output$source),]
  #print(sortedOutput)
  
  return(sortedOutput)
}