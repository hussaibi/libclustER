################################################################################
### Add edge to adjecency list representation. For optimization reasons, the
### graph operations should be done independant of the graph class.
################################################################################
addEdge.list <- function(from.tags, to.tags, edge.list, edge.weights) {
  edge.list[[from.tags]][['edges']] =
    c(edge.list[[from.tags]][['edges']], to.tags);
  edge.list[[from.tags]][['weights']] =
    c(edge.list[[from.tags]][['weights']], edge.weights);
  # add reverse edge
  if (to.tags != from.tags) { # check self loop
    edge.list[[to.tags]][['edges']] =
      c(edge.list[[to.tags]][['edges']], from.tags);
    edge.list[[to.tags]][['weights']] =
      c(edge.list[[to.tags]][['weights']], edge.weights);
  }
  return(edge.list);
}
################################################################################
### Returns Cluster. Will re-index to start from 0, if index starting from 1.
################################################################################
load.true.cluster <- function(file.path="./../data/5k.csv"
                              , index.correction=FALSE) {
  a.file = read.csv(file.path, header=FALSE);
  the.clusters = list();
  for (a.record in c(1:dim(a.file)[1])) {
    a.id = a.file[a.record, 2];
    the.clusters[[a.id]] = a.id;
  }
  for (a.record in c(1:dim(a.file)[1])) {
    a.id = a.file[a.record, 2];
    a.tid = a.file[a.record, 1];
    if (a.id != a.tid) {
      the.clusters[[a.id]] = c(the.clusters[[a.id]], a.tid);
    }
  }
  output = the.clusters[!as(Map(f=is.null, the.clusters), "logical")];
  index.check = min(as.numeric(lapply(FUN=min, output)))
  if (index.check == 1 && index.correction) {
    #re-indexing starting with zero
    output = lapply(FUN=function(x) x-1, X=output)
  }
  return(output);
}
################################################################################
### Returns Graph. Grabs links from pair data and similarity with
### string data.
################################################################################
load.graph.csv <- function(
                      edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
                      node.path="./../data/5k.csv", sparse=FALSE, knnk=100,
                      index.correction=FALSE)
{
  edge.list = list();
  # grabbing nodes
  a.file = read.csv(node.path, header=FALSE);
  tid.list = as(a.file[[1]], 'character');
  # grabbing edge data
  a.file2 = read.csv(edge.path, header=FALSE);
  tid1.list = as(a.file2[[1]], 'integer');
  tid2.list = as(a.file2[[2]], 'integer');
  score.list = as(a.file2[[3]], 'numeric');
  #sparsify graph
  if (sparse) {
    # sort by weight
    indexes <- order(score.list, decreasing=TRUE, na.last=TRUE);
    tid1.list = tid1.list[indexes];
    from.vertex = as.character(tid1.list);
    tid2.list = tid2.list[indexes];
    to.vertex = as.character(tid2.list);
    score.list = score.list[indexes];
    # sparsing data
    vertex.knn = integer(length(tid.list));
    names(vertex.knn) = as.character(1:length(tid.list))
    to.select = logical(length(score.list));
    for (i in 1:length(score.list)) {
      vertex1 = from.vertex[i];
      vertex2 = to.vertex[i];;
      if (vertex.knn[vertex1] <= knnk && vertex.knn[vertex2] <= knnk){
        vertex.knn[vertex1] = vertex.knn[vertex1] + 1;
        vertex.knn[vertex2] = vertex.knn[vertex2] + 1;
        to.select[i] = TRUE;
      }
    }
    tid1.list = tid1.list[to.select];
    tid2.list = tid2.list[to.select];
    score.list = score.list[to.select];
  }
  #create graph
  g=NULL;
  if (index.correction){
    g = graph(edges=t(matrix( data=c(tid1.list -1, tid2.list -1),
                              nrow=length(tid1.list), ncol=2)), directed=FALSE);
  }
  else {
    g = graph(edges=t(matrix( data=c(tid1.list, tid2.list),
                              nrow=length(tid1.list), ncol=2)), directed=FALSE);
  }
  E(g)$weight = score.list;
  return(g);
}
################################################################################
### This is a helper function for geneticAugmentation and experiment specific.
### Some files were delimited differently.
################################################################################
load.all.scores <- function(data.file.type="cu1", left.join=FALSE)
{
  the.merge.func=merge.outer.scores;
  if(left.join) {
    the.merge.func=merge.left.outer.scores;
  }
  # grabbing score data
  a.file1 = read.csv(paste("./../data/scores_",data.file.type,"_weightedjaccardbm25.csv",
                           sep=""),
                     header=FALSE);
  output = a.file1;
  names(output)[3] = "weightedjaccardbm25";
  
  a.file3 = read.csv(paste("./../data/scores_",data.file.type,"_jaccard.csv",
                              sep=""),
                        header=FALSE);
  output = the.merge.func(output, a.file3,
                                       new.label="jaccard");
  a.file4 = read.csv(paste("./../data/scores_",data.file.type,"_hmm.csv",
                              sep=""),
                        header=FALSE);
  output = the.merge.func(output, a.file4,
                                       new.label="hmm");
  a.file5 = read.csv(paste("./../data/scores_",data.file.type,"_tfidf.csv",
                              sep=""),
                        header=FALSE);
  output = the.merge.func(output, a.file5,
                                       new.label="tfidf");  
  a.file7 = read.csv(paste("./../data/scores_",data.file.type,"_editdistance.csv",
                              sep=""),
                        header=FALSE);
  output = the.merge.func(output, a.file7,
                                       new.label="editdistance");
  for(i in 3:dim(output)[2]){
    output[[i]]=as.numeric(as.vector(output[[i]]));
  }
  return(output);
}
################################################################################
### Helper function to accumulate scores. (not used in experiments due to theta
### thresholding) !!NOT USED!!
################################################################################
merge.outer.scores <- function(df1, df2, new.label="newlabel")
{
  names(df2)[3] = new.label;
  return(merge(df1, df2, all=FALSE));
}
################################################################################
### Helper function to accumulate scores for a specific set of edges.
################################################################################
merge.left.outer.scores <- function(df1, df2, new.label="newlabel")
{
  names(df2)[3] = new.label;
  return(merge(df1, df2, all.x=TRUE));
}
