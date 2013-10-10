################################################################################
### Will round to zero and one for appropriate values.
################################################################################
hard.cluster.Matrix <- function(m, limit=0.01) {
  m[m <= limit] <- 0; # cause Stjin told me so...
  m[m > 0] <- 1; # trust me... This is not a redundant step.
  return(m);
}
################################################################################
### Returns column max
################################################################################
rowMax <- function(m) {
  a.function <- function(i) {
    return(max(m[i,]));
  }
  return(as.numeric(lapply(X=1:dim(m)[1], FUN=a.function)));
}
################################################################################
### Will sparsify the matrix (along the columns)
################################################################################
sparsify.matrix <- cmpfun(f=function(m, k=100) {
  # Doesn't ignore self loops
  g <- graph.adjacency(adjmatrix=m, mode="directed", weighted=TRUE
                       , diag=TRUE);
  # sort by weight
  oewm = ordered.edge.weight.mapping(g);
  tid1.list = oewm[['from']] + 1;
  from.vertex = as.character(tid1.list);
  tid2.list = oewm[['to']] + 1;
  to.vertex = as.character(tid2.list);
  score.list = oewm[['weight']];
  # sparsing data
  vertex.knn = integer(vcount(g));
  names(vertex.knn) = as.character(1:vcount(g))
  to.select = logical(length(score.list));
  for (i in 1:ecount(g)) {
    vertex1 = from.vertex[i];
    vertex2 = to.vertex[i];
    if (vertex.knn[vertex1] <= k && vertex.knn[vertex2] <= k){
      vertex.knn[vertex1] = vertex.knn[vertex1] + 1;
      vertex.knn[vertex2] = vertex.knn[vertex2] + 1;
      to.select[i] = TRUE;
    }
  }
  tid1.list = tid1.list[to.select];
  tid2.list = tid2.list[to.select];
  score.list = score.list[to.select];
  #create graph
  g <- graph(edges=t(matrix( data=c(tid1.list - 1, tid2.list - 1),
                            nrow=length(tid1.list), ncol=2)), directed=FALSE);
  E(g)$weight <- score.list;
  return(igraph.to.Matrix(g));
});
################################################################################
### Returns the output of the gamma operator.
################################################################################
matrix.gamma <- function(m, r=2) {
  m.r = m^r;
  return(m.r / rowSums(m.r));
}
################################################################################
### Returns the determined clusters for MCL Algorithm (self loops assumed)
################################################################################
markov <- cmpfun(f=function(g,r=2, k=100, prune=FALSE, limit=0.01) {
  m_1 = igraph.to.Matrix(g);
  # making stochastic(normalizing scores to probabilities)
  m_1 = matrix.gamma(m_1, 1);
  chaos = 1;
  while(chaos > 0.001) {
    if (prune) {
      tmp = sparsify.matrix(m_1, k);
      m_2 = tmp %*% tmp;
    } else {
      m_2 = m_1 %*% m_1;
    }
    m_1 = matrix.gamma(m_2, r);
    chaos = max(rowMax(m_1) - rowSums(m_1^2));
  }
  # this is the key step that makes the algorithm properly cluster...
  m_1 = hard.cluster.Matrix(m_1, limit); #...and run more quickly
  return(graph.adjacency(m_1, mode="undirected"));
});
################################################################################
### The MCL Algorithm
################################################################################
markov.cluster <- function(g,r=2, k=100, prune=FALSE, limit=0.01) {
  return(igraph.to.clusters(markov(g,r,k,prune, limit)));
}
