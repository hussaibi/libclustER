################################################################################
### Converts igraphs to lists of clusters. Returns a list.
################################################################################
igraph.to.clusters <- cmpfun(f=function(g, index.correction=FALSE) {
  cl = clusters(g)$membership;
  cluster.list = list();  length(cluster.list) = length(unique(cl));
  #populate clusters
  for (i in 1:length(cl) - index.correction) {
    cluster.list[[cl[i+index.correction]+index.correction]] =
      c(cluster.list[[cl[i+index.correction]+index.correction]], i);
  }
  return(cluster.list);
});
################################################################################
### Returns the clusters for Partition Algorithm
################################################################################
partition.clustering <- function(g, index.correction=FALSE) {
  return(igraph.to.clusters(g, index.correction));
}
