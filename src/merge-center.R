################################################################################
### Creates Graph according to MERGE-CENTER specs.
################################################################################
merge.center <- cmpfun(f=function(g, index.correction=FALSE) {
  # for inspection
  oewm = ordered.edge.weight.mapping(g);
  oewm.from = oewm[['from']];
  oewm.to = oewm[['to']];
  oewm.weight = oewm[['weight']];
  # for clustering
  total.nodes = vcount(g);
  centers = !as.logical(1:total.nodes);
  non.centers = !as.logical(1:total.nodes);
  # for adding edges
  edges.from = c();   edges.to= c();  edges.weight = c();
  # main loop
  for (i in 1:length(oewm.weight)) {
    nodeF = oewm.from[i];         nodeT = oewm.to[i];
    curW = oewm.weight[i];
    if (nodeF != nodeT) {
      # if centers, than add, if non-centers then ignore
      if (centers[nodeF+index.correction] && centers[nodeT+index.correction]) {
        # only condition for two clusters to merge is when their centers
        # are connected
        edges.from = c(edges.from, nodeF);
        edges.to = c(edges.to, nodeT);
        edges.weight = c(edges.weight, curW);
      } else if (non.centers[nodeF+index.correction] || non.centers[nodeT+index.correction]) {
        # ignore since one of them is already associated with another center
      } else if (centers[nodeF+index.correction]) {
        # add to edge list. No new center to add. Add nodeT to non.center
        edges.from = c(edges.from, nodeF);
        edges.to = c(edges.to, nodeT);
        edges.weight = c(edges.weight, curW);
        non.centers[nodeT+index.correction] = TRUE;
      } else if (centers[nodeT+index.correction]) {
        # add to edge list. No new center to add. Add nodeF to non.center
        edges.from = c(edges.from, nodeF);
        edges.to = c(edges.to, nodeT);
        edges.weight = c(edges.weight, curW);
        non.centers[nodeF+index.correction] = TRUE;
      } else { # new nodes
        # add to edge list. Add nodeF to center. Add nodeT to non.center
        edges.from = c(edges.from, nodeF);
        edges.to = c(edges.to, nodeT);
        edges.weight = c(edges.weight, curW);
        centers[nodeF+index.correction] = TRUE;
        non.centers[nodeT+index.correction] = TRUE;
      }
    } else {
      # this case ignores self-loops. It is very much possible for a node to
      # be labled neither center nor non-center. This is a result of all its
      # neighbours being taken by other centers, but not being connected to
      # those centers. In this case, the node becomes a singleton, even though
      # it is weakly connected.
    }
  }
  tempG = graph(edges=t(matrix( data=c(edges.from, edges.to),
                                nrow=length(edges.from), ncol=2)),
                                directed=FALSE, n=total.nodes);
  E(tempG)$weight = edges.weight;
  return(tempG);
});
################################################################################
### Returns the clusters for MERGE-CENTER Algorithm
################################################################################
merge.center.cluster <- function(g, index.correction=FALSE) {
  return(igraph.to.clusters(merge.center(g, index.correction), index.correction));
}
