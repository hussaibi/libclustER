###############################################################################
### Creates Mapping from edges to weights.
### Returns as data.frame
################################################################################
edge.weight.mapping <- function(g, index.correction=FALSE) {
  return(list(  from=as.integer(get.edges(graph=g, es=1:ecount(g) - 
                  index.correction)[,1]),
                to=as.integer(get.edges(graph=g, es=1:ecount(g) - 
                  index.correction)[,2]),
                weight=E(g)$weight));
}
################################################################################
### Grabs veretices and their weights, and then sorts from heaviest to lightest
### Returns as data.frame
################################################################################
ordered.edge.weight.mapping <- function(g) {
  ewm = edge.weight.mapping(g);
  indexes <- order(ewm$weight, decreasing=TRUE, na.last=TRUE);
  ewm$from = ewm$from[indexes];
  ewm$to = ewm$to[indexes];
  ewm$weight = ewm$weight[indexes];
  return(ewm);
}
################################################################################
### Creates Graph according to CENTER specs. Mode is arbitrary by default
### but, reverts to mean on input error. 
################################################################################
center <- cmpfun(f=function(g, mode = "arbitrary", order = 1,
                   index.correction=FALSE) {
  # for center picking
  vwm = vertex.weight.mapping(g, method = mode, order);
  vwm.weight = as.numeric(vwm[['weight']]);
  # for inspection
  oewm = ordered.edge.weight.mapping(g);
  oewm.from = oewm[['from']];
  oewm.to = oewm[['to']];
  oewm.weight = oewm[['weight']];
  # for clustering
  total.nodes = vcount(g);
  centers = logical(length=total.nodes);
  non.centers = logical(length=total.nodes);
  # for adding edges
  edges.from = c();   edges.to= c();  edges.weight = c();
  # main loop
  for (i in 1:length(oewm.weight)) {
    nodeF = oewm.from[i];         nodeT = oewm.to[i];
    curW = oewm.weight[i];
    if (nodeF != nodeT) {
      # if centers, than add, if non-centers then ignore
      if (centers[nodeF + index.correction] &&
          centers[nodeT + index.correction]) {
        # ignore for center algorithm
      } else if (non.centers[nodeF + index.correction] ||
                 non.centers[nodeT + index.correction]) {
        # ignore since one of them is already associated with another center
      } else if (centers[nodeF + index.correction]) {
        # add to edge list. No new center to add. Add nodeT to non.center
        edges.from = c(edges.from, nodeF);
        edges.to = c(edges.to, nodeT);
        edges.weight = c(edges.weight, curW);
        non.centers[nodeT + index.correction] = TRUE;
      } else if (centers[nodeT + index.correction]) {
        # add to edge list. No new center to add. Add nodeF to non.center
        edges.from = c(edges.from, nodeF);
        edges.to = c(edges.to, nodeT);
        edges.weight = c(edges.weight, curW);
        non.centers[nodeF + index.correction] = TRUE;
      } else { # new nodes
        # add to edge list. Add nodeF to center. Add nodeT to non.center
        edges.from = c(edges.from, nodeF);
        edges.to = c(edges.to, nodeT);
        edges.weight = c(edges.weight, curW);
        if (mode == "arbitrary") {
          centers[nodeF + index.correction] = TRUE;
          non.centers[nodeT + index.correction] = TRUE;
        } else { # use centrality values to decide 
          if (vwm.weight[nodeF + index.correction] >
              vwm.weight[nodeT + index.correction]) {
            centers[nodeF + index.correction] = TRUE;
            non.centers[nodeT + index.correction] = TRUE;
          } else {
            centers[nodeT + index.correction] = TRUE;
            non.centers[nodeF + index.correction] = TRUE;
          }
        }
      }
    }
  }
  tempG = graph(edges=t(matrix( data=c(edges.from, edges.to),
                                nrow=length(edges.from), ncol=2)),
                                directed=FALSE, n=total.nodes);
  E(tempG)$weight = edges.weight;
  return(tempG);
});
################################################################################
### Returns the clusters for CENTER Algorithm
################################################################################
center.cluster <- function(g, mode = "arbitrary", order = 1,
                           index.correction=FALSE) {
  return(igraph.to.clusters(center(g, mode, order, index.correction), index.correction));
}