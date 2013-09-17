################################################################################
### Returns a sparse matrix representation of a graph
################################################################################
igraph.to.Matrix <- function(g) {
  return(get.adjacency(graph=g, type="both", sparse=TRUE, attr='weight'));
}
################################################################################
### Returns ranking based on steady state probabilities.
################################################################################
steady.state.markov <- cmpfun(f=function(g) {
  gMatrix = igraph.to.Matrix(g);
  # ensuring self loops
  #   for (i in c(1:dim(gMatrix)[2])) {
  #     gMatrix[i,i] = 1; # can safely assume score of 1 with self
  #   }
  # normalizing scores to probabilities
  gMatrix = gMatrix / rowSums(gMatrix);
  # grabbing infinite transitions
  change=TRUE;
  while(change != 0) {
    gMatrix2 = gMatrix %*% gMatrix; #faster convergence
    change = sum(rowSums(zapsmall(gMatrix) !=  zapsmall(gMatrix2)));
    gMatrix = gMatrix2;
  }
  # returning ranking
  return(colSums(gMatrix));
});
################################################################################
### Grabs vertices and their 'score'
### The possible scores are neighbourhood mean / sum.
### A random number can also be given as the score.
### Returns as data.frame
################################################################################
vertex.weight.mapping <- function( g, method='mean', order=1)
{
  o.from = 0:{vcount(g)-1}
  if (method == 'sum') {
    return(list(weight=graph.strength(graph=g, loops=FALSE), from=o.from));
  } else if (method == 'degree') {
    return(list(weight=degree(g, loops=FALSE), from=o.from));
  } else if (method == 'random') {
    return(list(weight=runif(vcount(g)), from=o.from));
  } else if (method == "kleinberg") {
    return(list(weight=authority.score(g)$vector, from=o.from));
  } else if (method == "evcent") {
    return(list(weight=evcent(g)$vector, from=o.from));
  } else if (method == "closeness") {
    return(list(weight=closeness.estimate(graph=g, cutoff=order), from=o.from));
  } else if (method == "markov") {
    return(list(weight=steady.state.markov(g), from=o.from));
  } else {
    return(list(  weight=graph.strength(graph=g, loops=FALSE)
                          / degree(g,loops=FALSE), from=o.from));
  }
}
################################################################################
### Grabs vertices and their weights, and then sorts from heaviest to lightest
### Returns as data.frame
################################################################################
ordered.vertex.weight.mapping <- function(g, method='mean', order=1) {
  vwm = vertex.weight.mapping(g, method, order);
  indexes <- order(vwm$weight, decreasing=TRUE, na.last=TRUE);
  vwm$weight = vwm$weight[indexes];
  vwm$from = vwm$from[indexes];
  return(vwm);
}
################################################################################
### Creates cluster list according to STAR specs.
################################################################################
star <- cmpfun(f=function(g, method='mean', overlap=TRUE, order=1,
                    no.subsets=TRUE) {
  # TODO: Remove 'marked' and use vertex attribute 'marked' instead.
  #       Same with centrality measure.
  cutClusters = list();          aCut = c();          V(g)$marked = FALSE;
  #creating an ordering attribute. Keeps consistency upon graph changes
  V(g)$original.name = as.integer(V(g));
  vwm = vertex.weight.mapping(g, method, order);
  V(g)$centrality.ordering = order(vwm$weight, decreasing=TRUE, na.last=TRUE);
  # iterate through centres and grab nodes
  for(a.ordering in 1:vcount(g)){
    v.name = as.integer(V(g)[centrality.ordering==a.ordering]);
    marked = V(g)[v.name]$marked;
    if (length(v.name) > 0) {# if vertex exists (may have been deleted)
      if (marked) {
        # move to next element if already clustered
      } else {
        if (overlap) {
          # grab the cluster
          aCut = neighborhood(graph=g, nodes=v.name, order=1)[[1]];
          V(g)[aCut]$marked = TRUE;
          aCut = V(g)[aCut]$original.name;
          cutClusters = c(cutClusters,list(aCut));
          if (no.subsets) {
            new.verts = as.integer(V(g));
            new.verts = new.verts[new.verts != v.name];
            g = induced.subgraph(graph=g, v=new.verts);
          }
        } else {
          # remove 1.cut to avoid overlap and 2. marked values
          # grab the cluster...
          
          aCut = neighborhood(graph=g, nodes=v.name, order=1)[[1]];
          ## ...without overlapping # forces assertion (not actually necessary)
          #aCut = as.integer(V(g)[aCut][marked==FALSE]);
          ## reduce iterations
          #V(g)[aCut]$marked = TRUE; # pointless since getting induced away
          new.verts = as.integer(V(g));
          new.verts = new.verts[! new.verts %in% aCut];
          aCut = V(g)[aCut]$original.name;
          cutClusters = c(cutClusters,list(aCut));
          g = induced.subgraph(graph=g, v=new.verts);
        }
      }
    }
  }
  return(cutClusters);
});

################################################################################
### Returns the clusters for STAR Algorithm
################################################################################
star.cluster <- function(g, method='mean', overlap=TRUE, no.subsets=TRUE) {
  return(star(g, method, overlap));
}
