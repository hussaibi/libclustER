################################################################################
### Helper function for Min-Cut Algorithm. Calculates s-t path.
################################################################################
graph.path <- function(graph, source.vertex,
                                target.vertex){
  e=new.env(); # Important: this acts as a pointer to anything stored in it.
  e$path = c();
  e$target.vertex = as.integer(target.vertex) - 1;
  tmp = graph.dfs(graph, root=source.vertex, unreachable=FALSE,
            #builds path in rho but eliminates search as soon as s-t
            #path is found.
             in.callback=function(graph, data, extra)
               {
                 e$path=c(e$path, data["vid"]);
                 if(data["vid"] == e$target.vertex)
                  {
                   #in C, indexing starts with zero.
                   #No root is indicated by NULL
                   #in R, indexing starts with 1.
                   #No root is indicated by 0.
                   e$path = e$path + 1;
                   return(TRUE);
                  }
                 else {return(FALSE);}
               },
             out.callback=function(graph, data, extra)
               { 
                 # ensures only an s-t path is stored
                 e$path=e$path[1:(length(e$path)-1)]; 
                 return(FALSE);
               },
            # This is important. It links the igraph c library's
            # environment with the environment of graph.path(). By using the
            # environment e, we are now able to grab at the computed path.
             rho = environment()
            );
  output = as.character(unname(e$path));
  if (length(output) <= 1) { # invalid or garbage path
    return(NULL);
  } else {
    return(output);
  }
}
################################################################################
### Helper function for Min-Cut Algorithm. Pushes flow along path.
################################################################################
push.flow<-function(graph, path){
  # grab edges of path
  path.edges = E(graph=graph, path=path);
  # pushed flow, and re-set weights/capacities for path in actual graph.
  graph = set.edge.attribute(graph=graph, name='weight',
                     index=path.edges,
                     value=path.edges$weight - min(path.edges$weight));
  # delete zero capacity edge
  graph = delete.edges(graph=graph, 
               edges=as.numeric(E(graph)[E(graph)$weight == 0]))
  return(graph);
}

################################################################################
### Helper function for Min-Cut Algorithm. Returns min.cut
################################################################################
ford.fulkerson.source.partition <- cmpfun(f=function(graph, source.vertex,
                                                     target.vertex)
{
  E(graph)$flow<-0;
  augment.path = graph.path(graph, source.vertex, target.vertex);
  while(!is.null(augment.path)){
    graph <- push.flow(graph, path=augment.path);
    augment.path = graph.path(graph, source.vertex, target.vertex);
  }
  #grab source partition
  #WARNING: returns vertex ids as integers instead of characters.
  the.parted = igraph.to.clusters(graph);
  return(the.parted[as.logical(lapply(FUN=function(x) source.vertex %in% x,
                                      the.parted))][[1]]);
});
################################################################################
### Helper function for Min-Cut Algorithm. Returns pseudo s-t cut
################################################################################
min.cut.with.pseudo.vertex <- function(graph, source.vertex, alpha=0.1) {
  # 1a. Add pseudo node
  pseudo.graph = add.vertices(graph, nv=1);
  target.vertex=max(V(pseudo.graph));
  # 1b. connect pseudo node
  pseudo.graph[target.vertex]<-alpha;  
  # 3. Return source partition
  return(ford.fulkerson.source.partition(graph=pseudo.graph,
                                         source.vertex, target.vertex));
}
################################################################################
### Helper function for Min-Cut Algorithm. Applies Min-Cut
################################################################################
min.cut <- cmpfun(f=function(g, method='mean', overlap=TRUE, order=1, alpha=0.1,
                    no.subsets=TRUE) {
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
            aCut = min.cut.with.pseudo.vertex(g, source.vertex=v.name, alpha);
            V(g)[aCut]$marked = TRUE;
            aCut = V(g)[aCut]$original.name;
            cutClusters = c(cutClusters,list(aCut));
            if (no.subsets) {
              g = delete.vertices(graph=g, v=v.name);
            }
          } else {
            # remove 1.cut to avoid overlap and 2. marked values
            # grab the cluster...
            
            aCut = min.cut.with.pseudo.vertex(g, source.vertex=v.name, alpha);
            ## ...without overlapping # forces assertion (not actually necessary)
            #aCut = as.integer(V(g)[aCut][marked==FALSE]);
            ## reduce iterations
            #V(g)[aCut]$marked = TRUE; # pointless since getting induced away
            
            #new.verts = as.integer(V(g));
            #new.verts = new.verts[! new.verts %in% aCut];
            
            orig.aCut = V(g)[aCut]$original.name;
            cutClusters = c(cutClusters,list(orig.aCut));
            # trying deleting instead of inducing subgraph
            #     (should reduce memory consumption by a lot)
            g = delete.vertices(graph=g, v=aCut);
          }
        }
      }
    }
    return(cutClusters);
});
################################################################################
### Implements Min-Cut Algorithm.
################################################################################
min.cut.cluster <- function(graph, method='mean', overlap=TRUE, order=1,
                            alpha=0.1, no.subsets=TRUE) {
  return(min.cut(graph, method, overlap, order, alpha, no.subsets));
}
