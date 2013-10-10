################################################################################
### Creates cluster list according to Sequential Ricochet specs. Includes 
### singleton fix
################################################################################
ricochet.SR <- cmpfun(f=function(g, method="mean") {
  V(g)$original.name = as.integer(V(g));
  cutClusters =  as.list((1:vcount(g))[neighborhood.size(graph=g, 
                                                         nodes=1:vcount(g), 
                                                         order=1) == 1]);
  # remove self loops and singletons
  g = delete.vertices(graph=g, 
                      v=((1:vcount(g))[neighborhood.size(graph=g, 
                                                         nodes=1:vcount(g), 
                                                         order=1) == 1]));
  g = igraph::simplify(g);
  # pre-clustering singletons with no neighbours
  aCut = c();          V(g)$marked = FALSE;
  #creating an ordering attribute. Keeps consistency upon graph changes
  vwm = vertex.weight.mapping(g, method, order=1);
  V(g)$centrality.ordering = order(vwm$weight, decreasing=TRUE, na.last=TRUE);
  V(g)[1:vcount(g)]$centroid = FALSE;
  #setting up initial mark
  v.name = as.integer(V(g)[centrality.ordering==1]);
  V(g)[1:vcount(g)]$marked = v.name;
  V(g)[v.name]$centroid = TRUE;
  # iterate through centres and grab nodes
  for(a.ordering in 2:vcount(g)){
    v.name = as.integer(V(g)[centrality.ordering==a.ordering]);
    #print(list("name", v.name));
    prior.mark = V(g)[v.name]$marked;
    #print(list("associated centroid", prior.mark));
    # grab the cluster
    aCut = neighborhood(graph=g, nodes=v.name, order=1)[[1]];
    #print(list("nei", aCut));
    # remove candidate centroid from cut
    aCut = aCut[!aCut %in% v.name];
    if (length(aCut) > 0) { # skip singletons (simple case)
      # remove centroids from cut
      aCut = aCut[!V(g)[aCut]$centroid];
      # steal from non-directly connected centroids by ...
      prior.Cut.marks = V(g)[aCut]$marked;
      #1. check esistence of edge between node and current centroid. If none, no problem stealing.
      edge.exists = as.logical((Map(f=function(x,y){are.connected(g, x, y)}, 
                                    aCut, prior.Cut.marks)));
      #2. if edge exists, then steal away (if more strongly connected to candidate)
      vertices.to.compare = aCut[edge.exists];
      prior.Cut.marks = V(g)[vertices.to.compare]$marked;
      association.to.candidate = as.logical((Map(f=function(x,y)
      {
        E(g)[from(v.name) & to(x)]$weight > E(g)[from(x) & to(y)]$weight
      },
                                                 vertices.to.compare,
                                                 prior.Cut.marks)));
      # Assign centroid. (even if singleton by the end)
      V(g)[c(association.to.candidate,
             aCut[!edge.exists], v.name)]$marked = v.name;
    } else {
      # ignore since singleton
      next;
    }
    # Assign centroid status
    V(g)[v.name]$centroid = TRUE;
    
    #print("cleaning up singles");
    # detect singleton centroids by checking occurence frequency
    current.centroids = as.integer(V(g)[V(g)$centroid == TRUE]);
    current.centroid.freq = table(as.integer(V(g)$marked));
    singleton.centroids = current.centroids[current.centroid.freq <= 1];
    #print("Grabbed singles");
    # reassign to most similar neighbour's centroid. 
    # NOTE: it is possible to previously create two singletons that will end up 
    # merging. As such, we need to check the frequency of the most similar
    # neighbour's centroid as well and skip them if no longer singleton.
    centroids.to.skip=c();
    for (a.singleton in singleton.centroids[!is.na(singleton.centroids)]) {
      if (! a.singleton %in% centroids.to.skip) {
        a.singleton.Cut = neighborhood(graph=g, nodes=a.singleton,
                                       order=1)[[1]];
        a.singleton.Cut.edges = E(g)[from(a.singleton)];
        #print(list("grabbing singleton edges", a.singleton.Cut.edges));
        # don't consider self
        a.singleton.Cut = a.singleton.Cut[!a.singleton.Cut %in% a.singleton];
        #print(list("grabbing singleton neighbours", a.singleton.Cut));
        # grab most similar
        most.similar.vertex = a.singleton.Cut[which.max(
          a.singleton.Cut.edges$weight)];
        #print(list("Most similar node", most.similar.vertex));
        most.similar.centroid = V(g)[most.similar.vertex]$marked;
        #print(list("Most similar centroid", most.similar.centroid));
        #adding to singletons to skip (doesn't matter if centroid not a 
        # singleton)
        centroids.to.skip = unique(c(centroids.to.skip, most.similar.centroid));
        #print(list("skipping", centroids.to.skip));
        #print("assigning singleton");
        V(g)[a.singleton]$marked = most.similar.centroid;
        V(g)[a.singleton]$centroid = FALSE;
      }
    }
  }
  # form clusters from labels
  cutClusters2 = list();
  label.marks = unique(as.character(V(g)$marked));
  length(cutClusters2) = length(label.marks);
  names(cutClusters2) = label.marks;
  for (i in as.integer(V(g))){
    cutClusters2[[as.character(V(g)[i]$marked)]] = c(
      cutClusters2[[as.character(V(g)[i]$marked)]], V(g)[i]$original.name);
  }
  return(c(cutClusters, unname(cutClusters2)));
});

################################################################################
### Creates cluster list according to Balanced Sequential Ricochet specs. 
### Includes singleton fix
################################################################################
ricochet.BSR <- cmpfun(f=function(g, method="mean") {
  V(g)$original.name = as.integer(V(g));
  cutClusters =  as.list((1:vcount(g))[neighborhood.size(graph=g, 
                                                         nodes=1:vcount(g), 
                                                         order=1) == 1]);
  # remove self loops and singletons
  g = delete.vertices(graph=g, 
                      v=((1:vcount(g))[neighborhood.size(graph=g, 
                                                         nodes=1:vcount(g), 
                                                         order=1) == 1]));
  g = igraph::simplify(g);
  # pre-clustering singletons with no neighbours
  aCut = c();          V(g)$marked = FALSE;
  #creating an ordering attribute. Keeps consistency upon graph changes
  vwm = vertex.weight.mapping(g, method, order=1);
  balanced.vwm = vwm;
  V(g)$centrality.ordering = order(balanced.vwm$weight, decreasing=TRUE,
                                   na.last=TRUE);
  V(g)[1:vcount(g)]$centroid = FALSE;
  V(g)[1:vcount(g)]$visited = FALSE;
  #setting up initial mark
  v.name = as.integer(V(g)[centrality.ordering==1]);
  V(g)[1:vcount(g)]$marked = v.name;
  V(g)[v.name]$centroid = TRUE;
  V(g)[v.name]$visited = TRUE;
  reassignment=TRUE;
  # Calculate ratio for picking next centroid (balanced centrality)
  centroids = as.integer(V(g)[centroid]);
  if ( length(centroids) == 1 ) {
    balanced.vwm$weight = vwm$weight[vwm$from] / g[vwm$from,centroids];
  } else {
    balanced.vwm$weight = vwm$weight[vwm$from] / g[vwm$from,centroids];
  }
  V(g)$centrality.ordering = order(balanced.vwm$weight, decreasing=TRUE,
                                   na.last=TRUE);
  # iterate through centroid candidates and grab nodes
  for(a.ordering in 2:vcount(g)){
    reassignment=FALSE;    
    #calc. balanced sequential ordering
    centroids = as.integer(V(g)[centroid]);
    if ( length(centroids) == 1 ) {
      balanced.vwm$weight = vwm$weight[vwm$from] / g[vwm$from,centroids];
    } else {
      balanced.vwm$weight = rowSums(vwm$weight[vwm$from] / g[vwm$from,centroids]);
    }
    V(g)$centrality.ordering = order(balanced.vwm$weight, decreasing=TRUE,
                                     na.last=TRUE);
    #grabbing max, non-visited vertex
    v.name = V(g)[!visited];
    v.name = as.integer(v.name[centrality.ordering == 1])[1];
    V(g)[v.name]$visited = TRUE;
    #print(list("name", v.name));
    prior.mark = V(g)[v.name]$marked;
    #print(list("associated centroid", prior.mark));
    # grab the cluster
    aCut = neighborhood(graph=g, nodes=v.name, order=1)[[1]];
    #print(list("nei", aCut));
    # remove candidate centroid from cut
    aCut = aCut[!aCut %in% v.name];
    if (length(aCut) > 0) { # skip singletons (simple case)
      # remove centroids from cut
      aCut = aCut[!V(g)[aCut]$centroid];
      # steal from non-directly connected centroids by ...
      prior.Cut.marks = V(g)[aCut]$marked;
      #1. check esistence of edge between node and current centroid. If none, no problem stealing.
      edge.exists = as.logical((Map(f=function(x,y){are.connected(g, x, y)}, 
                                    aCut, prior.Cut.marks)));
      #2. if edge exists, then steal away (if more strongly connected to candidate)
      vertices.to.compare = aCut[edge.exists];
      prior.Cut.marks = V(g)[vertices.to.compare]$marked;
      association.to.candidate = as.logical((Map(f=function(x,y)
      {
        E(g)[from(v.name) & to(x)]$weight > E(g)[from(x) & to(y)]$weight
      },
                                                 vertices.to.compare,
                                                 prior.Cut.marks)));
      # Assign centroid. (even if singleton by the end)
      V(g)[c(association.to.candidate,
             aCut[!edge.exists], v.name)]$marked = v.name;
      if (length(c(association.to.candidate, aCut[!edge.exists], v.name)) > 1){
        reassignment=TRUE;
      }
    } else {
      # no reassignments since singleton
      # HOWEVER, may not link back to original centroid when cleaning
      # HOWEVER, due to pre cleaning of singleton's, this should never happen.
      next;
    }
    # Assign centroid status
    V(g)[v.name]$centroid = TRUE;
    
    #print("cleaning up singles");
    # detect singleton centroids by checking occurence frequency
    current.centroids = as.integer(V(g)[V(g)$centroid == TRUE]);
    current.centroid.freq = table(as.integer(V(g)$marked));
    singleton.centroids = current.centroids[current.centroid.freq <= 1];
    #print("Grabbed singles");
    # reassign to most similar neighbour's centroid. 
    # NOTE: it is possible to previously create two singletons that will end up 
    # merging. As such, we need to check the frequency of the most similar
    # neighbour's centroid as well and skip them if no longer singleton.
    centroids.to.skip=c();
    for (a.singleton in singleton.centroids[!is.na(singleton.centroids)]) {
      if (! a.singleton %in% centroids.to.skip) {
        a.singleton.Cut = neighborhood(graph=g, nodes=a.singleton,
                                       order=1)[[1]];
        a.singleton.Cut.edges = E(g)[from(a.singleton)];
        #print(list("grabbing singleton edges", a.singleton.Cut.edges));
        # don't consider self
        a.singleton.Cut = a.singleton.Cut[!a.singleton.Cut %in% a.singleton];
        #print(list("grabbing singleton neighbours", a.singleton.Cut));
        # grab most similar
        most.similar.vertex = a.singleton.Cut[which.max(
          a.singleton.Cut.edges$weight)];
        #print(list("Most similar node", most.similar.vertex));
        most.similar.centroid = V(g)[most.similar.vertex]$marked;
        #print(list("Most similar centroid", most.similar.centroid));
        #adding to singletons to skip (doesn't matter if centroid not a 
        # singleton)
        centroids.to.skip = unique(c(centroids.to.skip, most.similar.centroid));
        #print(list("skipping", centroids.to.skip));
        #print("assigning singleton");
        V(g)[a.singleton]$marked = most.similar.centroid;
        V(g)[a.singleton]$centroid = FALSE;
      }
    }
    if (!reassignment) {
      break;
    }
  }
  # form clusters from labels
  cutClusters2 = list();
  label.marks = unique(as.character(V(g)$marked));
  length(cutClusters2) = length(label.marks);
  names(cutClusters2) = label.marks;
  for (i in as.integer(V(g))){
    cutClusters2[[as.character(V(g)[i]$marked)]] = c(
      cutClusters2[[as.character(V(g)[i]$marked)]], V(g)[i]$original.name);
  }
  return(c(cutClusters, unname(cutClusters2)));
});

################################################################################
### Subroutine for OCR.
################################################################################
propogate.ripple.OCR <- cmpfun(f=function(graph, index, clusters,
                                          clustered.node.list){
  centroid.change=FALSE;
  eids = as.integer(E(graph)[the.ordering==index]);
  ripple.ordering = as.integer(E(graph)[eids]$ripple.ordering);
  es.matrix = get.edges(graph=graph, es=E(graph)[eids]);
  all.v = es.matrix[ripple.ordering, 1];
  all.w = es.matrix[ripple.ordering, 2];
  for(a.ordering in 1:(dim(es.matrix)[1])){
    v = all.v[a.ordering];
    w = all.w[a.ordering];
    v.previously.clustered=FALSE
    #check if already clustered
    v.previously.clustered = V(graph)[v]$original.name %in% clustered.node.list;
    if (!v.previously.clustered){
      if(V(graph)[w]$centroid) {
        # check which one has greater weight (we check ordering instead)
        if(V(graph)[w]$centrality.ordering < V(graph)[v]$centrality.ordering){
          #so, w has higher order than v, add v to w' cluster
          clusters[[as.character(V(graph)[w]$original.name)]] = append(
            x = clusters[[as.character(V(graph)[w]$original.name)]],
            value = V(graph)[v]$original.name);
          #add v's cluster to w's cluster
          clusters[[as.character(V(graph)[w]$original.name)]] = append(
            x = clusters[[as.character(V(graph)[w]$original.name)]],
            value = clusters[[as.character(V(graph)[v]$original.name)]]);
          # emptying v's cluster
          clusters[[as.character(V(graph)[v]$original.name)]] = c();
          #remove centroid status
          if(V(graph)[v]$centroid){
            V(graph)[v]$centroid = FALSE;
            centroid.change=TRUE;
          }
        } else { # same thing as before but with v and w switched
          clusters[[as.character(V(graph)[v]$original.name)]] = append(
            x = clusters[[as.character(V(graph)[v]$original.name)]],
            value = V(graph)[w]$original.name);
          clusters[[as.character(V(graph)[v]$original.name)]] = append(
            x = clusters[[as.character(V(graph)[v]$original.name)]],
            value = clusters[[as.character(V(graph)[w]$original.name)]]);
          clusters[[as.character(V(graph)[w]$original.name)]] = c();
          # can assume loss of status
          V(graph)[w]$centroid = FALSE;
          centroid.change=TRUE;
        }
      } else {
        clusters[[as.character(V(graph)[v]$original.name)]] = append(
          x = clusters[[as.character(V(graph)[v]$original.name)]],
          value = V(graph)[w]$original.name);
        if(!V(graph)[v]$centroid){
          V(graph)[v]$centroid = TRUE;
          centroid.change=TRUE;
        }
      }
    }
    clustered.node.list = unique(x=c(clustered.node.list,
                                     V(graph)[v]$original.name,
                                     V(graph)[w]$original.name));
  }
  return(list(graph,centroid.change, clusters, clustered.node.list));
});

################################################################################
### Creates cluster list according to Ordered Concurrent Ricochet specs. 
################################################################################
ricochet.OCR <- cmpfun(f=function(g, method='mean') {
  all.nodes = 1:vcount(g);
  V(g)[1:vcount(g)]$centroid = FALSE;
  V(g)$original.name = as.integer(V(g));
  cutClusters =  as.list((1:vcount(g))[neighborhood.size(graph=g, 
                                                         nodes=1:vcount(g), 
                                                         order=1) == 1]);
  # remove self loops and singletons
  g = delete.vertices(graph=g, 
                      v=((1:vcount(g))[neighborhood.size(graph=g, 
                                                         nodes=1:vcount(g), 
                                                         order=1) == 1]));
  g = igraph::simplify(g);
  E(g)$original.edge.index = as.integer(E(g));
  
  # sorting edges globally
  E(g)$edge.weight.ordering = order(E(g)$weight);
  
  #setting vertex weight ordering.
  vwm = vertex.weight.mapping(g, method, order=1);
  V(g)$centrality.ordering = order(vwm$weight, decreasing=TRUE, na.last=TRUE);
  
  # sorting neighbourhood edges (locally)
  g2 = as.directed(graph=g, mode = "mutual");
  #using ordering of global ordering to decide neighbourhood ordering.
  E(g2)$the.ordering = 0;
  for(x in 1:vcount(g2)) {
    E(g2)[from(x)]$the.ordering = order(E(g2)[from(x)]$edge.weight.ordering,
                                        decreasing=FALSE);
  }
  #setting ordering for edges when rippling
  #NOTE: can calculate before hand
  E(g2)$ripple.ordering = 0;
  for(x in 1:vcount(g2)) {
    eids = as.integer(E(g2)[the.ordering==x]);
    E(g2)[eids]$ripple.ordering = order(E(g2)[eids]$weight);
  }
  
  centroid.change = TRUE;
  index = 0 + 1;#indexing starts from 1
  cutClusters2= list();
  length(cutClusters2) = vcount(g2);
  names(cutClusters2) = V(g2)$original.name;
  clustered.node.list = c();
  #NOTE: we have to use directed graphs at this point.
  while(centroid.change && index < vcount(g2) - 1 + 1 #indexing starts at 1
  ){
    centroid.change = FALSE;
    #updates
    sub.routine.return <- propogate.ripple.OCR(graph=g2,
                                           index=index,
                                           clusters=cutClusters2, 
                                           clustered.node.list=clustered.node.list);
    g2 = sub.routine.return[[1]];
    centroid.change = sub.routine.return[[2]];
    cutClusters2 = sub.routine.return[[3]];
    # necessary to track.
    clustered.node.list = sub.routine.return[[4]];
    index = index + 1;
  }
  #remove empty clusterings
  cutClusters2 = cutClusters2[!as.logical(Map(f=is.null,cutClusters2))]
  #forming clusters and inserting
  cluster.index= names(cutClusters2);
  for (i in cluster.index){
    cutClusters2[[as.character(i)]] = c(cutClusters2[[as.character(i)]],
                                        as.integer(i));
  }
  cutClusters3 = c(cutClusters, unname(cutClusters2));
  #NOTE: original assumption is that all nodes are in their own cluster, thus
  #     remaining nodes must be singletons if no change occurred regarding them.
  ## These are nodes that are connected to others but remained singleton.
  clustered.nodes = unique(Reduce(f=append, x=cutClusters3));
  nonclustered.nodes = all.nodes[! all.nodes %in% clustered.nodes];
  if (length(nonclustered.nodes)==0){
    return(cutClusters3);
  } else {
    return(c(cutClusters3, as.list(nonclustered.nodes)));
  }
});
################################################################################
### Subroutine for CR.
################################################################################
propogate.ripple.CR <- cmpfun(f=function(graph, index, clusters, 
                                         clustered.node.list,
                                         edges.seen){
  centroid.change=FALSE;
  eids = as.integer(E(graph)[the.ordering==index]);
  if (length(eids) == 0) {
    return(list(graph,FALSE, clusters, clustered.node.list,
                edges.seen));
  }
  edges.seen = edges.seen + length(eids);# updated
  ripple.ordering = as.integer(E(graph)[eids]$ripple.ordering);
  es.matrix = get.edges(graph=graph, es=E(graph)[eids]);
  all.v = es.matrix[ripple.ordering, 1];
  all.w = es.matrix[ripple.ordering, 2];
  for(a.ordering in 1:(dim(es.matrix)[1])){
    v = all.v[a.ordering];
    w = all.w[a.ordering];
    v.previously.clustered=FALSE
    #check if already clustered
    v.previously.clustered = V(graph)[v]$original.name %in% clustered.node.list;
    if (!v.previously.clustered){
      if(V(graph)[w]$centroid) {
        # check which one has greater weight (we check ordering instead)
        if(V(graph)[w]$centrality.ordering < V(graph)[v]$centrality.ordering){
          #so, w has higher order than v, add v to w' cluster
          clusters[[as.character(V(graph)[w]$original.name)]] = append(
            x = clusters[[as.character(V(graph)[w]$original.name)]],
            value = V(graph)[v]$original.name);
          #add v's cluster to w's cluster
          clusters[[as.character(V(graph)[w]$original.name)]] = append(
            x = clusters[[as.character(V(graph)[w]$original.name)]],
            value = clusters[[as.character(V(graph)[v]$original.name)]]);
          # emptying v's cluster
          clusters[[as.character(V(graph)[v]$original.name)]] = c();
          #remove centroid status
          if(V(graph)[v]$centroid){
            V(graph)[v]$centroid = FALSE;
            centroid.change=TRUE;
          }
        } else { # same thing as before but with v and w switched
          clusters[[as.character(V(graph)[v]$original.name)]] = append(
            x = clusters[[as.character(V(graph)[v]$original.name)]],
            value = V(graph)[w]$original.name);
          clusters[[as.character(V(graph)[v]$original.name)]] = append(
            x = clusters[[as.character(V(graph)[v]$original.name)]],
            value = clusters[[as.character(V(graph)[w]$original.name)]]);
          clusters[[as.character(V(graph)[w]$original.name)]] = c();
          # can assume loss of status
          V(graph)[w]$centroid = FALSE;
          centroid.change=TRUE;
        }
      } else {
        clusters[[as.character(V(graph)[v]$original.name)]] = append(
          x = clusters[[as.character(V(graph)[v]$original.name)]],
          value = V(graph)[w]$original.name);
        if(!V(graph)[v]$centroid){
          V(graph)[v]$centroid = TRUE;
          centroid.change=TRUE;
        }
      }
    }
    clustered.node.list = unique(x=c(clustered.node.list,
                                     V(graph)[v]$original.name,
                                     V(graph)[w]$original.name));
  }
  return(list(graph,centroid.change, clusters, clustered.node.list, edges.seen));
});
################################################################################
### Creates cluster list according to Concurrent Ricochet specs. 
### NOTE: based on OCR implementation
################################################################################
ricochet.CR <- cmpfun(f=function(g, method="mean") {
  all.nodes = 1:vcount(g);
  V(g)[1:vcount(g)]$centroid = FALSE;
  V(g)$original.name = as.integer(V(g));
  cutClusters =  as.list((1:vcount(g))[neighborhood.size(graph=g, 
                                                         nodes=1:vcount(g), 
                                                         order=1) == 1]);
  # remove self loops and singletons
  g = delete.vertices(graph=g, 
                      v=((1:vcount(g))[neighborhood.size(graph=g, 
                                                         nodes=1:vcount(g), 
                                                         order=1) == 1]));
  g = igraph::simplify(g);
  E(g)$original.edge.index = as.integer(E(g));
  
  # sorting edges globally
  E(g)$edge.weight.ordering = order(E(g)$weight);
  
  #setting vertex weight ordering.
  vwm = vertex.weight.mapping(g, method, order=1);
  V(g)$centrality.ordering = order(vwm$weight, decreasing=TRUE, na.last=TRUE);
  
  # sorting neighbourhood edges (locally)
  g2 = as.directed(graph=g, mode = "mutual");
  #using ordering of global ordering to decide neighbourhood ordering.
  #setting ordering for edges when rippling and modifying nei. order as well
  #NOTE: can calculate before hand
  E(g2)$the.ordering = 0;
  E(g2)$ripple.ordering = 0;
  last.eids=integer(0);
  for(x in 1:vcount(g2)) {
    E(g2)[from(x)]$the.ordering = order(E(g2)[from(x)]$edge.weight.ordering,
                                        decreasing=FALSE);
  }
  new.ordering=1;
  for(x in 1:vcount(g2)) {
    eids = as.integer(E(g2)[the.ordering==x]); # grabing neibourhood edges
    # grabing all edges greater than lowest
    low = min(E(g2)[eids]$weight);
    eids = as.integer(E(g2)[weight >= low]);
    eids = eids[! eids %in% last.eids];
    # following is here because definition of 'next closest edge' is vague
    if (length(eids) !=0){ #conservatively avoid premature termination
      E(g2)[eids]$ripple.ordering = order(E(g2)[eids]$weight);
      E(g2)[eids]$the.ordering = new.ordering; #modified nei. ordering accordingly
      last.eids = c(last.eids, eids);
      new.ordering=new.ordering + 1;
    }
  }
  
  centroid.change = TRUE;
  index = 0 + 1;#indexing starts from 1
  edges.seen = 0;
  cutClusters2= list();
  length(cutClusters2) = vcount(g2);
  names(cutClusters2) = V(g2)$original.name;
  clustered.node.list = c();
  #NOTE: we have to use directed graphs at this point.
  while(centroid.change && index < vcount(g2) - 1 + 1 #indexing starts at 1
        && length(edges.seen)!=ecount(g2) # effectively E being empty
        ){
    centroid.change = FALSE;
    #updates
    sub.routine.return <- propogate.ripple.CR(graph=g2,
                                           index=index,
                                           clusters=cutClusters2, 
                                           clustered.node.list=clustered.node.list,
                                           edges.seen = edges.seen);
    g2 = sub.routine.return[[1]];
    centroid.change = sub.routine.return[[2]];
    cutClusters2 = sub.routine.return[[3]];
    # necessary to track.
    clustered.node.list = sub.routine.return[[4]];
    edges.seen = sub.routine.return[[5]];
    index = index + 1;
  }
  #remove empty clusterings
  cutClusters2 = cutClusters2[!as.logical(Map(f=is.null,cutClusters2))]
  #forming clusters and inserting
  cluster.index= names(cutClusters2);
  for (i in cluster.index){
    cutClusters2[[as.character(i)]] = c(cutClusters2[[as.character(i)]],
                                        as.integer(i));
  }
  cutClusters3 = c(cutClusters, unname(cutClusters2));
  #NOTE: original assumption is that all nodes are in their own cluster, thus
  #     remaining nodes must be singletons if no change occurred regarding them.
  ## These are nodes that are connected to others but remained singleton.
  clustered.nodes = unique(Reduce(f=append, x=cutClusters3));
  nonclustered.nodes = all.nodes[! all.nodes %in% clustered.nodes];
  if (length(nonclustered.nodes)==0){
    return(cutClusters3);
  } else {
    return(c(cutClusters3, as.list(nonclustered.nodes)));
  }
});

################################################################################
### Returns the clusters for Ricochet Family Algorithms
################################################################################
ricochet.cluster <- function(g, method='mean', algo="sr") {
  if(algo == 'sr') {
    return(ricochet.SR(g, method));
  } else if(algo == 'bsr') {
    return(ricochet.BSR(g, method));  
  } else if(algo == 'cr') {
    return(ricochet.CR(g, method));
  } else if(algo == 'ocr') {
    return(ricochet.OCR(g, method));
  } else {
    return(ricochet.SR(g, method));    
  }
}
