################################################################################
### Returns map used to grab ground truth closest to clusters in question.
################################################################################
create.mapping <- function(clusters, ground.truth.clusters) {
  a.mapping = list();
  for (cluster.index in 1:length(clusters)) {
    a.cluster.mapping = c();
    for (ground.truth.cluster.index in 1:length(ground.truth.clusters)) {
      # grabbing percentage coverage wrt ground truth
      a.cluster.mapping[as.character(ground.truth.cluster.index)] =
        sum(clusters[[cluster.index]] %in%
          ground.truth.clusters[[ground.truth.cluster.index]]) # /
        # length(ground.truth.clusters[[ground.truth.cluster.index]])
        ;
    }
    a.mapping[[as.character(cluster.index)]] = a.cluster.mapping;
  }
  a.mapping = lapply(FUN=function(x) return(as.numeric(names(x[x ==
    max(x)][1]))), X=a.mapping);
  return(a.mapping);
}
################################################################################
### Returns Precision between two clusters.
################################################################################
precision.helper.i <- function(ground.truth.cluster, a.cluster) {
  return(sum(ground.truth.cluster %in% a.cluster) /
    length(a.cluster));
}
################################################################################
### Returns Precision between two cluster sets, given a mapping from cluster to
### ground truth clusters.
################################################################################
precision.helper<- function(a.clusters, ground.truth.clusters, a.mapping) {
  accumulator = 0;
  relation.size = 0;
  for (a.cluster.index in 1:length(a.clusters)) {
    ground.truth.cluster= ground.truth.clusters[[a.mapping[[a.cluster.index]]]];
    relation.size = relation.size + length(ground.truth.cluster);
    accumulator = accumulator + precision.helper.i(ground.truth.cluster,
      a.clusters[[a.cluster.index]]) * length(ground.truth.cluster);
  }
  return(accumulator / relation.size);
}
################################################################################
### Returns Precision.
################################################################################
precision <- function(a.clusters, ground.truth.clusters) {
  return(precision.helper(a.clusters, ground.truth.clusters,
    create.mapping(a.clusters, ground.truth.clusters)));
}
################################################################################
### Returns Recall between two clusters.
################################################################################
recall.helper.i <- function(ground.truth.cluster, a.cluster) {
  return(sum(a.cluster %in% ground.truth.cluster) /
    length(ground.truth.cluster));
}
################################################################################
### Returns Recall between two cluster sets, given a mapping from cluster to
### ground truth clusters.
################################################################################
recall.helper<- function(a.clusters, ground.truth.clusters, a.mapping) {
  accumulator = 0;
  relation.size = 0;
  for (a.cluster.index in c(1:length(a.clusters))) {
    ground.truth.cluster= ground.truth.clusters[[a.mapping[[a.cluster.index]]]];
    relation.size = relation.size + length(ground.truth.cluster);
    accumulator = accumulator + recall.helper.i(ground.truth.cluster,
      a.clusters[[a.cluster.index]]) * length(ground.truth.cluster);
  }
  return(accumulator / relation.size);
}
################################################################################
### Returns Recall.
################################################################################
recall <- function(a.clusters, ground.truth.clusters) {
  return(recall.helper(a.clusters, ground.truth.clusters,
    create.mapping(a.clusters, ground.truth.clusters)));
}
################################################################################
### Returns F1-measure.
################################################################################
f1.measure <- function(a.clusters, ground.truth.clusters) {
  a.recall = recall(a.clusters, ground.truth.clusters);
  a.precision = precision(a.clusters, ground.truth.clusters);
  return(2 * a.precision * a.recall / (a.precision + a.recall));
}
################################################################################
### Returns Clustering Precision numerator for a given ground truth cluster.
################################################################################
clustering.precision.helper.i <- function(a.cluster, ground.truth.cluster)
{
  numerator = 0;
  denominator = choose(length(a.cluster), 2);
  num.cohesive.elements = sum( a.cluster %in% ground.truth.cluster);
  if(num.cohesive.elements > 1) {
    numerator = choose(num.cohesive.elements, 2);
  } else {
    # effectively ignoring clusters due to no possible shared pairings
  }
  return(numerator/denominator);
}
################################################################################
### Returns Clustering Precision for a cluster given ground-truth-clusters.
################################################################################
clustering.precision.helper <- function(a.clusters, ground.truth.clusters, a.mapping) {
  accumulator = 0;
  relation.size = 0;
  for (a.cluster.index in 1:length(a.clusters)) {
    ground.truth.cluster= ground.truth.clusters[[a.mapping[[a.cluster.index]]]];
    relation.size = relation.size + 1;
    if(length(ground.truth.cluster) > 1 && 
         length(a.clusters[[a.cluster.index]]) > 1 )
    {
      accumulator = accumulator + clustering.precision.helper.i(
      ground.truth.cluster, a.clusters[[a.cluster.index]]);
    }
  }
  return(accumulator / relation.size);
}
################################################################################
### Returns Clustering Precision
################################################################################
clustering.precision<- function(  a.clusters, ground.truth.clusters)
{
  # returns {\sum ^|C| _j {\sum  ^|G| _i {\choose{|g_i /\ c_j|}{2}}
  #   choose{|c_j|}{2}} / |C|}
  return(clustering.precision.helper(a.clusters, ground.truth.clusters,
                          create.mapping(a.clusters, ground.truth.clusters)));
}
################################################################################
### Returns Penalized Clustering Precision
################################################################################
penalized.clustering.precision<- function(a.clusters, ground.truth.clusters) {
  k = length(ground.truth.clusters);
  k.prime = length(a.clusters);
  if (k < k.prime) {
    return(k / k.prime * clustering.precision(a.clusters,
      ground.truth.clusters));
  } else {
    return(k.prime / k * clustering.precision(a.clusters,
      ground.truth.clusters));
  }
}
################################################################################
### This is a helper function for computing length. It allows for partial
### membership.
################################################################################
k.partial.length <- function(a.clusters)
{
  # constructing weight list
  vertex.count = 0:max(as.numeric(lapply(FUN=max, X=a.clusters)))
  names(vertex.count) = as.character(vertex.count)
  # calculate membership
  vertex.count = 1/as.numeric(lapply(X=as.list(1:length(vertex.count)-1)
                                     , FUN=function(i)
                                     {return(sum(as.numeric(lapply(X=a.clusters , FUN=function(x) i%in%x))))}))
  # OBSCURE things to remember: produced clusters are number vertices from 0 but
  # NORMAL in-R indexing from 1
  ##No longer true for latest igraph
  names(vertex.count) = as.character(1:length(vertex.count)-1)
  return(vertex.count)
}
################################################################################
### This is a helper function for computing ACP and AAP, since they are the
### same computation but with switched parameters.
################################################################################
k.helper <- function(set1, set2, partial.length)
{
  return(sum(as.numeric(lapply(FUN=function(x) {
    #print(partial.length(x))
    return(sum(as.numeric(lapply(FUN=function(y) {
      #print(partial.length(y[y%in%x])^2)
      return(partial.length(y[y%in%x])^2)
    }, X=set2))) / partial.length(x))
  }, X=set1))));
}
################################################################################
### Returns the Average-Cluster-Purity (ACP). Used to calculate the K measure.
################################################################################
average.cluster.purity <- function( a.clusters, ground.truth.clusters,
                                    total.num.records, partial.length)
{
  return(k.helper(a.clusters, ground.truth.clusters, partial.length)
         / total.num.records);
}
################################################################################
### Returns the Average-Author-Purity (AAP). Used to calculate the K measure.
################################################################################
average.author.purity <- function( a.clusters, ground.truth.clusters,
                                   total.num.records, partial.length)
{
  return(k.helper(ground.truth.clusters, a.clusters, partial.length)
         / total.num.records);
}
################################################################################
### Returns the K measure metric.
################################################################################
k.measure <- function(a.clusters, ground.truth.clusters,
                      total.num.records=sum(as.numeric(
                        lapply(FUN=length,X=ground.truth.clusters)))
                      , vertex.weights = k.partial.length(a.clusters)
                      , partial.length = function(x){return(sum(
                        vertex.weights[as.character(x)]))})
{
  return(sqrt(average.cluster.purity( a.clusters, ground.truth.clusters,
                                      total.num.records, partial.length)
              * average.author.purity(a.clusters, ground.truth.clusters,
                                      total.num.records, partial.length)));
}
################################################################################
### Returns entropy measure (joint probability version)
################################################################################
joint.entropy.metric <- function(a.clusters, ground.truth.clusters)
{
  total.num.records=sum(as.numeric(lapply(FUN=function(x)
  {return(sum(as.numeric(lapply(FUN=function(y)
  {return(length(y[y%in%x]))}
                                , X=ground.truth.clusters))));}
                                          , X=a.clusters)));
  return(-sum(as.numeric(lapply(FUN=function(x)
  {
    return(sum(as.numeric(lapply(FUN=function(y)
    {
      joint.p = length(y[y%in%x]) / total.num.records;
      #a.p = length(y) / total.num.records;
      #ground.p = length(x) / total.num.records;
      if (joint.p == 0 #|| a.p == 0 || ground.p == 0
      ){
        return(0);
      } else {
        return(joint.p*log(joint.p));
      }
    }, X=ground.truth.clusters))));
  }, X=a.clusters))));
}
################################################################################
### Returns entropy measure (works for overlapping clusters)
################################################################################
entropy.metric <- function( a.clusters, total.num.records=sum(as.numeric
                                                              (lapply(FUN=length, 
                                                                      X=a.clusters))))
  
{
  prob = as.numeric(lapply(FUN=length, X=a.clusters)) / total.num.records;
  return(-sum(prob*log(prob)));
}
################################################################################
### Returns VI measure (non-slice algo. version)
################################################################################
variation.of.information <- function(a.clusters, ground.truth.clusters)
{
  return(2 * joint.entropy.metric( a.clusters, ground.truth.clusters)
         - entropy.metric(a.clusters) - entropy.metric(ground.truth.clusters));
}
################################################################################
### Returns Normalized VI measure (non-slice algo. version)
################################################################################
normalized.variation.of.information <- function(a.clusters
                                                , ground.truth.clusters)
{
  return(variation.of.information(a.clusters, ground.truth.clusters)
         / joint.entropy.metric( a.clusters, ground.truth.clusters));
}
normalized.VI.fast <- function(a.clusters
                               , ground.truth.clusters)
{
  return(gmd.VI(a.clusters, ground.truth.clusters)
         / joint.entropy.metric( a.clusters, ground.truth.clusters));
}
################################################################################
### Returns Generalized Merge Distance (in linear time via Slice Algorithm).
### The defualt split and merge functions result in returning Basic Merge
### Distance.
################################################################################

general.merge.distance <- function( a.clusters, ground.truth.clusters,
                            split.func=function(x,y) return(1),
                            merge.func=function(x,y) return(1))
{
  map.m = NULL; map.m2 = NULL;big.map = NULL;
  e=new.env(); #for treating variables as if on heap
  e$r.sizes = as.numeric(lapply(FUN=length, X=a.clusters));
  # Grabbing edges to push flow on legal paths
  for (i in 1:length(a.clusters)) {
      map.m = rbind(map.m, cbind(as.matrix(a.clusters[[i]]), i));
  }
  # Grabbing edges to pull flow on legal paths
  for (j in 1:length(ground.truth.clusters)) {
      map.m2 = rbind(map.m2, cbind(as.matrix(ground.truth.clusters[[j]]), j));
  }
  map.m = as.data.frame(map.m);
  map.m2 = as.data.frame(map.m2);
  big.map = merge(x=map.m, y=map.m2, all=FALSE)
  gtc.bool.idxs = lapply(FUN=function(x) big.map[['j']]==x ,
                           X=1:length(ground.truth.clusters));
  # forcing edges to flow between cliques on legal paths
  cost = 0;
  map.merge.func <-function(gtc.bool.idx){
    # collect common records and aggregate counts
    #NOTE: [['i']] is not the same as [[i]]. Not using variable i here.
    #      'i' due to prior loops cbinding of i
    p.map = as.data.frame(table( big.map[gtc.bool.idx, ][['i']]));
    ###pulling 'flow' to this cluster###  
    p.map.count = as.numeric(as.character(p.map[['Freq']]));
    # cost to merge into ground truth cluster
    if (length(p.map.count) > 1) {
      p.map.total.recs = Reduce(f='+', x=p.map.count, init=0,
                                            accumulate=TRUE);
      return(sum(as.numeric(Map(f=merge.func, p.map.count[-1],
                    p.map.total.recs[-c(length(p.map.total.recs), 1)]))));
    } else {return(0);}
  }
  cost = sum(as.numeric(lapply(FUN=map.merge.func, X=gtc.bool.idxs)));
  map.split.func <- function(gtc.bool.idx) {
    # collect common records and aggregate counts
    p.map = as.data.frame(table( big.map[gtc.bool.idx, ][['i']]));
    ###pulling 'flow' to this cluster###  
    p.map.clustering = as.numeric(as.character(p.map[['Var1']]));
    p.map.count = as.numeric(as.character(p.map[['Freq']]));  
    #add cost to split produced cluster
    p.map.r.sizes.count.diff = e$r.sizes[p.map.clustering] - p.map.count;
    p.map.diff.check = p.map.r.sizes.count.diff > 0;
    #reflecting changes
    e$r.sizes[p.map.clustering] = p.map.r.sizes.count.diff;
    return(sum(as.numeric(Map(f=split.func,
      p.map.count[p.map.diff.check],
      p.map.r.sizes.count.diff[p.map.diff.check]))));
  }
  cost = cost + sum(as.numeric(lapply(FUN=map.split.func, X=gtc.bool.idxs)));
  return(cost);
}
normalized.general.merge.distance1 <- function( graph, a.clusters,
                                                ground.truth.clusters,
                                                split.func=function(x,y)
                                                  return(1),
                                                merge.func=function(x,y)
                                                  return(1))
{
  return(general.merge.distance(a.clusters, ground.truth.clusters, split.func,
                                merge.func)
         / general.merge.distance(a.clusters, list(1:vcount(g)) , split.func,
                                  merge.func));
}
normalized.general.merge.distance2 <- function( graph, a.clusters,
                                                ground.truth.clusters,
                                                split.func=function(x,y)
                                                  return(1),
                                                merge.func=function(x,y)
                                                  return(1))
{
  return(general.merge.distance(a.clusters, ground.truth.clusters, split.func,
                                merge.func)
         / general.merge.distance(a.clusters, as.list(1:vcount(g)) , split.func,
                                  merge.func));
}
normalized.general.merge.similarity1 <- function( graph, a.clusters,
                                                  ground.truth.clusters,
                                                  split.func=function(x,y)
                                                    return(1),
                                                  merge.func=function(x,y)
                                                    return(1))
{
  return(1 - normalized.general.merge.distance1(graph, a.clusters,
                                                ground.truth.clusters,
                                                split.func,
                                                merge.func));
}
normalized.general.merge.similarity2 <- function( graph, a.clusters,
                                                  ground.truth.clusters,
                                                  split.func=function(x,y)
                                                    return(1),
                                                  merge.func=function(x,y)
                                                    return(1))
{
  return(1 - normalized.general.merge.distance2(graph, a.clusters,
                                                ground.truth.clusters,
                                                split.func,
                                                merge.func));
}
################################################################################
### Returns the basic merge distance.
################################################################################
basic.merge.distance <- function(a.clusters, ground.truth.clusters)
{
  return(general.merge.distance(a.clusters, ground.truth.clusters));
}
###
bmd.fast <- function( a.clusters, ground.truth.clusters)
{
  map.m = NULL; map.m2 = NULL;big.map = NULL;
  e=new.env(); #for treating variables as if on heap
  e$r.sizes = as.integer(lapply(FUN=length, X=a.clusters));
  # Grabbing edges to push flow on legal paths
  for (i in 1:length(a.clusters)) {
      map.m = rbind(map.m, cbind(as.matrix(a.clusters[[i]]), i));
  }
  # Grabbing edges to pull flow on legal paths
  for (j in 1:length(ground.truth.clusters)) {
      map.m2 = rbind(map.m2, cbind(as.matrix(ground.truth.clusters[[j]]), j));
  }
  map.m = as.data.frame(map.m);
  map.m2 = as.data.frame(map.m2);
  big.map = merge(x=map.m, y=map.m2, all=FALSE)
  p.maps = lapply(
    FUN=function(x)
    {
      y <-as.data.frame(table(big.map[big.map[['j']]==x, ][['i']]));
      y$Var1 <- as.integer(as.character(y$Var1));
      return(y);
    },
    X=1:length(ground.truth.clusters));
  cost=as.integer(0);
  #Use of .C convention with C code
  #Defining two functions, one of which calls the other
  sigGMDLp <- signature(p_maps="raw", rsizes="integer", costs="integer")
  codeGMDLp <- "
    SEXP p_map = R_NilValue;
    int *i, *count;
    int si_cost = 0, total_rec = 0;
    for (R_len_t j = 0; j < length(p_maps); j++){
      //ground clustering
      p_map = VECTOR_ELT(p_maps, j);
      i=INTEGER(VECTOR_ELT(p_map, 0));
      count=INTEGER(VECTOR_ELT(p_map, 1));
      si_cost = 0; total_rec=0;
      for (R_len_t l = 0; l < length(VECTOR_ELT(p_map, 0)); l++){
        //iterating splits
        if (INTEGER(rsizes)[i[l] - 1] > count[l]) {
          si_cost += 1;
        }
        INTEGER(rsizes)[i[l] - 1] -= count[l];
        //iterating merges
        if (total_rec > 0) {
          si_cost += 1;
        }
        total_rec += count[l];
      }
      INTEGER(costs)[0] += si_cost;
    }
    return(costs);
  "
  fns <- cfunction( list(workingfn=sigGMDLp),
    list(codeGMDLp),
    convention=".Call");
  return(fns$workingfn(p.maps, e$r.sizes, cost));
}
################################################################################
### Returns the metric used to compute precision linearly via slice algo.
################################################################################
gmd.precision <- function( a.clusters, ground.truth.clusters,
        split.func=function(x,y) return(x*y),
        merge.func=function(x,y) return(0),
        num.vertices = sum(as.numeric(lapply(X=ground.truth.clusters,
                                             FUN=length)))
)
{
  x = general.merge.distance(a.clusters, ground.truth.clusters,
                             split.func, merge.func);
  y = general.merge.distance(a.clusters, as.list(1:num.vertices),
                             split.func, merge.func);
  return(1-x/y);
}
###
gmd.precision.fast <- function( a.clusters, ground.truth.clusters,
        num.vertices = sum(as.numeric(lapply(X=ground.truth.clusters,
                                             FUN=length)))
)
{
  x = gmd.P(a.clusters, ground.truth.clusters);
  y = gmd.P(a.clusters, as.list(1:num.vertices));
  return(1-x/y);
}

###
gmd.P <- function( a.clusters, ground.truth.clusters)
{
  map.m = NULL; map.m2 = NULL;big.map = NULL;
  e=new.env(); #for treating variables as if on heap
  e$r.sizes = as.integer(lapply(FUN=length, X=a.clusters));
  # Grabbing edges to push flow on legal paths
  for (i in 1:length(a.clusters)) {
      map.m = rbind(map.m, cbind(as.matrix(a.clusters[[i]]), i));
  }
  # Grabbing edges to pull flow on legal paths
  for (j in 1:length(ground.truth.clusters)) {
      map.m2 = rbind(map.m2, cbind(as.matrix(ground.truth.clusters[[j]]), j));
  }
  map.m = as.data.frame(map.m);
  map.m2 = as.data.frame(map.m2);
  big.map = merge(x=map.m, y=map.m2, all=FALSE)
  p.maps = lapply(
    FUN=function(x)
    {
      y <-as.data.frame(table(big.map[big.map[['j']]==x, ][['i']]));
      y$Var1 <- as.integer(as.character(y$Var1));
      return(y);
    },
    #FUN=function(x) as.data.frame(table(big.map[big.map[['j']]==x, ][['i']])),
    X=1:length(ground.truth.clusters));
  cost=as.integer(0);
  #Use of .C convention with C code
  #Defining two functions, one of which calls the other


  sigGMDLp <- signature(p_maps="raw", rsizes="integer", costs="integer")


  codeGMDLp <- "
    SEXP p_map = R_NilValue;
    int *i, *count;
    int si_cost = 0, total_rec = 0;
    for (R_len_t j = 0; j < length(p_maps); j++){
      //ground clustering
      p_map = VECTOR_ELT(p_maps, j);
      i=INTEGER(VECTOR_ELT(p_map, 0));
      count=INTEGER(VECTOR_ELT(p_map, 1));
      si_cost = 0; total_rec=0;
      for (R_len_t l = 0; l < length(VECTOR_ELT(p_map, 0)); l++){
        //iterating splits
        if (INTEGER(rsizes)[i[l] - 1] > count[l]) {
          si_cost += count[l] * (INTEGER(rsizes)[i[l] - 1] - count[l]);
        }
        INTEGER(rsizes)[i[l] - 1] -= count[l]; 
      }
      INTEGER(costs)[0] += si_cost;
    }
    return(costs);
  "
  fns <- cfunction( list(workingfn=sigGMDLp),
    list(codeGMDLp),
    convention=".Call");
  return(fns$workingfn(p.maps, e$r.sizes, cost));
}

################################################################################
### Returns the metric used to compute recall linearly via slice algo.
################################################################################
gmd.recall <- function( a.clusters, ground.truth.clusters,
        split.func=function(x,y) return(0),
        merge.func=function(x,y) return(x*y),
        num.vertices = sum(as.numeric(lapply(X=ground.truth.clusters,
                                             FUN=length)))
)
{
  x = general.merge.distance(a.clusters, ground.truth.clusters,
                             split.func, merge.func);
  y = general.merge.distance(as.list(1:num.vertices),
                             ground.truth.clusters,
                             split.func, merge.func);
  return(1-x/y);
}
###
gmd.recall.fast <- function( a.clusters, ground.truth.clusters,
        split.func=function(x,y) return(0),
        merge.func=function(x,y) return(x*y),
        num.vertices = sum(as.numeric(lapply(X=ground.truth.clusters,
                                             FUN=length)))
)
{
  x = gmd.R(a.clusters, ground.truth.clusters);
  y = gmd.R(as.list(1:num.vertices),
                             ground.truth.clusters);
  return(1-x/y);
}

###
gmd.R <- function( a.clusters, ground.truth.clusters)
{
  map.m = NULL; map.m2 = NULL;big.map = NULL;
  e=new.env(); #for treating variables as if on heap
  e$r.sizes = as.integer(lapply(FUN=length, X=a.clusters));
  # Grabbing edges to push flow on legal paths
  for (i in 1:length(a.clusters)) {
      map.m = rbind(map.m, cbind(as.matrix(a.clusters[[i]]), i));
  }
  # Grabbing edges to pull flow on legal paths
  for (j in 1:length(ground.truth.clusters)) {
      map.m2 = rbind(map.m2, cbind(as.matrix(ground.truth.clusters[[j]]), j));
  }
  map.m = as.data.frame(map.m);
  map.m2 = as.data.frame(map.m2);
  big.map = merge(x=map.m, y=map.m2, all=FALSE)
  p.maps = lapply(
    FUN=function(x)
    {
      y <-as.data.frame(table(big.map[big.map[['j']]==x, ][['i']]));
      y$Var1 <- as.integer(as.character(y$Var1));
      return(y);
    },
    #FUN=function(x) as.data.frame(table(big.map[big.map[['j']]==x, ][['i']])),
    X=1:length(ground.truth.clusters));
  cost=as.integer(0);
  #Use of .C convention with C code
  #Defining two functions, one of which calls the other


  sigGMDLp <- signature(p_maps="raw", rsizes="integer", costs="integer")


  codeGMDLp <- "
    SEXP p_map = R_NilValue;
    int *i, *count;
    int si_cost = 0, total_rec = 0;
    for (R_len_t j = 0; j < length(p_maps); j++){
      //ground clustering
      p_map = VECTOR_ELT(p_maps, j);
      i=INTEGER(VECTOR_ELT(p_map, 0));
      count=INTEGER(VECTOR_ELT(p_map, 1));
      si_cost = 0; total_rec=0;
      for (R_len_t l = 0; l < length(VECTOR_ELT(p_map, 0)); l++){
        //iterating merges
        if (total_rec > 0) {
          si_cost += count[l] * total_rec;
        }
        total_rec += count[l];
      }
      INTEGER(costs)[0] += si_cost;
    }
    return(costs);
  "
  fns <- cfunction( list(workingfn=sigGMDLp),
    list(codeGMDLp),
    convention=".Call");
  return(fns$workingfn(p.maps, e$r.sizes, cost));
}

################################################################################
### Returns the metric used to compute F linearly via slice algo.
### NOTE: Normal F1 is not returning exact values. However VI is.
################################################################################
gmd.F1.measure <- function(a.clusters, ground.truth.clusters,
                           num.vertices = sum(as.numeric(lapply(
                                       X=ground.truth.clusters,
                                       FUN=length)))
)
{
  pR = gmd.precision.fast(a.clusters, ground.truth.clusters,
                     num.vertices=num.vertices);
  rE = gmd.recall.fast(a.clusters, ground.truth.clusters,
                     num.vertices=num.vertices);
  return(2 * pR * rE / {pR + rE});

}
###
gmd.F1.measure.fast <- function(a.clusters, ground.truth.clusters,
                           num.vertices = sum(as.numeric(lapply(
                                       X=ground.truth.clusters,
                                       FUN=length)))
)
{
  xp = gmd.P(a.clusters, ground.truth.clusters);
  yp = gmd.P(a.clusters, as.list(1:num.vertices));
  xr = gmd.R(a.clusters, ground.truth.clusters);
  yr = gmd.R(as.list(1:num.vertices),
                             ground.truth.clusters);
  pR =1-xp/yp;
  print(pR);
  rE = 1-xr/yr;
  return(2 * pR * rE / {pR + rE});

}
###
gmd.F1.measure.faster <- function(a.clusters, ground.truth.clusters,
                           num.vertices = sum(as.numeric(lapply(
                                       X=ground.truth.clusters,
                                       FUN=length)))
)
{
  ### Note: Re-wrote F1 because lots of unnecessary computations.
  ###       It is better to rewite by inlining in the case of slice.
  ### Gains: at least 3 times faster
  a.func=function(x,y) x*y;
  #used for numerator computations and a denominator
  map.m = NULL; map.m2 = NULL;
  e=new.env();
  e$r.sizes = as.numeric(lapply(FUN=length, X=a.clusters));
  # Grabbing edges to flow on legal paths
  for (i in 1:length(a.clusters)) {
      map.m = rbind(map.m, cbind(as.matrix(a.clusters[[i]]), i));
  }
  # Grabbing edges to pull flow on legal paths
  for (j in 1:length(ground.truth.clusters)) {
      map.m2 = rbind(map.m2, cbind(as.matrix(ground.truth.clusters[[j]]), j));
  }
  map.m = as.data.frame(map.m);
  map.m2 = as.data.frame(map.m2);
  singleton.map.m = data.frame(V1=1:num.vertices, i=1:num.vertices);

  big.map = merge(x=map.m, y=map.m2, all=FALSE)
  singleton.big.map = merge(x=singleton.map.m, y=map.m2, all=FALSE)
  singleton.map.m = data.frame(V1=1:num.vertices, j=1:num.vertices); # re-init for proper joining
  big.singleton.map = merge(x=map.m, y=singleton.map.m, all=FALSE)
  gtc.bool.idxs = lapply(FUN=function(x) big.map[['j']]==x ,
                           X=1:length(ground.truth.clusters));
  singleton.gtc.bool.idxs = lapply(FUN=function(x) singleton.big.map[['j']]==x ,
                                     X=1:length(ground.truth.clusters));
  singleton.bool.idxs = lapply(FUN=function(x) big.singleton.map[['j']]==x ,
                                 X=1:num.vertices);


  # forcing edges to flow between cliques on legal paths
  np= 0;dp= 0;nr= 0;dr= 0;
  map.a.func1 <-function(gtc.bool.idx){
    # collect common records and aggregate counts
    #NOTE: [['i']] is not the same as [[i]]. Not using variable i here.
    #      'i' due to prior loops cbinding of i
    p.map = as.data.frame(table( big.map[gtc.bool.idx, ][['i']]));
    ###pulling 'flow' to this cluster###  
    p.map.count = as.numeric(as.character(p.map[['Freq']]));
    # cost to merge into ground truth cluster
    if (length(p.map.count) > 1) {
      p.map.total.recs = Reduce(f='+', x=p.map.count, init=0,
                                            accumulate=TRUE);
      return(sum(as.numeric(Map(f=a.func, p.map.count[-1],
                    p.map.total.recs[-c(length(p.map.total.recs), 1)]))));
    } else {return(0);}
  }
  map.a.func2 <-function(singleton.gtc.bool.idx){
    # collect common records and aggregate counts
    singleton.p.map = as.data.frame(table(singleton.big.map[singleton.gtc.bool.idx, ][['i']]));
    ###pulling 'flow' to this cluster###  
    singleton.p.map.count = as.numeric(as.character(singleton.p.map[['Freq']]));
    # cost to merge intoground truth cluster using singletons
    if (length(singleton.p.map.count) > 1) {
      singleton.p.map.total.recs = 0:length(singleton.p.map.count);
      return(sum(as.numeric(Map(f=a.func, singleton.p.map.count[-1],
        singleton.p.map.total.recs[-c(length(singleton.p.map.total.recs), 1)]))));
    } else {return(0);}
  }
  map.a.func3 <- function(gtc.bool.idx) {
    # collect common records and aggregate counts
    p.map = as.data.frame(table(big.map[gtc.bool.idx, ][['i']]));
    ###pulling 'flow' to this cluster###  
    p.map.clustering = as.numeric(as.character(p.map[['Var1']]));
    p.map.count = as.numeric(as.character(p.map[['Freq']]));    
    #add cost to split produced cluster and reflect changes
    p.map.r.sizes.count.diff = e$r.sizes[p.map.clustering] - p.map.count;
    e$r.sizes[p.map.clustering] = p.map.r.sizes.count.diff;
    p.map.diff.check = p.map.r.sizes.count.diff > 0;
    return(sum(as.numeric(Map(f=a.func,
      p.map.count[p.map.diff.check],
      p.map.r.sizes.count.diff[p.map.diff.check]))));
  }
  nr = sum(as.numeric(lapply(FUN=map.a.func1, X=gtc.bool.idxs)));
  dr = sum(as.numeric(lapply(FUN=map.a.func2, X=singleton.gtc.bool.idxs)));
  np = sum(as.numeric(lapply(FUN=map.a.func3, X=gtc.bool.idxs)));
  #for precision denominator via slitting to singletons
  e$r.sizes = as.numeric(lapply(FUN=length, X=a.clusters)); #needs resetting
  map.a.func4<-function(singleton.bool.idx) {
    # collect common records and aggregate counts
    p.map = as.data.frame(table(big.singleton.map[singleton.bool.idx, ][['i']]));
    ###pulling 'flow' to this cluster###  
    p.map.clustering = as.numeric(as.character(p.map[['Var1']]));
    p.map.count = as.numeric(as.character(p.map[['Freq']]));
    #add cost to split produced cluster and reflecting changes
    p.map.r.sizes.count.diff = e$r.sizes[p.map.clustering] - p.map.count;
    e$r.sizes[p.map.clustering] = p.map.r.sizes.count.diff;
    p.map.diff.check = p.map.r.sizes.count.diff > 0;
    return(sum(as.numeric(Map(f=a.func,
      p.map.count[p.map.diff.check],
      p.map.r.sizes.count.diff[p.map.diff.check]))));
  }
  dp = sum(as.numeric(lapply(FUN=map.a.func4, X=singleton.bool.idxs)));
  pR = 1-np/dp;  rE = 1-nr/dr;
  return(2 * pR * rE / {pR + rE});
}
###
# Setting up F1 .Call helper functions
sig.GMD.F1.numerator <- signature(p_maps="raw", rsizes="integer", costs="integer");
sig.GMD.R.denominator <- signature(p_maps="raw", rsizes="integer", costs="integer");
sig.GMD.P.denominator <- signature(p_maps="raw", rsizes="integer", costs="integer");
code.GMD.F1.numerator <- "
  SEXP p_map = R_NilValue;
  int *i, *count;
  int si_cost1 = 0, si_cost2 = 0, total_rec = 0;
  for (R_len_t j = 0; j < length(p_maps); j++){
    //ground clustering
    p_map = VECTOR_ELT(p_maps, j);
    i=INTEGER(VECTOR_ELT(p_map, 0));
    count=INTEGER(VECTOR_ELT(p_map, 1));
    si_cost1 = 0; si_cost2 = 0;total_rec=0;
    for (R_len_t l = 0; l < length(VECTOR_ELT(p_map, 0)); l++){
      //iterating splits
      if (INTEGER(rsizes)[i[l] - 1] > count[l]) {
        si_cost1 += count[l] * (INTEGER(rsizes)[i[l] - 1] - count[l]);
      }
      INTEGER(rsizes)[i[l] - 1] -= count[l]; 
      //iterating merges
      if (total_rec > 0) {
        si_cost2 += count[l] * total_rec;
      }
      total_rec += count[l];
    }
    INTEGER(costs)[0] += si_cost1;
    INTEGER(costs)[1] += si_cost2;
  }
  return(costs);
"
code.GMD.P.denominator <- "
  SEXP p_map = R_NilValue;
  int *i, *count;
  int si_cost = 0, total_rec = 0;
  for (R_len_t j = 0; j < length(p_maps); j++){
    //ground clustering
    p_map = VECTOR_ELT(p_maps, j);
    i=INTEGER(VECTOR_ELT(p_map, 0));
    count=INTEGER(VECTOR_ELT(p_map, 1));
    si_cost = 0;total_rec=0;
    for (R_len_t l = 0; l < length(VECTOR_ELT(p_map, 0)); l++){
      //iterating splits
      if (INTEGER(rsizes)[i[l] - 1] > count[l]) {
        si_cost += count[l] * (INTEGER(rsizes)[i[l] - 1] - count[l]);
      }
      INTEGER(rsizes)[i[l] - 1] -= count[l];
    }
    INTEGER(costs)[0] += si_cost;
  }
  return(costs);
"
code.GMD.R.denominator <- "
  SEXP p_map = R_NilValue;
  int *i, *count;
  int si_cost = 0, total_rec = 0;
  for (R_len_t j = 0; j < length(p_maps); j++){
    //ground clustering
    p_map = VECTOR_ELT(p_maps, j);
    i=INTEGER(VECTOR_ELT(p_map, 0));
    count=INTEGER(VECTOR_ELT(p_map, 1));
    si_cost = 0;total_rec=0;
    for (R_len_t l = 0; l < length(VECTOR_ELT(p_map, 0)); l++){
      //iterating merges
      if (total_rec > 0) {
        si_cost += count[l] * total_rec;
      }
      total_rec += count[l];
    }
    INTEGER(costs)[0] += si_cost;
  }
  return(costs);
"
F1.helper.func <- cfunction( list(gmd_helper_function_F1_numerator=sig.GMD.F1.numerator,
                       gmd_helper_function_R_denominator=sig.GMD.R.denominator,
                       gmd_helper_function_P_denominator=sig.GMD.P.denominator),
  list(code.GMD.F1.numerator, code.GMD.R.denominator, code.GMD.P.denominator),
  convention=".Call");
###
gmd.F1.measure.fastest <- function(a.clusters, ground.truth.clusters,
                           num.vertices = sum(as.numeric(lapply(
                                       X=ground.truth.clusters,
                                       FUN=length)))
)
{
  #used for numerator computations and a denominator
  map.m = NULL; map.m2 = NULL;
  e=new.env();
  e$r.sizes = as.integer(lapply(FUN=length, X=a.clusters));
  e$singleton.sizes = as.integer(lapply(FUN=function(x) 1, X=1:num.vertices));
  # Grabbing edges to flow on legal paths
  for (i in 1:length(a.clusters)) {
      map.m = rbind(map.m, cbind(as.matrix(a.clusters[[i]]), i));
  }
  # Grabbing edges to pull flow on legal paths
  for (j in 1:length(ground.truth.clusters)) {
      map.m2 = rbind(map.m2, cbind(as.matrix(ground.truth.clusters[[j]]), j));
  }
  map.m = as.data.frame(map.m);
  map.m2 = as.data.frame(map.m2);
  singleton.map.m = data.frame(V1=1:num.vertices, i=1:num.vertices);

  big.map = merge(x=map.m, y=map.m2, all=FALSE)
  singleton.big.map = merge(x=singleton.map.m, y=map.m2, all=FALSE)
  singleton.map.m = data.frame(V1=1:num.vertices, j=1:num.vertices); # re-init for proper joining
  big.singleton.map = merge(x=map.m, y=singleton.map.m, all=FALSE)
  #gtc.bool.idxs = lapply(FUN=function(x) big.map[['j']]==x ,
  #                         X=1:length(ground.truth.clusters));
  p.maps = lapply(
    FUN=function(x)
    {
      y <-as.data.frame(table(big.map[big.map[['j']]==x, ][['i']]));
      y$Var1 <- as.integer(as.character(y$Var1));
      return(y);
    },
    X=1:length(ground.truth.clusters));
  #singleton.gtc.bool.idxs = lapply(FUN=function(x) singleton.big.map[['j']]==x ,
  #                                   X=1:length(ground.truth.clusters));
  singleton.p.maps = lapply(
    FUN=function(x)
    {
      y <-as.data.frame(table(singleton.big.map[singleton.big.map[['j']]==x, ][['i']]));
      y$Var1 <- as.integer(as.character(y$Var1));
      return(y);
    },
    X=1:length(ground.truth.clusters));
  #singleton.bool.idxs = lapply(FUN=function(x) big.singleton.map[['j']]==x ,
  #                               X=1:num.vertices);
  p.singleton.maps = lapply(
    FUN=function(x)
    {
      y <-as.data.frame(table(big.singleton.map[big.singleton.map[['j']]==x, ][['i']]));
      y$Var1 <- as.integer(as.character(y$Var1));
      return(y);
    },
    X=1:num.vertices);

  np.nr=integer(2); dr=integer(1); dp=integer(1);
  np.nr = F1.helper.func$gmd_helper_function_F1_numerator(p.maps, e$r.sizes, np.nr);
  np = np.nr[1]; nr = np.nr[2];
  dr = F1.helper.func$gmd_helper_function_R_denominator(singleton.p.maps,
    e$singleton.sizes, dr);
  e$r.sizes = as.integer(lapply(FUN=length, X=a.clusters)); #needs resetting
  dp = F1.helper.func$gmd_helper_function_P_denominator(p.singleton.maps,
    e$r.sizes, dp);
  pR = 1-np/dp;  rE = 1-nr/dr;  
  return(2 * pR * rE / {pR + rE});
}
################################################################################
### Returns the metric used to compute a hybrid metric linearly via slice algo.
### Highly sensitive to record sizes and retains overhead for splits/merges.
################################################################################
gmd.hybrid <- function( a.clusters, ground.truth.clusters)
{
  return(general.merge.distance(a.clusters, ground.truth.clusters,
        merge.func=function(x,y) return(x*y + 1),
        split.func=function(x,y) return(x*y + 1)));
}
################################################################################
### Returns the metric used to compute VI linearly via slice algo. This
### is NOT a helper function, but the actual Variance of Information(VI) metric.
################################################################################
gmd.VI <- function( a.clusters, ground.truth.clusters,
                          num.vertices=sum(as.numeric(lapply(FUN=length,
                            X=ground.truth.clusters))))
{
  # entorpy function
  h = function(x) return(x*log(x/num.vertices)/num.vertices);
  return(general.merge.distance(a.clusters, ground.truth.clusters,
        merge.func=function(x,y) return(h(x + y) - h(x) - h(y)),
        split.func=function(x,y) return(h(x + y) - h(x) - h(y))));
}
