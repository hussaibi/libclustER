################################################################################
### Computes runtime to summarize the algorithm performance.
################################################################################
a.trial.for.runtime <- function(the.graph, theta, trialNum=5,
                                the.algo=partition.clustering,
                                ...) {
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  for (i in 1:trialNum){
    print(system.time(the.cluster.output <- the.algo(the.graph, ...)));
  }
  return(the.graph);
  
}
################################################################################
### Runs runtime trials on given data source.
################################################################################
data.trials.for.runtime <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv",
  the.algo=partition.clustering, 
  trialNum=5,
  ...)
{
  print(edge.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.4)) {
    a.trial.for.runtime(the.graph, theta, trialNum, the.algo, ...);
  }
}
################################################################################
### Runs runtime trials.
################################################################################
data.sources.run.for.runtime <- function(the.algo=partition.clustering, ...)
{
  cmp.algo = cmpfun(f=the.algo);
  data.trials.for.runtime(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv", cmp.algo, ...);
}
#-------------------------------------------------------------------------------
################################################################################
### Computes scales to summarize the graph.
################################################################################
a.trial.graph.scale <- function(the.graph, theta) {
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print("VertexNum");
  print(vcount(graph=the.graph));
  print("EdgeNum");
  print(ecount(graph=the.graph));
  print("MaxVertexDegree");
  print(max(neighborhood.size(graph=the.graph, order=1)));
}
################################################################################
### Runs Graph Metric trials on given data source.
################################################################################
data.trials.graph.scale <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.center.txt", append=TRUE, type="output", split=TRUE);
  print(edge.path);
  #the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.graph.scale(the.graph, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Graph Metrics trials.
################################################################################
data.sources.run.graph.scale <- function()
{
  #the cu's
  data.trials.graph.scale(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.graph.scale(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.graph.scale(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.graph.scale(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Computes metrics to summarize the graph.
################################################################################
a.trial.graph.metrics <- function(the.graph, theta) {
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print("GlobalTransitivity");
  print(transitivity(graph=the.graph, type=c("globalundirected")));
  print("LocalAverageTransitivity");
  print(transitivity(graph=the.graph, type=c("localaverageundirected")));
  print("GraphEdgeDensity"); #Gauranteed to be in [0,1]
  print(graph.density(graph=simplify(remove.loops=TRUE,remove.multiple=TRUE
                                     ,graph=the.graph), loops=FALSE));
  print("GraphStrengthDensity"); #Gauranteed to be in [0,1]
  print(sum(E(simplify(remove.loops=TRUE, remove.multiple=TRUE
                 , graph=the.graph))$weight)/choose(vcount(the.graph),2));
  print("LocalAverageGraphDegreeDensity"); #Gauranteed to be in [0,1]
  print(mean(vertex.weight.mapping(g=the.graph,method="degree")[[1]])
        /vcount(the.graph));
  print("LocalAverageGraphStrengthDensity"); #Gauranteed to be in [0,1]
  print(mean(vertex.weight.mapping(g=the.graph,method="sum")[[1]])
        /vcount(the.graph));
  print("AvgEVCent");
  print(mean(vertex.weight.mapping(g=the.graph,method="evcent")[[1]]));
  # Wish-list: Markov steady state mean (NumErr: too close to one!)
  print("StdMarkov");
  print(sd(vertex.weight.mapping(g=the.graph,method="markov")[[1]]));
  print("MeanNeighbourhoodMeanStrength"); # ignores singleton components
  print(mean(vertex.weight.mapping(g=the.graph,method="mean")[[1]], na.rm=TRUE));
}
################################################################################
### Runs Graph Metric trials on given data source.
################################################################################
data.trials.graph.metrics <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.center.txt", append=TRUE, type="output", split=TRUE);
  print(edge.path);
  #the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.graph.metrics(the.graph, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Graph Metrics trials.
################################################################################
data.sources.run.graph.metrics <- function()
{
  #the cu's
  data.trials.graph.metrics(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.graph.metrics(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.graph.metrics(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.graph.metrics(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Center experiments
################################################################################
a.trial.center <- function ( the.graph, the.true.cluster, theta
                             , mode = "arbitrary", order = 1)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- center.cluster(the.graph, mode
                                                         , order)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Center trials on given data source.
################################################################################
data.trials.center <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv", mode = "arbitrary", order = 1)
{
  #sink(file="data.center.txt", append=TRUE, type="output", split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.center(the.graph, the.true.cluster, theta, mode, order);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Center trials.
################################################################################
data.sources.run.center <- function(mode = "arbitrary", order = 1)
{
  #the cu's
  data.trials.center(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv", mode, order);
  #the f's
  data.trials.center(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv", mode, order);
  #unlink("tmp.txt");# this deletes the file
  data.trials.center(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv", mode, order);
  data.trials.center(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv", mode, order);
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Merge Center experiments
################################################################################
a.trial.merge.center <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- merge.center.cluster(the.graph)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Merge Center trials on given data source.
################################################################################
data.trials.merge.center <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.merge.center.txt", append=TRUE, type="output", split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.merge.center(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Merge Center trials.
################################################################################
data.sources.run.merge.center <- function()
{
  #the cu's
  data.trials.merge.center(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.merge.center(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.merge.center(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.merge.center(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Partition experiments
################################################################################
a.trial.partition <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- partition.clustering(the.graph)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Partition trials on given data source.
################################################################################
data.trials.partition <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.partition.txt", append=TRUE, type="output",split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.partition(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Partition trials.
################################################################################
data.sources.run.partition <- function()
{
  #the cu's
  data.trials.partition(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.partition(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.partition(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.partition(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.partition(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.partition(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.partition(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.partition(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.partition(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.partition(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.partition(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.partition(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.partition(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.partition(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.partition(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.partition(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.partition(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.partition(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Markov experiments
################################################################################
a.trial.markov <- function ( the.graph, the.true.cluster, theta
                             , k=100, prune=FALSE)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- markov.cluster(the.graph
                                                         , k=k
                                                         , prune=prune)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Markov trials on given data source.
################################################################################
data.trials.markov <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv", k=100, prune=FALSE, sparse=FALSE, knnk=100)
{
  #sink(file="data.markov.txt", append=TRUE, type="output",split=TRUE);
  print(edge.path);
  if (prune){
  print("TopKPruned");
  print(k);}
  if (sparse){
  print("Knn");
  print(knnk);}
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path, sparse, knnk);
  for (theta in c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3)) {
    a.trial.markov(the.graph, the.true.cluster, theta, k, prune);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Markov trials.
################################################################################
data.sources.run.markov <- function(k=100, prune=FALSE, sparse=FALSE, knnk=100)
{
  #the cu's
  data.trials.markov(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv", k, prune, sparse, knnk);
  #the f's
  data.trials.markov(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv", k, prune, sparse, knnk);
  #unlink("tmp.txt");# this deletes the file
  data.trials.markov(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv", k, prune, sparse, knnk);
  data.trials.markov(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv", k, prune, sparse, knnk);
}



#-------------------------------------------------------------------------------
################################################################################
### Trial for Affinity Propagation experiments
################################################################################
a.trial.affinity.propagation <- function ( the.graph, the.true.cluster, theta,
					   maxits=500,
					   convits=10, pref=NA, q=NA, normalize=TRUE, median.default.case=TRUE)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- affinity.propagation.cluster(g=the.graph
                                                         , maxits=maxits
                                                         , convits=convits
                                                         , pref=pref
                                                         , q=q
                                                         , normalize=normalize
                                              , median.default.case=median.default.case))
        );
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Affinity Propagation trials on given data source.
################################################################################
data.trials.affinity.propagation <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv",
  maxits=500, convits=10, pref=NA, q=NA, normalize=TRUE, median.default.case=TRUE)
{
  #sink(file="data.markov.txt", append=TRUE, type="output",split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)) {
    a.trial.affinity.propagation(the.graph, the.true.cluster, theta,
					maxits, convits, pref, q, normalize, median.default.case);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Affinity Propagation trials.
################################################################################
data.sources.run.affinity.propagation <- function(maxits=500, convits=10
                                                  , pref=NA, q=NA
                                                  , normalize=TRUE
                                                  , median.default.case=TRUE)
{
  #the cu's
  data.trials.affinity.propagation(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv", maxits, convits, pref, q, normalize, median.default.case);
  #the f's
  data.trials.affinity.propagation(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv", maxits, convits, pref, q, normalize, median.default.case);
  data.trials.affinity.propagation(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv", maxits, convits, pref, q, normalize, median.default.case);
  #unlink("tmp.txt");# this deletes the file
  ####### UNCOMMENTING THESE FOR AFFINITY PAPER #################
  #   data.trials.affinity.propagation(
  #     edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  #     node.path="./../data/5k.csv", maxits, convits, pref, q, normalize, median.default.case);
  #   data.trials.affinity.propagation(
  #     edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
  #     node.path="./../data/10k.csv", maxits, convits, pref, q, normalize, median.default.case);
  #   data.trials.affinity.propagation(
  #     edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
  #     node.path="./../data/20k.csv", maxits, convits, pref, q, normalize, median.default.case);
  #   data.trials.affinity.propagation(
  #     edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
  #     node.path="./../data/50k.csv", maxits, convits, pref, q, normalize, median.default.case);
  #   data.trials.affinity.propagation(
  #     edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
  #     node.path="./../data/100k.csv", maxits, convits, pref, q, normalize, median.default.case);
}

################################################################################
### Runs Affinity Propagation trials. For Zipf
################################################################################
data.sources.run.affinity.propagation.zipf <- function(maxits=500, convits=10
                                                  , pref=NA, q=NA
                                                  , normalize=TRUE
                                                  , median.default.case=TRUE)
{
     data.trials.affinity.propagation(
       edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
       node.path="./../data/10k.csv", maxits, convits, pref, q, normalize,
       median.default.case);
}


#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (mean centrality, with overlap) experiments
################################################################################
a.trial.star.mean.overlap <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (mean centrality, with overlap) trials on given data source.
################################################################################
data.trials.star.mean.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.mean.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.mean.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (mean centrality, with overlap) trials.
################################################################################
data.sources.run.star.mean.overlap <- function()
{
  #the cu's
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.mean.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (sum centrality, with overlap) experiments
################################################################################
a.trial.star.sum.overlap <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
    method="sum")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (sum centrality, with overlap) trials on given data source.
################################################################################
data.trials.star.sum.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.sum.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.sum.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (sum centrality, with overlap) trials.
################################################################################
data.sources.run.star.sum.overlap <- function()
{
  #the cu's
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.sum.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (degree centrality, with overlap) experiments
################################################################################
a.trial.star.degree.overlap <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
    method="degree")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (degree centrality, with overlap) trials on given data source.
################################################################################
data.trials.star.degree.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.degree.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.degree.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (degree centrality, with overlap) trials.
################################################################################
data.sources.run.star.degree.overlap <- function()
{
  #the cu's
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.degree.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (kleinberg centrality, with overlap) experiments
################################################################################
a.trial.star.kleinberg.overlap <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
    method="kleinberg")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (kleinberg centrality, with overlap) trials on given data source.
################################################################################
data.trials.star.kleinberg.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.kleinberg.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.kleinberg.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (kleinberg centrality, with overlap) trials.
################################################################################
data.sources.run.star.kleinberg.overlap <- function()
{
  #the cu's
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.kleinberg.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (eigen centrality, with overlap) experiments
################################################################################
a.trial.star.evcent.overlap <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
    method="evcent")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (eigen centrality, with overlap) trials on given data source.
################################################################################
data.trials.star.evcent.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.evcent.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.evcent.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (eigen centrality, with overlap) trials.
################################################################################
data.sources.run.star.evcent.overlap <- function()
{
  #the cu's
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.evcent.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (markov centrality, with overlap) experiments
################################################################################
a.trial.star.markov.overlap <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
    method="markov")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (markov centrality, with overlap) trials on given data source.
################################################################################
data.trials.star.markov.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.markov.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.markov.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (markov centrality, with overlap) trials.
################################################################################
data.sources.run.star.markov.overlap <- function()
{
  #the cu's
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.markov.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (mean centrality, without overlap) experiments
################################################################################
a.trial.star.mean.non.overlap <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
overlap=FALSE)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (mean centrality, without overlap) trials on given data source.
################################################################################
data.trials.star.mean.non.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.mean.non.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.mean.non.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (mean centrality, without overlap) trials.
################################################################################
data.sources.run.star.mean.non.overlap <- function()
{
  #the cu's
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.mean.non.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (sum centrality, without overlap) experiments
################################################################################
a.trial.star.sum.non.overlap <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
    method="sum", overlap=FALSE)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (sum centrality, without overlap) trials on given data source.
################################################################################
data.trials.star.sum.non.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.sum.non.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.sum.non.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (sum centrality, without overlap) trials.
################################################################################
data.sources.run.star.sum.non.overlap <- function()
{
  #the cu's
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.sum.non.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (degree centrality, without overlap) experiments
################################################################################
a.trial.star.degree.non.overlap <- function ( the.graph, the.true.cluster,
                                              theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
                                                       method="degree", overlap=FALSE)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
                                       ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
                             ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
               ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
  #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
                                            ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (degree centrality, without overlap) trials on given data source.
################################################################################
data.trials.star.degree.non.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.degree.non.overlap.txt", append=TRUE, type="output",
  #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.degree.non.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (degree centrality, without overlap) trials.
################################################################################
data.sources.run.star.degree.non.overlap <- function()
{
  #the cu's
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.degree.non.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (random centrality, without overlap) experiments.
### i.e. cc-pivot
################################################################################
a.trial.cc.pivot <- function ( the.graph, the.true.cluster,
                                              theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
                                                       method="random",
                                                       overlap=FALSE)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
                                       ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
                             ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(gmd.precision(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  print("Re");
  print(gmd.recall(a.clusters=the.cluster.output,
               ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
  #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
                                            ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs ccpivot trials on given data source.
################################################################################
data.trials.cc.pivot <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.degree.non.overlap.txt", append=TRUE, type="output",
  #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.cc.pivot(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs ccpivot trials.
################################################################################
data.sources.run.cc.pivot <- function()
{
  #the cu's
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.cc.pivot(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.cc.pivot(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (kleinberg centrality, without overlap) experiments
################################################################################
a.trial.star.kleinberg.non.overlap <- function ( the.graph, the.true.cluster,
theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
    method="kleinberg", overlap=FALSE)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (kleinberg centrality, without overlap) trials on given data
### source.
################################################################################
data.trials.star.kleinberg.non.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.kleinberg.non.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.kleinberg.non.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (kleinberg centrality, without overlap) trials.
################################################################################
data.sources.run.star.kleinberg.non.overlap <- function()
{
  #the cu's
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.kleinberg.non.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (eigen centrality, without overlap) experiments
################################################################################
a.trial.star.evcent.non.overlap <- function ( the.graph, the.true.cluster,
theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
    method="evcent", overlap=FALSE)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (eigen centrality, without overlap) trials on given data source.
################################################################################
data.trials.star.evcent.non.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.evcent.non.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.evcent.non.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (eigen centrality, without overlap) trials.
################################################################################
data.sources.run.star.evcent.non.overlap <- function()
{
  #the cu's
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.evcent.non.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (markov centrality, without overlap) experiments
################################################################################
a.trial.star.markov.non.overlap <- function ( the.graph, the.true.cluster,
theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
    method="markov", overlap=FALSE)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Star (markov centrality, without overlap) trials on given data source.
################################################################################
data.trials.star.markov.non.overlap <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.markov.non.overlap.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.star.markov.non.overlap(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Star (markov centrality, without overlap) trials.
################################################################################
data.sources.run.star.markov.non.overlap <- function()
{
  #the cu's
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.star.markov.non.overlap(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Ricochet (mean centrality ) experiments
################################################################################
a.trial.ricochet.SR.mean <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Ricochet (mean centrality ) trials on given data source.
################################################################################
data.trials.ricochet.SR.mean <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.SR.mean.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.SR.mean(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Ricochet (mean centrality ) trials.
################################################################################
data.sources.run.ricochet.SR.mean <- function()
{
  #the cu's
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.SR.mean(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Ricochet (sum centrality ) experiments
################################################################################
a.trial.ricochet.SR.sum <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
    method="sum")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Ricochet (sum centrality ) trials on given data source.
################################################################################
data.trials.ricochet.SR.sum <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.SR.sum.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.SR.sum(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Ricochet (sum centrality ) trials.
################################################################################
data.sources.run.ricochet.SR.sum <- function()
{
  #the cu's
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.SR.sum(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Ricochet (degree centrality ) experiments
################################################################################
a.trial.ricochet.SR.degree <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
    method="degree")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Ricochet (degree centrality ) trials on given data source.
################################################################################
data.trials.ricochet.SR.degree <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.SR.degree.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.SR.degree(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Ricochet (degree centrality ) trials.
################################################################################
data.sources.run.ricochet.SR.degree <- function()
{
  #the cu's
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.SR.degree(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Ricochet (kleinberg centrality ) experiments
################################################################################
a.trial.ricochet.SR.kleinberg <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
    method="kleinberg")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Ricochet (kleinberg centrality ) trials on given data source.
################################################################################
data.trials.ricochet.SR.kleinberg <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.SR.kleinberg.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.SR.kleinberg(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Ricochet (kleinberg centrality ) trials.
################################################################################
data.sources.run.ricochet.SR.kleinberg <- function()
{
  #the cu's
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.SR.kleinberg(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Ricochet (eigen centrality ) experiments
################################################################################
a.trial.ricochet.SR.evcent <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
    method="evcent")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Ricochet (eigen centrality ) trials on given data source.
################################################################################
data.trials.ricochet.SR.evcent <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.SR.evcent.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.SR.evcent(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Ricochet (eigen centrality ) trials.
################################################################################
data.sources.run.ricochet.SR.evcent <- function()
{
  #the cu's
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.SR.evcent(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Ricochet (markov centrality ) experiments
################################################################################
a.trial.ricochet.SR.markov <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
    method="markov")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Ricochet (markov centrality ) trials on given data source.
################################################################################
data.trials.ricochet.SR.markov <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.SR.markov.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.SR.markov(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Ricochet (markov centrality ) trials.
################################################################################
data.sources.run.ricochet.SR.markov <- function()
{
  #the cu's
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.SR.markov(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Balanced Ricochet (mean centrality ) experiments
################################################################################
a.trial.ricochet.BSR.mean <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
type="BSR")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Balanced Ricochet (mean centrality ) trials on given data source.
################################################################################
data.trials.ricochet.BSR.mean <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.BSR.mean.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.BSR.mean(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Balanced Ricochet (mean centrality ) trials.
################################################################################
data.sources.run.ricochet.BSR.mean <- function()
{
  #the cu's
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.BSR.mean(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Balanced Ricochet (sum centrality ) experiments
################################################################################
a.trial.ricochet.BSR.sum <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
    method="sum", type="BSR")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Balanced Ricochet (sum centrality ) trials on given data source.
################################################################################
data.trials.ricochet.BSR.sum <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.BSR.sum.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.BSR.sum(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Balanced Ricochet (sum centrality ) trials.
################################################################################
data.sources.run.ricochet.BSR.sum <- function()
{
  #the cu's
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.BSR.sum(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Balanced Ricochet (degree centrality ) experiments
################################################################################
a.trial.ricochet.BSR.degree <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
    method="degree", type="BSR")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Balanced Ricochet (degree centrality ) trials on given data source.
################################################################################
data.trials.ricochet.BSR.degree <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.BSR.degree.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.BSR.degree(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Balanced Ricochet (degree centrality ) trials.
################################################################################
data.sources.run.ricochet.BSR.degree <- function()
{
  #the cu's
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.BSR.degree(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Balanced Ricochet (kleinberg centrality ) experiments
################################################################################
a.trial.ricochet.BSR.kleinberg <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
    method="kleinberg", type="BSR")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Balanced Ricochet (kleinberg centrality ) trials on given data source.
################################################################################
data.trials.ricochet.BSR.kleinberg <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.BSR.kleinberg.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.BSR.kleinberg(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Balanced Ricochet (kleinberg centrality ) trials.
################################################################################
data.sources.run.ricochet.BSR.kleinberg <- function()
{
  #the cu's
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.BSR.kleinberg(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Balanced Ricochet (eigen centrality ) experiments
################################################################################
a.trial.ricochet.BSR.evcent <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
    method="evcent", type="BSR")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Balanced Ricochet (eigen centrality ) trials on given data source.
################################################################################
data.trials.ricochet.BSR.evcent <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.BSR.evcent.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.BSR.evcent(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Balanced Ricochet (eigen centrality ) trials.
################################################################################
data.sources.run.ricochet.BSR.evcent <- function()
{
  #the cu's
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.BSR.evcent(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Balanced Ricochet (markov centrality ) experiments
################################################################################
a.trial.ricochet.BSR.markov <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- ricochet.cluster(the.graph,
    method="markov", type="BSR")));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
    #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Balanced Ricochet (markov centrality ) trials on given data source.
################################################################################
data.trials.ricochet.BSR.markov <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.ricochet.BSR.markov.txt", append=TRUE, type="output",
    #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.ricochet.BSR.markov(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Balanced Ricochet (markov centrality ) trials.
################################################################################
data.sources.run.ricochet.BSR.markov <- function()
{
  #the cu's
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.ricochet.BSR.markov(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for MinCut experiments
################################################################################
a.trial.min.cut <- function ( the.graph, the.true.cluster, theta, 
                              method = "mean", overlap=FALSE, order = 1,
                              alpha = 0.1, no.subsets=TRUE)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- min.cut.cluster(the.graph,method,
                                                          overlap,order,alpha,
                                                          no.subsets)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
                                       ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
                             ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(gmd.precision(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  print("Re");
  print(gmd.recall(a.clusters=the.cluster.output,
               ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
  #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
                                            ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs MinCut trials on given data source.
################################################################################
data.trials.min.cut <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv", method = "mean", overlap=FALSE, order = 1,
  alpha = 0.1, no.subsets=TRUE)
{
  #sink(file="data.center.txt", append=TRUE, type="output", split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in rev(1:9/10)) {
    a.trial.min.cut(the.graph, the.true.cluster, theta, method, overlap,
                    order, alpha, no.subsets);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs MinCut trials.
################################################################################
data.sources.run.min.cut <- function(method = "mean", overlap=FALSE, order = 1,
                                     alpha = 0.1, no.subsets=TRUE)
{
  #the cu's
  data.trials.min.cut(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv", method, overlap, order, alpha, no.subsets);
  #the f's
  data.trials.min.cut(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv", method, overlap, order, alpha, no.subsets);
  #unlink("tmp.txt");# this deletes the file
  data.trials.min.cut(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv", method, overlap, order, alpha, no.subsets);
  data.trials.min.cut(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv", method, overlap, order, alpha, no.subsets);
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Articulation Point experiments
################################################################################
a.trial.articulation.point <- function ( the.graph, the.true.cluster, theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(
    the.cluster.output <- articulation.point.clustering(the.graph)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
                                       ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
                             ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
               ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
  #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
                                            ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Articulation Point trials on given data source.
################################################################################
data.trials.articulation.point <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.center.txt", append=TRUE, type="output", split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.articulation.point(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Articulation Point trials.
################################################################################
data.sources.run.articulation.point <- function()
{
  #the cu's
  data.trials.articulation.point(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.articulation.point(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.articulation.point(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.articulation.point(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}
#-------------------------------------------------------------------------------
################################################################################
### Trial for Star (random centrality, without overlap) experiments.
### i.e. cc-pivot
################################################################################
a.trial.cc.pivot <- function ( the.graph, the.true.cluster,
                               theta)
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(the.cluster.output <- star.cluster(the.graph,
                                                       method="random",
                                                       overlap=FALSE)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
                                       ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
                             ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(gmd.precision(a.clusters=the.cluster.output,
                      ground.truth.clusters=the.true.cluster));
  print("Re");
  print(gmd.recall(a.clusters=the.cluster.output,
                   ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
  #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
                                            ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs ccpivot trials on given data source.
################################################################################
data.trials.cc.pivot <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv")
{
  #sink(file="data.star.degree.non.overlap.txt", append=TRUE, type="output",
  #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)) {
    a.trial.cc.pivot(the.graph, the.true.cluster, theta);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs ccpivot trials.
################################################################################
data.sources.run.cc.pivot <- function()
{
  #the cu's
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv");
  #the f's
  data.trials.cc.pivot(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv");
  #unlink("tmp.txt");# this deletes the file
  data.trials.cc.pivot(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv");
  data.trials.cc.pivot(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv");
}

#-------------------------------------------------------------------------------

################################################################################
### Trial for Ricochet experiments
################################################################################
a.trial.ricochet <- function ( the.graph, the.true.cluster, theta, 
                                       method='mean', algo='sr')
{
  print("theta");
  print(theta);
  the.graph <- delete.edges(the.graph, E(the.graph)[weight < theta]);
  print(system.time(try(the.cluster.output <- ricochet.cluster(the.graph, method,
                                                           algo))));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
                                       ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
                             ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(precision(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  print("Re");
  print(recall(a.clusters=the.cluster.output,
               ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K");
  print(k.measure(a.clusters=the.cluster.output,
                  ground.truth.clusters=the.true.cluster));
  #print("CK");
  #print(clustering.k.measure(a.clusters=the.cluster.output,
  #ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.variation.of.information(a.clusters=the.cluster.output,
                                            ground.truth.clusters=the.true.cluster));
  return(the.graph);
}
################################################################################
### Runs Ricochet trials on given data source.
################################################################################
data.trials.ricochet <- function(
  edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
  node.path="./../data/5k.csv", method='mean', algo='sr')
{
  #sink(file="data.ricochet.SR.mean.txt", append=TRUE, type="output",
  #split=TRUE);
  print(edge.path);
  the.true.cluster = load.true.cluster(file.path=node.path);
  the.graph = load.graph.csv(edge.path, node.path);
  for (theta in rev(2:9) / 10) {
    a.trial.ricochet(the.graph, the.true.cluster, theta, method, algo);
  }
  #sink();
  #rm(the.true.cluster);
  #rm(the.graph);
}
################################################################################
### Runs Ricochet (mean centrality ) trials.
################################################################################
data.sources.run.ricochet <- function(method='mean', algo='sr')
{
  #the cu's
  data.trials.ricochet(
    edge.path="./../data/scores_cu1_weightedjaccardbm25.csv",
    node.path="./../data/cu1.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_cu2_weightedjaccardbm25.csv",
    node.path="./../data/cu2.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_cu3_weightedjaccardbm25.csv",
    node.path="./../data/cu3.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_cu4_weightedjaccardbm25.csv",
    node.path="./../data/cu4.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_cu5_weightedjaccardbm25.csv",
    node.path="./../data/cu5.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_cu6_weightedjaccardbm25.csv",
    node.path="./../data/cu6.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv", method, algo);
  #the f's
  data.trials.ricochet(
    edge.path="./../data/scores_f1_weightedjaccardbm25.csv",
    node.path="./../data/f1.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_f2_weightedjaccardbm25.csv",
    node.path="./../data/f2.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_f3_weightedjaccardbm25.csv",
    node.path="./../data/f3.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_f4_weightedjaccardbm25.csv",
    node.path="./../data/f4.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_f5_weightedjaccardbm25.csv",
    node.path="./../data/f5.csv", method, algo);
  #unlink("tmp.txt");# this deletes the file
  data.trials.ricochet(
    edge.path="./../data/scores_5k_weightedjaccardbm25.csv",
    node.path="./../data/5k.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_10k_weightedjaccardbm25.csv",
    node.path="./../data/10k.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_20k_weightedjaccardbm25.csv",
    node.path="./../data/20k.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_50k_weightedjaccardbm25.csv",
    node.path="./../data/50k.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_100k_weightedjaccardbm25.csv",
    node.path="./../data/100k.csv", method, algo);
}

################################################################################
### Runs Ricochet (mean centrality ) trials.
################################################################################
data.sources.run.ricochet.leftovers <- function(method='mean', algo='sr')
{
  #the cu's
  data.trials.ricochet(
    edge.path="./../data/scores_cu7_weightedjaccardbm25.csv",
    node.path="./../data/cu7.csv", method, algo);
  data.trials.ricochet(
    edge.path="./../data/scores_cu8_weightedjaccardbm25.csv",
    node.path="./../data/cu8.csv", method, algo);
}
#-------------------------------------------------------------------------------



















































data.runs.gp.fold.stat <- function(a.f, a.data, a.gtest, a.gtc, a.clust.alg)
{
  fold.measure.function <- function(x) {data.gp.evaluation(f=a.f,
                            data=a.data, gtest=a.gtest,
                            ground.truth.clusters=a.gtc,
                            a.measure=x,
                            clust.algo=a.clust.alg)}
  test.precision <- fold.measure.function(gmd.precision);
  print(paste("Precision:", test.precision));
  test.recall <- fold.measure.function(gmd.recall);
  print(paste("Recall:", test.recall));
  test.k <- fold.measure.function(k.measure);
  print(paste("K:", test.k));
  test.norm.VI <- fold.measure.function(normalized.VI.fast);
  print(paste("norm.VI:", test.norm.VI));
}



###
data.runs.gp <- function(a.clust.algo=center.cluster,
                         a.ground.truth.clusters, data.source, num.folds=5, 
                         num.runs=10,
                         a.simple.functionSet=TRUE, an.evaluationLimit=50, 
                         a.transferLimit=20,
                         num.phases=10,
                         identification.boundary=1)
{
  # store best function
  best.functions <- c(); # will be a list of functions
  for(i in 1:num.runs)
  {
    fold.index = k.split.index(k=num.folds, a.data.frame=data.source);
    current.fold=data.source[fold.index==1,];
    gdata=fold.graph(current.fold);
    fold.truth.clusters=fold.cluster(
      ground.truth.clusters=a.ground.truth.clusters,
      a.current.fold=current.fold);
    print(paste("GP Run:",i));
    #--------------------------------------------------------------
    print("GP Run Training Time:");
    print(system.time({a.gp.result <- genetic.augmentation(
      simple.functionSet=a.simple.functionSet, clust.algo=a.clust.algo,
      ground.truth.clusters=fold.truth.clusters, data=current.fold,
      evaluationLimit=an.evaluationLimit, transferLimit=a.transferLimit,
      phases=num.phases)}));
    #--------------------------------------------------------------
    fitnessFunction1 <- function(x){ return( data.gp.evaluation(f=x, 
                              data=current.fold,
                              gtest=gdata, clust.algo=a.clust.algo,
                              ground.truth.clusters=fold.truth.clusters,
                              a.measure=gmd.F1.measure.fastest));}
    #NOTE: using which.max because not using rsme in fitnessFunction1
    a.gp.func <- a.gp.result$population[[which.max(sapply
      (a.gp.result$elite, fitnessFunction1))]];
    print("best GP Function:");
    print(a.gp.func); #not able to grab string representaion to paste
        
    # collecting best of the best (according to training)
    best.functions <- c(best.functions, a.gp.func);
    best.functions <- best.functions[[which.max(sapply
      (best.functions, fitnessFunction1))]];
    #--------------------------------------------------------------
    print("GP Run Testing:");
    print("Fold:");
    data.runs.gp.fold.stat(a.f=a.gp.func, a.data=current.fold,
                           a.gtest=gdata, a.gtc=fold.truth.clusters,
                           a.clust.alg=a.clust.algo);
    #--------------------------------------------------------------
    current.unfold=data.source[fold.index!=1,];
    gundata=fold.graph(current.unfold);
    unfold.truth.clusters=fold.cluster(
      ground.truth.clusters=a.ground.truth.clusters,
      a.current.fold=current.unfold);
    print("Unfold:");
    data.runs.gp.fold.stat(a.f=a.gp.func, a.data=current.unfold,
                           a.gtest=gundata, a.gtc=unfold.truth.clusters,
                           a.clust.alg=a.clust.algo);
  }
    return(best.functions); # best of the best used for testing theta robustness
}


a.trial.gp <- function(a.graph, a.clust.algo, the.true.cluster)
{
  print(system.time(the.cluster.output <- a.clust.algo(a.graph)));
  print("PCPr");
  print(penalized.clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("CPr");
  print(clustering.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Pr");
  print(gmd.precision(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Re");
  print(gmd.recall(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("Cluster#");
  print(length(the.cluster.output));
  print("K"); #NOTE: returning NaN or NA for some reason.
  print(k.measure(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
  print("VI");
  print(normalized.VI.fast(a.clusters=the.cluster.output,
    ground.truth.clusters=the.true.cluster));
}

data.robustness.trials.gp<- function(clust.algo=center.cluster, data, 
                                     ground.truth.clusters,
                                     a.gp.func=function(weightedjaccardbm25,
                                            jaccard, hmm, tfidf, editdistance)
                                              return(weightedjaccardbm25)
)
{
  print("Robustness trials:");
  for (theta in {1:9} /10) {
    # Avoid theta thresholding in this manner due to quick loss of edges
    #for(i in 3:dim(data)[2]) {
      # NOTE: Not possible to normalize and use as theta in any meaningful way 
      #       for hmm. 
      #if (i != 5) {
        #data = data[data[[i]] > theta, ];
      #}
    #}
    data = data[data[[3]] > theta, ];
    print("theta");
    print(theta);
    the.graph=graph.edgelist(as.matrix(data[1:2]), directed=FALSE);
    E(the.graph)$weight <- Vectorize(a.gp.func)(weightedjaccardbm25=data[[3]],
                                            jaccard=data[[4]],
                                            hmm=data[[5]],
                                            tfidf=data[[6]],
                                            editdistance=data[[7]]);
    a.trial.gp(the.graph, clust.algo, ground.truth.clusters);
  }  
}

data.run.gp.center.simple <- function(data, clust.algo=center.cluster,
                                      ground.truth.clusters, a.simplefuncset)
{
  return(data.runs.gp(a.clust.algo=clust.algo,
               a.ground.truth.clusters=ground.truth.clusters,
               data.source=data,
               a.simple.functionSet=a.simplefuncset));
}

################################################################################
### Runs for Genetic Programming Augmentation Framework for STRINGER trials
################################################################################
data.sources.run.gp.center.simple <- function()
{
  #the cu's
  for (i in 1:8) 
  { 
    node.path = paste("./../data/","cu", i,".csv", sep="")
    print(node.path);
    the.true.cluster = load.true.cluster(file.path=node.path);
    the.data = load.all.scores(data.file.type=paste("cu", i, sep=""))
    the.best.function = data.run.gp.center.simple(data=the.data,
                              ground.truth.clusters=the.true.cluster,
                              clust.algo=center.cluster,
                              a.simplefuncset=TRUE);
    #NOTE: We look at the best trainee because we assume we wouldn't have tested 
    #      the function on a ground truth different yet representative of the 
    #      types of entities resolved.
    print("Best GP Trained Function:");
    print(the.best.function);
    # similar to prior data trial functions but only on best of data set
    # REASON: Checking if robustness needs to be a part of GP or not
    # (i.e. is this an algo. property or mix between similarity and algo)
    data.robustness.trials.gp(clust.algo=center.cluster, data=the.data,
                              ground.truth.clusters=the.true.cluster,
                              a.gp.func= the.best.function);
  }
}

data.run.gp.center.complex <- function(data, clust.algo=center.cluster,
                                      ground.truth.clusters, a.simplefuncset)
{
  return(data.runs.gp(a.clust.algo=clust.algo,
               a.ground.truth.clusters=ground.truth.clusters,
               data.source=data,
               a.simple.functionSet=a.simplefuncset));
}

################################################################################
### Runs for Genetic Programming Augmentation Framework for STRINGER trials
################################################################################
data.sources.run.gp.center.complex <- function()
{
  #the cu's
  for (i in 1:8) { #TODO: load files for cu2-8
    node.path = paste("./../data/","cu", i,".csv", sep="")
    print(node.path);
    the.true.cluster = load.true.cluster(file.path=node.path);
    the.data = load.all.scores(data.file.type=paste("cu", i, sep=""))
    the.best.function = data.run.gp.center.complex(data=the.data,
                              ground.truth.clusters=the.true.cluster,
                              clust.algo=center.cluster,
                              a.simplefuncset=FALSE);
    #NOTE: We look at the best trainee because we assume we wouldn't have tested 
    #      the function on a ground truth different yet representative of the 
    #      types of entities resolved.
    print("Best GP Trained Function:");
    print(the.best.function);
    # similar to prior data trial functions but only on best of data set
    # REASON: Checking if robustness needs to be a part of GP or not
    # (i.e. is this an algo. property or mix between similarity and algo)
    data.robustness.trials.gp(clust.algo=center.cluster, data=the.data,
                              ground.truth.clusters=the.true.cluster,
                              a.gp.func= the.best.function);
  }
}

data.run.gp.merge.center.simple <- function(data, clust.algo=merge.center.cluster,
                                      ground.truth.clusters, a.simplefuncset)
{
  return(data.runs.gp(a.clust.algo=clust.algo,
               a.ground.truth.clusters=ground.truth.clusters,
               data.source=data,
               a.simple.functionSet=a.simplefuncset));
}

################################################################################
### Runs for Genetic Programming Augmentation Framework for STRINGER trials
################################################################################
data.sources.run.gp.merge.center.simple <- function()
{
  #the cu's
  for (i in 1:8) { #TODO: load files for cu2-8
    node.path = paste("./../data/","cu", i,".csv", sep="")
    print(node.path);
    the.true.cluster = load.true.cluster(file.path=node.path);
    the.data = load.all.scores(data.file.type=paste("cu", i, sep=""))
    the.best.function = data.run.gp.center.simple(data=the.data,
                              ground.truth.clusters=the.true.cluster,
                              clust.algo=merge.center.cluster,
                              a.simplefuncset=TRUE);
    #NOTE: We look at the best trainee because we assume we wouldn't have tested 
    #      the function on a ground truth different yet representative of the 
    #      types of entities resolved.
    print("Best GP Trained Function:");
    print(the.best.function);
    # similar to prior data trial functions but only on best of data set
    # REASON: Checking if robustness needs to be a part of GP or not
    # (i.e. is this an algo. property or mix between similarity and algo)
    data.robustness.trials.gp(clust.algo=merge.center.cluster, data=the.data,
                              ground.truth.clusters=the.true.cluster,
                              a.gp.func= the.best.function);
  }
}




data.run.gp.merge.center.complex <- function(data, clust.algo=merge.center.cluster,
                                      ground.truth.clusters, a.simplefuncset)
{
  return(data.runs.gp(a.clust.algo=clust.algo,
               a.ground.truth.clusters=ground.truth.clusters,
               data.source=data,
               a.simple.functionSet=a.simplefuncset));
}

################################################################################
### Runs for Genetic Programming Augmentation Framework for STRINGER trials
################################################################################
data.sources.run.gp.merge.center.complex <- function()
{
  #the cu's
  for (i in 1:8) { #TODO: load files for cu2-8
    node.path = paste("./../data/","cu", i,".csv", sep="")
    print(node.path);
    the.true.cluster = load.true.cluster(file.path=node.path);
    the.data = load.all.scores(data.file.type=paste("cu", i, sep=""))
    the.best.function = data.run.gp.center.simple(data=the.data,
                              ground.truth.clusters=the.true.cluster,
                              clust.algo=merge.center.cluster,
                              a.simplefuncset=FALSE);
    #NOTE: We look at the best trainee because we assume we wouldn't have tested 
    #      the function on a ground truth different yet representative of the 
    #      types of entities resolved.
    print("Best GP Trained Function:");
    print(the.best.function);
    # similar to prior data trial functions but only on best of data set
    # REASON: Checking if robustness needs to be a part of GP or not
    # (i.e. is this an algo. property or mix between similarity and algo)
    data.robustness.trials.gp(clust.algo=merge.center.cluster, data=the.data,
                              ground.truth.clusters=the.true.cluster,
                              a.gp.func= the.best.function);
  }
}










################################################################################
### Runs for Genetic Programming Augmentation Framework for STRINGER trials
################################################################################
data.sources.run.gp.merge.center.baseline <- function()
{
  #the cu's
  for (i in 1:8) { #TODO: load files for cu2-8
    node.path = paste("./../data/","cu", i,".csv", sep="")
    print(node.path);
    the.true.cluster = load.true.cluster(file.path=node.path);
    the.data = load.all.scores(data.file.type=paste("cu", i, sep=""))
    # similar to prior data trial functions but only on best of data set
    # REASON: Checking if robustness needs to be a part of GP or not
    # (i.e. is this an algo. property or mix between similarity and algo)
    data.robustness.trials.gp(clust.algo=merge.center.cluster, data=the.data,
                              ground.truth.clusters=the.true.cluster);
  }
}
################################################################################
### Runs for Genetic Programming Augmentation Framework for STRINGER trials
################################################################################
data.sources.run.gp.center.baseline <- function()
{
  #the cu's
  for (i in 1:8) { #TODO: load files for cu2-8
    node.path = paste("./../data/","cu", i,".csv", sep="")
    print(node.path);
    the.true.cluster = load.true.cluster(file.path=node.path);
    the.data = load.all.scores(data.file.type=paste("cu", i, sep=""))
    # similar to prior data trial functions but only on best of data set
    # REASON: Checking if robustness needs to be a part of GP or not
    # (i.e. is this an algo. property or mix between similarity and algo)
    data.robustness.trials.gp(clust.algo=center.cluster, data=the.data,
                              ground.truth.clusters=the.true.cluster);
  }
}
