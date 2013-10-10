#data.sources.run.min.cut.random.non.overlap.workaround
################################################################################
### The purpose of this script is to clean up after every trial. Thus saving
### memory. only doing this for 0.2
################################################################################
rm(list=ls());
gc(reset=TRUE);
source("libcluster.R", echo=FALSE);
theta <- 0.3;
method <- "random";
overlap <- FALSE;
order <- 1;
alpha <- 0.2;
no.subsets <- TRUE;
edge.path="./../data/scores_cu1_weightedjaccardbm25.csv"
node.path="./../data/cu1.csv"
print(edge.path);
the.true.cluster = load.true.cluster(file.path=node.path);
the.graph = load.graph.csv(edge.path, node.path);

a.trial.min.cut(the.graph, the.true.cluster, theta, method, overlap,
                order, alpha, no.subsets);



rm(list=ls());
gc(reset=TRUE);
source("libcluster.R", echo=FALSE);
theta <- 0.3;
method <- "random";
overlap <- FALSE;
order <- 1;
alpha <- 0.2;
no.subsets <- TRUE;
edge.path="./../data/scores_cu2_weightedjaccardbm25.csv";
node.path="./../data/cu2.csv";
print(edge.path);
the.true.cluster = load.true.cluster(file.path=node.path);
the.graph = load.graph.csv(edge.path, node.path);

a.trial.min.cut(the.graph, the.true.cluster, theta, method, overlap,
                order, alpha, no.subsets);


rm(list=ls());
gc(reset=TRUE);
source("libcluster.R", echo=FALSE);
theta <- 0.3;
method <- "random";
overlap <- FALSE;
order <- 1;
alpha <- 0.2;
no.subsets <- TRUE;
edge.path="./../data/scores_cu3_weightedjaccardbm25.csv";
node.path="./../data/cu3.csv";
print(edge.path);
the.true.cluster = load.true.cluster(file.path=node.path);
the.graph = load.graph.csv(edge.path, node.path);

a.trial.min.cut(the.graph, the.true.cluster, theta, method, overlap,
                order, alpha, no.subsets);


rm(list=ls());
gc(reset=TRUE);
source("libcluster.R", echo=FALSE);
theta <- 0.3;
method <- "random";
overlap <- FALSE;
order <- 1;
alpha <- 0.2;
no.subsets <- TRUE;
edge.path="./../data/scores_cu4_weightedjaccardbm25.csv";
node.path="./../data/cu4.csv";
print(edge.path);
the.true.cluster = load.true.cluster(file.path=node.path);
the.graph = load.graph.csv(edge.path, node.path);

a.trial.min.cut(the.graph, the.true.cluster, theta, method, overlap,
                order, alpha, no.subsets);



rm(list=ls());
gc(reset=TRUE);
source("libcluster.R", echo=FALSE);
theta <- 0.3;
method <- "random";
overlap <- FALSE;
order <- 1;
alpha <- 0.2;
no.subsets <- TRUE;
edge.path="./../data/scores_cu5_weightedjaccardbm25.csv";
node.path="./../data/cu5.csv";
print(edge.path);
the.true.cluster = load.true.cluster(file.path=node.path);
the.graph = load.graph.csv(edge.path, node.path);

a.trial.min.cut(the.graph, the.true.cluster, theta, method, overlap,
                order, alpha, no.subsets);




rm(list=ls());
gc(reset=TRUE);
source("libcluster.R", echo=FALSE);
theta <- 0.3;
method <- "random";
overlap <- FALSE;
order <- 1;
alpha <- 0.2;
no.subsets <- TRUE;
edge.path="./../data/scores_cu6_weightedjaccardbm25.csv";
node.path="./../data/cu6.csv";
print(edge.path);
the.true.cluster = load.true.cluster(file.path=node.path);
the.graph = load.graph.csv(edge.path, node.path);

a.trial.min.cut(the.graph, the.true.cluster, theta, method, overlap,
                order, alpha, no.subsets);


rm(list=ls());
gc(reset=TRUE);
source("libcluster.R", echo=FALSE);
theta <- 0.3;
method <- "random";
overlap <- FALSE;
order <- 1;
alpha <- 0.2;
no.subsets <- TRUE;
edge.path="./../data/scores_cu7_weightedjaccardbm25.csv";
node.path="./../data/cu7.csv";
print(edge.path);
the.true.cluster = load.true.cluster(file.path=node.path);
the.graph = load.graph.csv(edge.path, node.path);

a.trial.min.cut(the.graph, the.true.cluster, theta, method, overlap,
                order, alpha, no.subsets);


rm(list=ls());
gc(reset=TRUE);
source("libcluster.R", echo=FALSE);
theta <- 0.3;
method <- "random";
overlap <- FALSE;
order <- 1;
alpha <- 0.2;
no.subsets <- TRUE;
edge.path="./../data/scores_cu8_weightedjaccardbm25.csv";
node.path="./../data/cu8.csv";print(edge.path);
the.true.cluster = load.true.cluster(file.path=node.path);
the.graph = load.graph.csv(edge.path, node.path);

a.trial.min.cut(the.graph, the.true.cluster, theta, method, overlap,
                order, alpha, no.subsets);

rm(list=ls());
gc(reset=TRUE);
