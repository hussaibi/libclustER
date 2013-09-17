library("igraph");
library("Matrix");
#library("multicore");
library("inline");
library("compiler");
library("rgp");
source("load.R", echo=FALSE);
source("partitioning.R", echo=FALSE);
source("center.R", echo=FALSE);
source("merge-center.R", echo=FALSE);

source("star.R", echo=FALSE);
source("ricochet.R", echo=FALSE);
source("correlation.R", echo=FALSE);
source("mincut.R", echo=FALSE);
source("articulation.R", echo=FALSE);
source("markov.R", echo=FALSE);
source("ensemble.R", echo=FALSE);
source("test.R", echo=FALSE);

source("sparse.apcluster/AllClasses.R", echo=FALSE);
source("sparse.apcluster/cutree-methods.R", echo=FALSE);
source("sparse.apcluster/labels-methods.R", echo=FALSE);
source("sparse.apcluster/show-methods.R", echo=FALSE);
source("sparse.apcluster/apcluster.R", echo=FALSE);
source("affinity.R", echo=FALSE);

source("GPAugmentation.R", echo=FALSE);

source("measures.R", echo=FALSE);
