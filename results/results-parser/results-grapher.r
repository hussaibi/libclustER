library(plotrix)
#setwd("~/dbgroup/libcluster.alt/trunk/results/results-parser");
old.output.single <- read.csv("../old/uniform-single-error.csv", header=TRUE);
old.output.error <- read.csv("../old/uniform-error.csv", header=TRUE);
output <- read.csv("output.csv", header=FALSE);
graph.output <- read.csv("graph.output.csv", header=FALSE);
graph.scale.output <- read.csv("graph.scale.output.csv", header=FALSE);

# Reseting names for easier reference
names(old.output.single) <- c('Algo', 'table', 'class', 'theta', 'PCPr', 'CPr',
                              'Pr', 'Re', 'F1', 'ClusterNum');
names(old.output.error) <- c('Algo', 'table', 'class', 'theta', 'PCPr', 'CPr',
                             'Pr', 'Re', 'F1', 'ClusterNum');
#re-align old data
align.old.by.file <- function(old.output.error) {
  old.output.error$file = "";
  tmp = character(length=dim(old.output.error)[2]);
  
  bool_check = old.output.error$table %in% c('h1', 'H1');
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " ./../data/scores_cu1_weightedjaccardbm25.csv ";
  old.output.error[bool_check, ]$file = tmp ;
  
  bool_check = old.output.error$table %in% c('h2', 'H2');
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " ./../data/scores_cu2_weightedjaccardbm25.csv ";
  old.output.error[bool_check, ]$file = tmp ;
  
  bool_check = old.output.error$table %in% c('m1', 'M1');
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " ./../data/scores_cu3_weightedjaccardbm25.csv ";
  old.output.error[bool_check, ]$file = tmp ;
  
  bool_check = old.output.error$table %in% c('m2', 'M2');
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " ./../data/scores_cu4_weightedjaccardbm25.csv ";
  old.output.error[bool_check, ]$file = tmp ;
  
  bool_check = old.output.error$table %in% c('m3', 'M3');
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " ./../data/scores_cu5_weightedjaccardbm25.csv ";
  old.output.error[bool_check, ]$file = tmp ;
  
  bool_check = old.output.error$table %in% c('m4', 'M4');
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " ./../data/scores_cu6_weightedjaccardbm25.csv ";
  old.output.error[bool_check, ]$file = tmp ;
  
  
  bool_check = old.output.error$table %in% c('l1', 'L1');
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " ./../data/scores_cu7_weightedjaccardbm25.csv ";
  old.output.error[bool_check, ]$file = tmp ;
  
  bool_check = old.output.error$table %in% c('l2', 'L2');
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " ./../data/scores_cu8_weightedjaccardbm25.csv ";
  old.output.error[bool_check, ]$file = tmp ;
  
  return(old.output.error)
}
align.old.by.algo <- function(old.output.error) {
  old.output.error$algo = "";
  
  bool_check = old.output.error$Algo %in% "mincut";
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " cut ";
  old.output.error[bool_check, ]$algo = tmp ;

  bool_check = old.output.error$Algo %in% "star";
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " star-degree-overlap ";
  old.output.error[bool_check, ]$algo = tmp ;

  bool_check = old.output.error$Algo %in% "mergec";
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " merge-center ";
  old.output.error[bool_check, ]$algo = tmp ;

  bool_check = old.output.error$Algo %in% "center";
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " center ";
  old.output.error[bool_check, ]$algo = tmp ;
  
  bool_check = old.output.error$Algo %in% "artpt";
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " articulation-point ";
  old.output.error[bool_check, ]$algo = tmp ;
  
  bool_check = old.output.error$Algo %in% "part";
  tmp = character(length=sum(bool_check));
  tmp[TRUE] = " partition ";
  old.output.error[bool_check, ]$algo = tmp ;

  return(old.output.error)
}
old.output.error = align.old.by.algo(old.output.error=old.output.error);
old.output.error = align.old.by.file(old.output.error=old.output.error);
#
names(output) <- c("ClusterNum", "PCPr", "K", "VI", "CPr", "theta", "Re", 'elapsed',
                   'algo', 'file', "Pr");
#
merge.old.and.new.data <- function(old.data, new.data){
  tmp = merge(x=new.data, y=old.data,by=c("theta", "algo", "file"),
               all=FALSE);
  
  tmp[["Pr.x"]] = NULL; tmp[["Re.x"]] = NULL; tmp[["CPr.x"]] = NULL;
  tmp[["PCPr.x"]] = NULL; tmp[["F1.x"]] = NULL; tmp[["class"]] = NULL;
  tmp[["ClusterNum.x"]] = NULL; tmp[["Algo"]] = NULL; tmp[["table"]] = NULL;

  names(tmp) = c("theta", "algo", "file", "K", "VI", "elapsed", "PCPr", "CPr",
                 "Pr", "Re", "F1", "ClusterNum");
  #update
  rownames( new.data ) <- paste( new.data$theta, new.data$algo,
                                 new.data$file);
  new.data[ paste( tmp$theta, tmp$algo,
                   tmp$file ), ]$Pr <- tmp$Pr
  new.data[ paste( tmp$theta, tmp$algo,
                   tmp$file ), ]$Re <- tmp$Re
  new.data[ paste( tmp$theta, tmp$algo,
                   tmp$file ), ]$CPr <- tmp$CPr
  new.data[ paste( tmp$theta, tmp$algo,
                   tmp$file ), ]$PCPr <- tmp$PCPr
  rownames( new.data ) <- NULL;
  return(new.data);
}
#output = merge.old.and.new.data(old.data=old.output.error, new.data=output);
#
names(graph.output) <- c("LocalAverageGraphDegreeDensity", "LocalAverageTransitivity"
  , "GraphEdgeDensity", "theta", "GraphStrengthDensity", "StdMarkov"
  , "LocalAverageGraphStrengthDensity", "file", "AvgEVCent"
  , "MeanNeighbourhoodMeanStrength", "GlobalTransitivity");
names(graph.scale.output) <- c("max{N(V)}", "theta", "|V|", "file", "|E|");
#inner join
output = merge(output,graph.output,by=c("theta","file"));
output = merge(output,graph.scale.output,by=c("theta","file"));
# Calculating F1 cause I never did it -_-;;
output[["F1"]] <- 2*output[["Pr"]]*output[["Re"]] /
  (output[["Pr"]]+output[["Re"]]);


#Different Data sets
error.high = output[output[['file']] %in%
  c(" ./../data/scores_cu1_weightedjaccardbm25.csv "
    , " ./../data/scores_cu2_weightedjaccardbm25.csv "),];
by.err.high = list(as.numeric(error.high[["theta"]])
                   , as.character(error.high[["algo"]]));
omean.err.high=aggregate(x = error.high, by =by.err.high, FUN = "mean");

error.medium = output[output[['file']] %in%
  c(" ./../data/scores_cu3_weightedjaccardbm25.csv "
    , " ./../data/scores_cu4_weightedjaccardbm25.csv "
    , " ./../data/scores_cu5_weightedjaccardbm25.csv "
    , " ./../data/scores_cu6_weightedjaccardbm25.csv "),];
by.err.medium = list(as.numeric(error.medium[["theta"]])
                   , as.character(error.medium[["algo"]]));
omean.err.medium=aggregate(x = error.medium, by =by.err.medium, FUN = "mean");

error.low = output[output[['file']] %in%
  c(" ./../data/scores_cu7_weightedjaccardbm25.csv "
    , " ./../data/scores_cu8_weightedjaccardbm25.csv "),];
by.err.low = list(as.numeric(error.low[["theta"]])
                   , as.character(error.low[["algo"]]));
omean.err.low=aggregate(x = error.low, by =by.err.low, FUN = "mean");

error.none = output[output[['file']] %in%
  c(" ./../data/scores_f1_weightedjaccardbm25.csv "
    , " ./../data/scores_f2_weightedjaccardbm25.csv "
    , " ./../data/scores_f3_weightedjaccardbm25.csv "
    , " ./../data/scores_f4_weightedjaccardbm25.csv "
    , " ./../data/scores_f5_weightedjaccardbm25.csv "),];
by.err.none = list(as.numeric(error.none[["theta"]])
                  , as.character(error.none[["algo"]]));
omean.err.none=aggregate(x = error.none, by=by.err.none, FUN = "mean");




#Grouping data for aggragation
by1 = list(as.numeric(output[["theta"]]), as.character(output[["algo"]])
           , as.character(output[["file"]]))
omean=aggregate(x = output, by = by1, FUN = "mean")
#osd=aggregate(x = output, by = by1, FUN = "sd")
#pdf(paper="a4r", width=11, height=8);


##################################################################################
## Theta Variant
plot.theta <- function(omean, x.lab, y.lab, hc=gray(1:9/10), main) {
  par(mfrow=c(1,2))
  plot(omean[[x.lab]], omean[[y.lab]], xlab=x.lab, ylab=y.lab,main=main, col=gray(1))
  for(theta in 1:9/10){
    x.2 <- as.numeric(omean[omean[["theta"]]==theta,][[x.lab]])
    y.2 <- as.numeric(omean[omean[["theta"]]==theta,][[y.lab]])
    points(x.2,y.2,col=hc[theta*10])
  }
  plot.new(); 
  plot.window(c(0,1), c(0,1));
  legend(0,1, 1:9/10, col=hc, bty="n", cex=1, pch="o")
}
plot.theta.means <- function(omean, x.lab, y.lab, hc=gray(1:9/10), main) {
  par(mfrow=c(1,2))
  plot(omean[[x.lab]], omean[[y.lab]], xlab=x.lab, ylab=y.lab,main=main, col=gray(1))
  tmpform=as.formula(paste(paste("~", x.lab, sep=""), y.lab, sep="+"))
  for(theta in 1:9/10) {
    x.2 <- mean(as.numeric(omean[omean[["theta"]]==theta,][[x.lab]]))
    y.2 <- mean(as.numeric(omean[omean[["theta"]]==theta,][[y.lab]]))
    tmpframe=data.frame(as.numeric(omean[omean[["theta"]]==theta,][[x.lab]]),
                       as.numeric(omean[omean[["theta"]]==theta,][[y.lab]]))
    names(tmpframe)=c(x.lab,y.lab)
    pcan = prcomp(formula=tmpform,data=tmpframe)
    points(x.2,y.2,col=hc[theta*10])
    draw.ellipse(x=pcan[["center"]][x.lab],y=pcan[["center"]][y.lab],
                 a=pcan[["sdev"]][1],b=pcan[["sdev"]][2],
                 angle=acos(pcan[["rotation"]][1,1])/pi*180)
  }
  plot.new(); 
  plot.window(c(0,1), c(0,1));
  legend(0,1, 1:9/10, col=hc, bty="n", cex=1, pch="o")
}

plot.theta.median <- function(omean, x.lab, y.lab, hc=gray(1:9/10), main) {
  par(mfrow=c(1,2))
  plot(omean[[x.lab]], omean[[y.lab]], xlab=x.lab, ylab=y.lab,main=main, col=gray(1))
  tmpform=as.formula(paste(paste("~", x.lab, sep=""), y.lab, sep="+"))
  for(theta in 1:9/10) {
    x.2 <- median(as.numeric(omean[omean[["theta"]]==theta,][[x.lab]]))
    y.2 <- median(as.numeric(omean[omean[["theta"]]==theta,][[y.lab]]))
    tmpframe=data.frame(as.numeric(omean[omean[["theta"]]==theta,][[x.lab]]),
                       as.numeric(omean[omean[["theta"]]==theta,][[y.lab]]))
    names(tmpframe)=c(x.lab,y.lab)
    pcan = prcomp(formula=tmpform,data=tmpframe)
    points(x.2,y.2,col=hc[theta*10])
    draw.ellipse(x=pcan[["center"]][x.lab],y=pcan[["center"]][y.lab],
                 a=pcan[["sdev"]][1],b=pcan[["sdev"]][2],
                 angle=acos(pcan[["rotation"]][1,1])/pi*180)
  }
  plot.new(); 
  plot.window(c(0,1), c(0,1));
  legend(0,1, 1:9/10, col=hc, bty="n", cex=1, pch="o")
}


plot.theta.singlepass <- function(omean, x.lab, y.lab, main) {
  #for partitioning
  #plot.theta(omean[omean[["Group.2"]]==" partition ", ], x.lab, y.lab
  #           , main=paste(main,"for Partitioning"))
  plot.theta.means(omean[omean[["Group.2"]]==" partition ", ], x.lab, y.lab
                   , main=paste(main,"for Partitioning"))
  #for merge-center
  #plot.theta(omean[omean[["Group.2"]]==" merge-center ",], x.lab, y.lab
  #           , main=paste(main, "for Merge-Center"))
  plot.theta.means(omean[omean[["Group.2"]]==" merge-center ",], x.lab, y.lab
                   , main=paste(main, "for Merge-Center"))
  #for center
  #plot.theta(omean[omean[["Group.2"]]==" center ",], x.lab, y.lab
  #           , main=paste(main,"for Center"))
  plot.theta.means(omean[omean[["Group.2"]]==" center ",], x.lab, y.lab
                   , main=paste(main,"for Center"))
  #for center-degree
  #plot.theta(omean[omean[["Group.2"]]==" center-degree ",], x.lab, y.lab
  #           , main=paste(main,"for Center-Degree"))
  plot.theta.means(omean[omean[["Group.2"]]==" center-degree ",], x.lab, y.lab
                   , main=paste(main,"for Center-Degree"))
  #for center-sum
  #plot.theta(omean[omean[["Group.2"]]==" center-sum ",], x.lab, y.lab
  #           , main=paste(main,"for Center-Sum"))
  plot.theta.means(omean[omean[["Group.2"]]==" center-sum ",], x.lab, y.lab
                   , main=paste(main,"for Center-Sum"))
  #for  center-evcent 
  #plot.theta(omean[omean[["Group.2"]]==" center-evcent ",], x.lab, y.lab
  #           , main=paste(main,"for Center-Eigen"))
  plot.theta.means(omean[omean[["Group.2"]]==" center-evcent ",], x.lab, y.lab
                   , main=paste(main,"for Center-Eigen"))
  #for   center-kleinberg  
  #plot.theta(omean[omean[["Group.2"]]==" center-kleinberg ",], x.lab, y.lab
  #           , main=paste(main,"for Center-Kleinberg"))
  plot.theta.means(omean[omean[["Group.2"]]==" center-kleinberg ",], x.lab, y.lab
                   , main=paste(main,"for Center-Kleinberg"))
}

plot.theta.affinity.prelim <- function(omean, x.lab, y.lab, main) {
  #for merge-center
  #plot.theta(omean[omean[["Group.2"]]==" merge-center ",], x.lab, y.lab
  #           , main=paste(main, "for Merge-Center"))
  plot.theta.means(omean[omean[["Group.2"]]==" merge-center ",], x.lab, y.lab
                   , main=paste(main, "for Merge-Center"))
  plot.theta.means(omean[omean[["Group.2"]]==" mcl ",], x.lab, y.lab
                   , main=paste(main, "for MCL"))
  plot.theta.means(omean[omean[["Group.2"]]==" affinity-propagation-uniform(=0) ",], x.lab, y.lab
                   , main=paste(main, "for AP"))
}


plot.theta.star.non.overlap <- function(omean, x.lab, y.lab, main) {
  #for  star-degree-non-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-degree-non-overlap ", ], x.lab, y.lab
  #           , main=paste(main,"for Star-Degree-Partitioning"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-degree-non-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Degree-Partitioning"))
  #for  star-sum-non-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-sum-non-overlap ", ], x.lab, y.lab
  #           , main=paste(main,"for Star-Sum-Partitioning"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-sum-non-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Sum-Partitioning"))
  #for  star-sum-non-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-mean-non-overlap ", ], x.lab, y.lab
  #           , main=paste(main,"for Star-Mean-Partitioning"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-mean-non-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Mean-Partitioning"))
  #for  star-evcent-non-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-evcent-non-overlap ", ], x.lab, y.lab
  #           , main=paste(main,"for Star-Eigen-Partitioning"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-evcent-non-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Eigen-Partitioning"))
  #for  star-kleinberg-non-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-kleinberg-non-overlap ", ], x.lab
  #           , y.lab , main=paste(main,"for Star-Kleinberg-Partitioning"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-kleinberg-non-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Kleinberg-Partitioning"))
  #for  star-markov-non-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-markov-non-overlap ", ], x.lab, y.lab
  #           , main=paste(main,"for Star-Markov-Partitioning"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-markov-non-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Markov-Partitioning"))
}

plot.theta.star.overlap <- function(omean, x.lab, y.lab, main) {
  #for  star-degree-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-degree-overlap ", ], x.lab, y.lab
  #           , main=paste(main,"for Star-Degree-Clustering"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-degree-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Degree-Clustering"))
  #for  star-sum-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-sum-overlap ", ], x.lab, y.lab
  #           , main=paste(main,"for Star-Sum-Clustering"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-sum-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Sum-Clustering"))
  #for  star-sum-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-mean-overlap ", ], x.lab, y.lab
  #           , main=paste(main,"for Star-Mean-Clustering"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-mean-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Mean-Clustering"))
  #for  star-evcent-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-evcent-overlap ", ], x.lab, y.lab
  #           , main=paste(main,"for Star-Eigen-Clustering"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-evcent-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Eigen-Clustering"))
  #for  star-kleinberg-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-kleinberg-overlap ", ], x.lab
  #           , y.lab , main=paste(main,"for Star-Kleinberg-Clustering"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-kleinberg-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Kleinberg-Clustering"))
  #for  star-markov-overlap 
  #plot.theta(omean[omean[["Group.2"]]==" star-markov-overlap ", ], x.lab, y.lab
  #           , main=paste(main,"for Star-Markov-Clustering"))
  plot.theta.means(omean[omean[["Group.2"]]==" star-markov-overlap ", ], x.lab
                   , y.lab , main=paste(main,"for Star-Markov-Clustering"))
}


# #for graph properties
# boxplot(formula=ClusterNum~theta
#         , data=omean[omean[["Group.2"]]==" partition "
#                      & !(omean[["Group.3"]] %in% 
#                        c(" ./../data/scores_10k_weightedjaccardbm25.csv "
#                          ," ./../data/scores_20k_weightedjaccardbm25.csv "
#                          ," ./../data/scores_50k_weightedjaccardbm25.csv "
#                          ," ./../data/scores_100k_weightedjaccardbm25.csv "))
#                      ,], horizontal=TRUE, main= "Graph Component Composition",
#         ylab="Theta", xlab="Cluster#")
# #for all
# plot.theta(omean, "K", "F1", main="Match Fragmentation by Theta")
# plot.theta.means(omean, "K", "F1", main="Match Fragmentation by Theta")
# plot.theta(omean, "F1", "VI", main="Match Correctness by Theta")
# plot.theta.means(omean, "F1", "VI", main="Match Correctness by Theta")
# plot.theta(omean, "VI", "K", main="Data Cleaning by Theta")
# plot.theta.means(omean, "VI", "K", main="Data Cleaning by Theta")
# plot.theta(omean, "Re", "Pr", main="ROC by Theta")
# plot.theta.means(omean, "Re", "Pr", main="ROC by Theta")
# 
# # names(graph.output) <- c("LocalAverageGraphDegreeDensity", "LocalAverageTransitivity"
# #   , "GraphEdgeDensity", "theta", "GraphStrengthDensity", "StdMarkov"
# #   , "LocalAverageGraphStrengthDensity", "file", "AvgEVCent"
# #   , "MeanNeighbourhoodMeanStrength", "GlobalTransitivity")
# 
# plot.theta(omean, "K", "LocalAverageGraphDegreeDensity", main="")
# plot.theta(omean, "K", "LocalAverageTransitivity", main="")
# plot.theta(omean, "K", "GraphEdgeDensity", main="")
# plot.theta(omean, "K", "GraphStrengthDensity", main="")
# plot.theta(omean, "K", "StdMarkov", main="")
# plot.theta(omean, "K", "LocalAverageGraphStrengthDensity", main="")
# plot.theta(omean, "K", "AvgEVCent", main="")
# plot.theta(omean, "K", "MeanNeighbourhoodMeanStrength", main="")
# plot.theta(omean, "K", "GlobalTransitivity", main="")
# 
# plot.theta(omean, "VI", "LocalAverageGraphDegreeDensity", main="")
# plot.theta(omean, "VI", "LocalAverageTransitivity", main="")
# plot.theta(omean, "VI", "GraphEdgeDensity", main="")
# plot.theta(omean, "VI", "GraphStrengthDensity", main="")
# plot.theta(omean, "VI", "StdMarkov", main="")
# plot.theta(omean, "VI", "LocalAverageGraphStrengthDensity", main="")
# plot.theta(omean, "VI", "AvgEVCent", main="")
# plot.theta(omean, "VI", "MeanNeighbourhoodMeanStrength", main="")
# plot.theta(omean, "VI", "GlobalTransitivity", main="")
# 
# plot.theta(omean, "F1", "LocalAverageGraphDegreeDensity", main="")
# plot.theta(omean, "F1", "LocalAverageTransitivity", main="")
# plot.theta(omean, "F1", "GraphEdgeDensity", main="")
# plot.theta(omean, "F1", "GraphStrengthDensity", main="")
# plot.theta(omean, "F1", "StdMarkov", main="")
# plot.theta(omean, "F1", "LocalAverageGraphStrengthDensity", main="")
# plot.theta(omean, "F1", "AvgEVCent", main="")
# plot.theta(omean, "F1", "MeanNeighbourhoodMeanStrength", main="")
# plot.theta(omean, "F1", "GlobalTransitivity", main="")
# 
# # Describing effect of theta on data
# # promising metrics to compare (local/global Transitivity, Strength Density,
# #  Degree Density)
# plot.theta(omean, "GraphEdgeDensity", "GlobalTransitivity"
#            , main="Data Completeness (Global)")
# plot.theta.means(omean, "GraphEdgeDensity", "GlobalTransitivity"
#                  , main="Data Completeness (Global)")
# plot.theta(omean, "LocalAverageGraphDegreeDensity", "LocalAverageTransitivity"
#            , main="Data Completeness (Local)")
# plot.theta.means(omean, "LocalAverageGraphDegreeDensity"
#                  , "LocalAverageTransitivity", main="Data Completeness (Local)")
# # for K and VI
# plot.theta(omean,"GraphStrengthDensity", "GraphEdgeDensity"
#            , main="Potential Regression Parameters (Global)")
# plot.theta.means(omean,"GraphStrengthDensity", "GraphEdgeDensity"
#                  , main="Potential Regression Parameters (Global)")
# plot.theta(omean,"LocalAverageGraphStrengthDensity"
#            , "LocalAverageGraphDegreeDensity"
#            , main="Potential Regression Parameters (Local)")
# plot.theta.means(omean,"LocalAverageGraphStrengthDensity"
#                  , "LocalAverageGraphDegreeDensity"
#                  , main="Potential Regression Parameters (Local)")
# 
# #for single pass
# plot.theta.singlepass(omean, "K", "F1", main="Match Fragmentation by Theta")
# plot.theta.singlepass(omean, "F1", "VI", main="Match Correctness by Theta")
# plot.theta.singlepass(omean, "VI", "K", main="Data Cleaning by Theta")
# plot.theta.singlepass(omean, "Re", "Pr", main="ROC by Theta")
# #for star
# plot.theta.star.non.overlap(omean, "K", "F1", main="Match Fragmentation by Theta")
# plot.theta.star.non.overlap(omean, "F1", "VI", main="Match Correctness by Theta")
# plot.theta.star.non.overlap(omean, "VI", "K", main="Data Cleaning by Theta")
# plot.theta.star.non.overlap(omean, "Re", "Pr", main="ROC by Theta")
# 
# plot.theta.star.overlap(omean, "K", "F1", main="Match Fragmentation by Theta")
# plot.theta.star.overlap(omean, "F1", "VI", main="Match Correctness by Theta")
# plot.theta.star.overlap(omean, "VI", "K", main="Data Cleaning by Theta")
# plot.theta.star.overlap(omean, "Re", "Pr", main="ROC by Theta")

#################################################################################
## Algo Variant
# Resolution Fragmentation
plot.algo <- function(omean, x.lab, y.lab, hc=rainbow(n=length(algos), alpha=1)
                       , main, algos=unique(omean[["Group.2"]]))
{
  par(mfrow=c(1,2))
  plot(omean[[x.lab]], omean[[y.lab]], xlab=x.lab, ylab=y.lab,main=main,
       col=gray(1))
  for(a.algo.i in 1:length(algos)){
    a.algo=algos[a.algo.i]
    x.2 <- as.numeric(omean[omean[["Group.2"]]==a.algo,][[x.lab]])
    y.2 <- as.numeric(omean[omean[["Group.2"]]==a.algo,][[y.lab]])
    points(x.2,y.2,col=hc[a.algo.i], pch=a.algo.i)
  }
  plot.new(); 
  plot.window(c(0,1), c(0,1));
  legend(0,1, algos, col=hc, bty="n", cex=0.5, pch=1:length(algos))
}
plot.algo.means <- function(omean, x.lab, y.lab, hc=rainbow(n=length(algos)
                                                            , alpha=1)
                            , main, algos=unique(omean[["Group.2"]])) {
  par(mfrow=c(1,2))
  plot(omean[[x.lab]], omean[[y.lab]], xlab=x.lab, ylab=y.lab,main=main, col=gray(1))
  tmpform=as.formula(paste(paste("~", x.lab, sep=""), y.lab, sep="+"))
  for(a.algo.i in 1:length(algos)){
    a.algo=algos[a.algo.i]
    x.2 <- mean(as.numeric(omean[omean[["Group.2"]]==a.algo,][[x.lab]]))
    y.2 <- mean(as.numeric(omean[omean[["Group.2"]]==a.algo,][[y.lab]]))
    tmpframe=data.frame(as.numeric(omean[omean[["Group.2"]]==a.algo,][[x.lab]]),
                       as.numeric(omean[omean[["Group.2"]]==a.algo,][[y.lab]]))
    names(tmpframe)=c(x.lab,y.lab)
    pcan = prcomp(formula=tmpform,data=tmpframe)
    points(x.2,y.2,col=hc[a.algo.i])
    draw.ellipse(x=pcan[["center"]][x.lab],y=pcan[["center"]][y.lab],
                 a=pcan[["sdev"]][1],b=pcan[["sdev"]][2],
                 angle=acos(pcan[["rotation"]][1,1])/pi*180)
  }
  plot.new(); 
  plot.window(c(0,1), c(0,1));
  legend(0,1, algos, col=hc, bty="n", cex=0.5, pch=1:length(algos))
}
  
plot.algo.singlepass <- function(omean, x.lab, y.lab, main) {
  for (theta in 1:9/10) {
    #plot.algo(omean1[omean1[["Group.1"]]==0.1, ], x.lab, y.lab
    #           , main=paste(main,"for theta = 0.1"))
    plot.algo.means(omean[omean[["Group.1"]]==theta, ], x.lab, y.lab
                      , main=paste(main,"for theta = ",theta)
                      , algos=c(" partition "," merge-center "
                                , " center ", " center-degree "
                                , " center-sum ", " center-mean "
                                , " center-evcent ", " center-kleinberg "
                                , " center-markov "))
  }
}

plot.algo.star.non.overlap <- function(omean, x.lab, y.lab, main) {
  for (theta in 1:9/10) {
    #plot.algo(omean1[omean1[["Group.1"]]==0.1, ], x.lab, y.lab
    #           , main=paste(main,"for theta = 0.1"))
    plot.algo.means(omean[omean[["Group.1"]]==theta, ], x.lab, y.lab
                      , main=paste(main,"for theta = ",theta)
                      , algos=c(" star-degree-non-overlap ", " partition "
                                ," star-evcent-non-overlap "
                                ," star-mean-non-overlap "
                                , " star-kleinberg-non-overlap "
                                , " star-sum-non-overlap "
                                , " star-markov-non-overlap "))
  }
}

plot.algo.star.overlap <- function(omean, x.lab, y.lab, main) {
  for (theta in 1:9/10) {
    #plot.algo(omean1[omean1[["Group.1"]]==0.1, ], x.lab, y.lab
    #           , main=paste(main,"for theta = 0.1"))
    plot.algo.means(omean[omean[["Group.1"]]==theta, ], x.lab, y.lab
                      , main=paste(main,"for theta = ",theta)
                      , algos=c(" star-degree-overlap ", " partition "
                                ," star-evcent-overlap "
                                ," star-mean-overlap "
                                , " star-kleinberg-overlap "
                                , " star-sum-overlap "
                                , " star-markov-overlap "))
  }
}
  
plot.algo.star.all <- function(omean, x.lab, y.lab, main) {
  for (theta in 1:9/10) {
    #plot.algo(omean1[omean1[["Group.1"]]==0.1, ], x.lab, y.lab
    #           , main=paste(main,"for theta = 0.1"))
    plot.algo.means(omean[omean[["Group.1"]]==theta, ], x.lab, y.lab
                      , main=paste(main,"for theta = ",theta)
                      , algos=c(" star-degree-overlap ", " partition "
                                ," star-evcent-overlap "
                                ," star-mean-overlap "
                                , " star-kleinberg-overlap "
                                , " star-sum-overlap "
                                , " star-markov-overlap "
                                , " star-degree-non-overlap "
                                ," star-evcent-non-overlap "
                                ," star-mean-non-overlap "
                                , " star-kleinberg-non-overlap "
                                , " star-sum-non-overlap "
                                , " star-markov-non-overlap "
                                ))
  }
}

plot.algo.mcl <- function(omean, x.lab, y.lab, main) {
  for (theta in 3:9/10) {
    #     plot.algo(omean1[omean1[["Group.1"]]==0.1, ], x.lab, y.lab
    #                , main=paste(main,"for theta = 0.1"))
    plot.algo.means(omean[omean[["Group.1"]]==theta, ], x.lab, y.lab
                    , main=paste(main,"for theta = ",theta)
                    , algos=c(" mcl ", " partition ", " center ", " merge-center ",
                              " star-mean-non-overlap "))
  }
}

plot.algo.affinity.prelim <- function(omean, x.lab, y.lab, main) {
  for (theta in 3:9/10) {
    #     plot.algo(omean1[omean1[["Group.1"]]==0.1, ], x.lab, y.lab
    #                , main=paste(main,"for theta = 0.1"))
    plot.algo.means(omean[omean[["Group.1"]]==theta, ], x.lab, y.lab
                    , main=paste(main,"for theta = ",theta)
                    , algos=c(" mcl ", " merge-center ",
                              " affinity-propagation-uniform(=1) "))
  }
}

  
# #for all
# plot.algo(omean, "K", "F1", main="Match Fragmentation by Algorithm")
# plot.algo.means(omean, "K", "F1", main="Match Fragmentation by Algorithm")
# plot.algo(omean, "F1", "VI", main="Match Correctness by Algorithm")
# plot.algo.means(omean, "F1", "VI", main="Match Correctness by Algorithm")
# plot.algo(omean, "VI", "K", main="Data Cleaning by Algorithm")
# plot.algo.means(omean, "VI", "K", main="Data Cleaning by Algorithm")
# # #for single pass
#  plot.algo.singlepass(omean, "K", "F1", main="Match Fragmentation by Algorithm")
#  plot.algo.singlepass(omean, "F1", "VI", main="Match Correctness by Algorithm")
#  plot.algo.singlepass(omean, "VI", "K", main="Data Cleaning by Algorithm")
# # #for star
#  plot.algo.star.non.overlap(omean, "K", "F1", main="Match Fragmentation by Algorithm")
#  plot.algo.star.non.overlap(omean, "F1", "VI", main="Match Correctness by Algorithm")
#  plot.algo.star.non.overlap(omean, "VI", "K", main="Data Cleaning by Algorithm")
#  
#  plot.algo.star.overlap(omean, "K", "F1", main="Match Fragmentation by Algorithm")
#  plot.algo.star.overlap(omean, "F1", "VI", main="Match Correctness by Algorithm")
#  plot.algo.star.overlap(omean, "VI", "K", main="Data Cleaning by Algorithm")
# 
#  plot.algo.star.all(omean, "K", "F1", main="Match Fragmentation by Algorithm")
#  plot.algo.star.all(omean, "F1", "VI", main="Match Correctness by Algorithm")
#  plot.algo.star.all(omean, "VI", "K", main="Data Cleaning by Algorithm")

#plot.algo.mcl(omean, "K", "F1", main="Match Fragmentation by Algorithm")
#plot.algo.mcl(omean, "F1", "VI", main="Match Correctness by Algorithm")
#plot.algo.mcl(omean, "VI", "K", main="Data Cleaning by Algorithm")

  
################################################################################
## Data Variant
# Resolution Fragmentation
datas = unique(omean$Group.3);
plot.data <- function(omean, x.lab, y.lab, hc=rainbow(n=length(datas), alpha=1)
                       , main, datas=unique(omean[["Group.3"]])) {
  par(mfrow=c(1,2))
  plot(omean[[x.lab]], omean[[y.lab]], xlab=x.lab, ylab=y.lab,main=main, col=gray(1))
  for(a.data.i in 1:length(datas)){
    a.data=datas[a.data.i]
    x.2 <- as.numeric(omean[omean[["Group.3"]]==a.data,][[x.lab]])
    y.2 <- as.numeric(omean[omean[["Group.3"]]==a.data,][[y.lab]])
    points(x.2,y.2,col=hc[a.data.i])
  }
  plot.new(); 
  plot.window(c(0,1), c(0,1));
  legend(0,1, datas, col=hc, bty="n", cex=0.5, pch="o")
}
plot.data.means <- function(omean, x.lab, y.lab, hc=rainbow(n=length(datas)
                                                            , alpha=1)
                            , main) {
  par(mfrow=c(1,2))
  plot(omean[[x.lab]], omean[[y.lab]], xlab=x.lab, ylab=y.lab,main=main, col=gray(1))
  tmpform=as.formula(paste(paste("~", x.lab, sep=""), y.lab, sep="+"))
  for(a.data.i in 1:length(datas)){
    a.data=datas[a.data.i]
    x.2 <- mean(as.numeric(omean[omean[["Group.3"]]==a.data,][[x.lab]]))
    y.2 <- mean(as.numeric(omean[omean[["Group.3"]]==a.data,][[y.lab]]))
    tmpframe=data.frame(as.numeric(omean[omean[["Group.3"]]==a.data,][[x.lab]]),
                       as.numeric(omean[omean[["Group.3"]]==a.data,][[y.lab]]))
    names(tmpframe)=c(x.lab,y.lab)
    pcan = prcomp(formula=tmpform,data=tmpframe)
    points(x.2,y.2,col=hc[a.data.i])
    draw.ellipse(x=pcan[["center"]][x.lab],y=pcan[["center"]][y.lab],
                 a=pcan[["sdev"]][1],b=pcan[["sdev"]][2],
                 angle=acos(pcan[["rotation"]][1,1])/pi*180)
  }
  plot.new(); 
  plot.window(c(0,1), c(0,1));
  legend(0,1, datas, col=hc, bty="n", cex=0.5, pch="o")
}
#  plot.data(omean, "K", "F1", main="Match Fragmentation by Data")
#  plot.data.means(omean, "K", "F1", main="Match Fragmentation by Data")
#  plot.data(omean, "F1", "VI", main="Match Correctness by Data")
#  plot.data.means(omean, "F1", "VI", main="Match Correctness by Data")
#  plot.data(omean, "VI", "K", main="Data Cleaning by Data")
#  plot.data.means(omean, "VI", "K", main="Data Cleaning by Data")
# #################################################################################

# ########################## AP preliminary Eval #################################
# ## F1 vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="F1"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="F1"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="F1"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="F1"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="No error")
# 
# ## K vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="K"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="K"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="K"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="Low error")
# # error none
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="K"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="No error")
# 
# ## Pr vs. Re
# # high error
# plot.algo(omean=omean.err.high, x.lab="Re", y.lab="Pr"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="Re", y.lab="Pr"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="Re", y.lab="Pr"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="Re", y.lab="Pr"
#           , algos=c(" mcl ", " merge-center "
#                     , " affinity-propagation-uniform(=1) "
#                     , " affinity-propagation-uniform(=0) "
#                     , " affinity-propagation-sparse-median "
#                     , " affinity-propagation-sparse-min "
#                     , " affinity-propagation-mean "), main="No error")
# 
# # dev.off();
# 



















# ########################## GP Eval #################################
# 
# names(gp.theta.output) <- c("ClusterNum", "PCPr", "K", "VI", "CPr", "theta", "Re", 'elapsed',
#                             'algo', 'file', "Pr")
# gp.theta.output[["F1"]] <- 2*gp.theta.output[["Pr"]]*gp.theta.output[["Re"]]/
#   (gp.theta.output[["Pr"]]+gp.theta.output[["Re"]])
# 
# 
# #Different Data sets
# error.high = gp.theta.output[gp.theta.output[['file']] %in%
#   c(" ./../data/cu1.csv ", " ./../data/cu2.csv "),];
# by.err.high = list(as.numeric(error.high[["theta"]])
#                    , as.character(error.high[["algo"]]));
# omean.err.high=aggregate(x = error.high, by =by.err.high, FUN = "mean");
# 
# error.medium = gp.theta.output[gp.theta.output[['file']] %in%
#   c(" ./../data/cu3.csv "
#     , " ./../data/cu4.csv "
#     , " ./../data/cu5.csv "
#     , " ./../data/cu6.csv "),];
# by.err.medium = list(as.numeric(error.medium[["theta"]])
#                      , as.character(error.medium[["algo"]]));
# omean.err.medium=aggregate(x = error.medium, by =by.err.medium, FUN = "mean");
# 
# error.low = gp.theta.output[gp.theta.output[['file']] %in%
#   c(" ./../data/cu7.csv "
#     , " ./../data/cu8.csv "),];
# by.err.low = list(as.numeric(error.low[["theta"]])
#                   , as.character(error.low[["algo"]]));
# omean.err.low=aggregate(x = error.low, by =by.err.low, FUN = "mean");
# 
# error.none = gp.theta.output[gp.theta.output[['file']] %in%
#   c(" ./../f1.csv "
#     , " ./../data/f2.csv "
#     , " ./../data/f3.csv "
#     , " ./../data/f4.csv "
#     , " ./../data/f5.csv "),];
# by.err.none = list(as.numeric(error.none[["theta"]])
#                    , as.character(error.none[["algo"]]));
# omean.err.none=aggregate(x = error.none, by=by.err.none, FUN = "mean");
# 
# 
# 
# 
# #Grouping data for aggragation
# by1 = list(as.numeric(gp.theta.output[["theta"]]), as.character(gp.theta.output[["algo"]])
#            , as.character(gp.theta.output[["file"]]))
# omean=aggregate(x = gp.theta.output, by = by1, FUN = "mean")
# 
# 
# ########################## GP Eval #################################
# ## F1 vs. Theta
# # high error
#                   
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="F1"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex "),
#           main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="F1"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex "),
#           main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="F1"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex "),
#           main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="F1"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex "),
#           main="No error")
# 
# ## K vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="K"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex ")
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="K"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex ")
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="K"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex ")
#           , main="Low error")
# # error none
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="K"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex ")
#           , main="No error")
# 
# ## Pr vs. Re
# # high error
# plot.algo(omean=omean.err.high, x.lab="Re", y.lab="Pr"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex ")
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="Re", y.lab="Pr"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex ")
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="Re", y.lab="Pr"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex ")
#           , main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="Re", y.lab="Pr"
#           , algos=c(" centre-baseline ", " merge-center-baseline ",
#                     " center-simple ", " merge-center-simple ",
#                     " center-complex ", " merge-center-complex ")
#           , main="No error")
# 
# # dev.off();
























# 
# ########################## Stringer Eval #################################
# ### single.pass
# single.pass.names = c(" partition ", " center ", " center-degree ",
#                       " center-sum ", " center-mean ", " center-evcent ",
#                       " center-kleinberg ", " center-markov ", " merge-center ");
# ## F1 vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="No error")
# 
# ## K vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="Low error")
# # error none
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="No error")
# 
# ## VI vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="Low error")
# # error none
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="No error")
# 
# ## Pr vs. Re
# # high error
# plot.algo(omean=omean.err.high, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="No error")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### cut.overlap
# cut.overlap.names = c(" partition " ," star-degree-overlap ", " star-evcent-overlap ",
#                       " star-kleinberg-overlap ", " star-markov-overlap ",
#                       " star-sum-overlap, star-mean-overlap ");
# single.pass.names = cut.overlap.names
# ## F1 vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="No error")
# 
# ## K vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="Low error")
# # error none
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="No error")
# 
# ## VI vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="Low error")
# # error none
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="No error")
# 
# ## Pr vs. Re
# # high error
# plot.algo(omean=omean.err.high, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="No error")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### cut.part
# cut.part.names = c(" partition ", " star-degree-non-overlap ", " star-evcent-non-overlap ",
#                    " star-kleinberg-non-overlap ", " star-markov-non-overlap ",
#                    " star-sum-non-overlap ", " star-mean-non-overlap ");
# single.pass.names = cut.part.names;
# ## F1 vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="No error")
# 
# ## K vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="Low error")
# # error none
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="No error")
# 
# ## VI vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="Low error")
# # error none
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="No error")
# 
# ## Pr vs. Re
# # high error
# plot.algo(omean=omean.err.high, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="No error")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ### prob
# prob.names=c(" partition ", " mcl ", " affinity-propagation-uniform(=0) ",
#              " affinity-propagation-uniform(=1) ",
#              " affinity-propagation-sparse-median ",
#              " affinity-propagation-sparse-min ", " affinity-propagation-mean ");
# single.pass.names = prob.names;
# ## F1 vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="F1"
#           , algos=single.pass.names,
#           main="No error")
# 
# ## K vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="Low error")
# # error none
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="K"
#           , algos=single.pass.names
#           , main="No error")
# 
# ## VI vs. Theta
# # high error
# plot.algo(omean=omean.err.high, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="Low error")
# # error none
# plot.algo(omean=omean.err.none, x.lab="theta", y.lab="VI"
#           , algos=single.pass.names
#           , main="No error")
# 
# ## Pr vs. Re
# # high error
# plot.algo(omean=omean.err.high, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="High error")
# # medium error
# plot.algo(omean=omean.err.medium, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="Medium error")
# # low error
# plot.algo(omean=omean.err.low, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="Low error")
# # no error
# plot.algo(omean=omean.err.none, x.lab="Re", y.lab="Pr"
#           , algos=single.pass.names
#           , main="No error")



################################################################################
### Scalability plotting
################################################################################
plot.graph.scale.edges <- function(omean, theta=0.1, categories=" partition ")
{
  hc=rainbow(n=length(categories), alpha=1);
  par(mfrow=c(1,2))
  omean = omean[ omean[['theta']]==theta, ];
  plot(omean[["|E|"]], omean[["elapsed"]], xlab="|E|", ylab="Runtime (s)",
       main="Algorithm Scalability", col=gray(1));
  for(a.algo.i in 1:length(categories)){
    a.algo=categories[a.algo.i];
    x.2 <- as.numeric(omean[omean[["Group.2"]]==a.algo,][["|E|"]])
    y.2 <- as.numeric(omean[omean[["Group.2"]]==a.algo,][["elapsed"]])
    points(x.2,y.2,col=hc[a.algo.i], pch=a.algo.i)
  }
  plot.new(); 
  plot.window(c(0,1), c(0,1));
  legend(0,1, categories, col=hc, bty="n", cex=0.5, pch=1:length(categories));
}

single.pass.algo = c(" center ", " merge-center ", " partition ");
cut.based.algo = c(" cc-pivot ", " cut ", " articulation-point ",
                   " star-degree-overlap ");
prob.algo = c(" mcl ", " affinity-propagation-uniform(=0) ");













###### temp
# an.output = list();
# for (i in as.character(unique(omean$Group.2))){
#   x = c(min(omean.err.low[omean.err.low$Group.2==i, ][['VI']]),
#         min(omean.err.medium[omean.err.medium$Group.2==i, ][['VI']]),
#         min(omean.err.high[omean.err.high$Group.2==i, ][['VI']]));
#   add = list(c(x,  - max(x)+min(x)));
#   names(add) = i;
#   an.output = c(an.output, add);
# }
# print(an.output);
# 
# an.output = list();
# for (i in as.character(unique(omean$Group.2))){
#   x = c(max(omean.err.low[omean.err.low$Group.2==i, ][['K']]),
#         max(omean.err.medium[omean.err.medium$Group.2==i, ][['K']]),
#         max(omean.err.high[omean.err.high$Group.2==i, ][['K']]));
#   add = list(c(x,  - max(x)+min(x)));
#   names(add) = i;
#   an.output = c(an.output, add);
# }
# print(an.output);
# 
# an.output = list();
# for (i in as.character(unique(omean$Group.2))){
#   x = c(max(omean.err.low[omean.err.low$Group.2==i, ][['F1']]),
#         max(omean.err.medium[omean.err.medium$Group.2==i, ][['F1']]),
#         max(omean.err.high[omean.err.high$Group.2==i, ][['F1']]));
#   add = list(c(x,  - max(x)+min(x)));
#   names(add) = i;
#   an.output = c(an.output, add);
# }
# print(an.output);
# 
# an.output = list();
# for (i in as.character(unique(omean$Group.2))){
#   x = c(max(omean.err.low[omean.err.low$Group.2==i, ][['PCPr']]),
#         max(omean.err.medium[omean.err.medium$Group.2==i, ][['PCPr']]),
#         max(omean.err.high[omean.err.high$Group.2==i, ][['PCPr']]));
#   add = list(c(x,  - max(x)+min(x)));
#   names(add) = i;
#   an.output = c(an.output, add);
# }
# print(an.output);
# 
# an.output = list();
# for (i in as.character(unique(omean$Group.2))){
#   x = c(
#     omean.err.low[omean.err.low$Group.2==i, ][['ClusterNum']][which.min(
#       abs(500 - omean.err.low[omean.err.low$Group.2==i, ][['ClusterNum']]))], 
#     omean.err.medium[omean.err.medium$Group.2==i, ][['ClusterNum']][which.min(
#       abs(500 - omean.err.medium[omean.err.medium$Group.2==i, ][['ClusterNum']]))],
#     omean.err.high[omean.err.high$Group.2==i, ][['ClusterNum']][which.min(
#       abs(500 - omean.err.high[omean.err.high$Group.2==i, ][['ClusterNum']]))]);
#   add = list(c(x, x[which.min(abs(500-x))] - x[which.max(abs(500-x))]));
#   names(add) = i;
#   an.output = c(an.output, add);
# }
# print(an.output);



library(ggplot2)


#Alternative type of plot for each algorithms accuracy
plot.algo.all.normalized.accuracy <- function(data, algos=" partition ", the.ranges=FALSE)
{
  #Grab data
  algos.to.plot = data[data[['algo']] %in% algos, ];
  frame.to.plot = data.frame(theta=algos.to.plot$theta,
                             Pr=algos.to.plot$Pr,
                             Re=algos.to.plot$Re,
                             F1=algos.to.plot$F1,
                             CPr=algos.to.plot$CPr,
                             PCPr=algos.to.plot$PCPr,
                             VI=algos.to.plot$VI,
                             K=algos.to.plot$K,
                             ClusterNum=algos.to.plot$ClusterNum);
  by1 = list((frame.to.plot[["theta"]]));
  frame.to.plot.mean = aggregate(x = frame.to.plot, by = by1,
                                 FUN = "mean");
  frame.to.plot.min = aggregate(x = frame.to.plot, by = by1,
                                 FUN = "min");
  frame.to.plot.max = aggregate(x = frame.to.plot, by = by1,
                                 FUN = "max");
  #Reshape data
  reshaped.data = data.frame(theta=NULL, accuracy=NULL, measure=NULL,
                             up=NULL, down=NULL);
  thetas = as.numeric(frame.to.plot.mean$theta);
  for(a.name in c("Pr", "Re", "F1", "CPr", "PCPr", "VI", "K" #,
                  #"ClusterNum"
                  ))
  {
    reshaped.data = rbind(reshaped.data,
                          data.frame(theta=thetas,
                                accuracy=as.numeric(as.character(
                                  frame.to.plot.mean[[a.name]])),
                                measure=a.name,
                                up=as.numeric(
                                  as.character(frame.to.plot.max[[a.name]])),
                                down=as.numeric(
                                  as.character(frame.to.plot.min[[a.name]])),
                                range=as.numeric(
                                  as.character(frame.to.plot.max[[a.name]])) - 
                          as.numeric(as.character(frame.to.plot.min[[a.name]]))
                                     ));
    
  }  
  #--Define axis labels:
  xlabel <- expression(theta);
  if(the.ranges) {
    ylabel <- "Accuracy Range";    
  } else {
    ylabel <- "Accuracy";        
  }
  
  if(the.ranges) {
    ggplot(data=reshaped.data, aes(x=theta,y=range,
                                   linetype=measure, shape=measure,
                                   group=measure, fill=measure)) +
      #coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))+
      #geom_smooth()+
      geom_line()+geom_point()+
      xlab(xlabel) + 
      #scale_fill_grey(start = 0, end = .9) + 
      scale_fill_brewer(palette="OrRd")+
      ylab(ylabel) + 
      theme_bw();
  } else {
    ggplot(data=reshaped.data, aes(x=theta,y=accuracy, ymin=up, ymax=down,
                                   linetype=measure, shape=measure,
                                   group=measure, fill=measure)) +
      #coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))+
      #geom_smooth()+
      geom_line()+geom_point()+
      xlab(xlabel) + 
      #scale_fill_grey(start = 0, end = .9) + 
      scale_fill_brewer(palette="OrRd")+
      ylab(ylabel) + 
      theme_bw();
  }
}



#plotting old data
# possible algo options: 
#artpt, bsr, ccl, center, cr, mcl, mergec, mincut, ocr, part, sr, star
plot.old.algo.all.normalized.accuracy <- function(data, algos=" part ")
{
  #Grab data
  algos.to.plot = data[data[['Algo']] %in% algos, ];
  frame.to.plot = data.frame(theta=algos.to.plot$theta,
                             Pr=algos.to.plot$Pr,
                             Re=algos.to.plot$Re,
                             F1=algos.to.plot$F1,
                             CPr=algos.to.plot$CPr,
                             PCPr=algos.to.plot$PCPr,
                             ClusterNum=algos.to.plot$ClusterNum);
  by1 = list((frame.to.plot[["theta"]]));
  frame.to.plot.mean = aggregate(x = frame.to.plot, by = by1,
                                 FUN = "mean");
  frame.to.plot.min = aggregate(x = frame.to.plot, by = by1,
                                FUN = "min");
  frame.to.plot.max = aggregate(x = frame.to.plot, by = by1,
                                FUN = "max");
  #Reshape data
  reshaped.data = data.frame(theta=NULL, accuracy=NULL, measure=NULL,
                             up=NULL, down=NULL);
  thetas = as.numeric(frame.to.plot.mean$theta);
  for(a.name in c("Pr", "Re", "F1", "CPr", "PCPr" #,
                  #"ClusterNum"
  ))
  {
    reshaped.data = rbind(reshaped.data,
                          data.frame(theta=thetas,
                                     accuracy=as.numeric(as.character(
                                       frame.to.plot.mean[[a.name]])),
                                     measure=a.name,
                                     up=as.numeric(
                                       as.character(frame.to.plot.max[[a.name]])),
                                     down=as.numeric(
                                       as.character(frame.to.plot.min[[a.name]]))));
    
  }  
  #--Define axis labels:
  xlabel <- expression(theta);
  ylabel <- "Accuracy";
  
  ggplot(data=reshaped.data, aes(x=theta,y=accuracy, ymin=up, ymax=down,
                                 linetype=measure, shape=measure,
                                 group=measure, fill=measure)) +
    #coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))+
    #geom_point()+
    #geom_smooth()+
    geom_line()+geom_point()+
    xlab(xlabel) + 
    ylab(ylabel) + #scale_fill_grey(start = 0, end = .9) +
    scale_fill_brewer(palette="OrRd")+
    theme_bw();
}


# Plots scalability
plot.algo.all.normalized.scalability <-  function(data,
  algos = c(" affinity-propagation-uniform(=0) ", " articulation-point ",
            " cc-pivot ", " center ", " mcl ", " merge-center ", " partition ",
            " star-degree-overlap ", " cut ", " ricochet-sr ", " ricochet-bsr ",
            " ricochet-cr ", " ricochet-ocr "))
{
  #Grab data
  algos.to.plot <- data[data[['algo']] %in% algos, ];
  algos.to.plot = algos.to.plot[
    algos.to.plot$`|E|` / choose(algos.to.plot$`|V|`, 2) < 0.01, ]
  algos.to.plot = algos.to.plot[
    algos.to.plot$`|E|` / choose(algos.to.plot$`|V|`, 2) > 0.001, ]
  temp.frame<-data.frame(algo=as.character(algos.to.plot$algo),
                        runtime=algos.to.plot$elapsed);
  
  temp.frame$algo <- ordered(temp.frame$algo);
  
  frame.to.plot.max <- aggregate(x = temp.frame, by = list(temp.frame$algo ),
                                FUN = "max");

  frame.to.plot <- data.frame(runtime=algos.to.plot$elapsed,
                              edges=algos.to.plot$`|E|` /
                                choose(algos.to.plot$`|V|`, 2),
                              algo=as.character(algos.to.plot$algo));
  frame.to.plot$algo <- ordered(frame.to.plot$algo);
  to.plot = c();
  for (item in algos) 
  {
    bool1 <-  frame.to.plot$algo == item;
    bool2 <-  frame.to.plot.max$algo == item;
    frame.to.plot[bool1, ]$runtime <- frame.to.plot[bool1, ]$runtime /
      frame.to.plot.max[bool2, ]$runtime;
    #smoothening
    the.smoothening <- loess.smooth(x=frame.to.plot[bool1, ]$edges,
                                    y=frame.to.plot[bool1, ]$runtime,
                                    evaluation=10)
    names(the.smoothening) <- c("edges", "runtime");
    if (length(to.plot)==0) {
      to.plot =  the.smoothening$edges
      to.plot = cbind(to.plot, the.smoothening$runtime)
      to.plot = cbind(to.plot, item);
    } else {
      to.plot <- rbind(to.plot, cbind(cbind(the.smoothening$edges,
                                            the.smoothening$runtime),
                                      rep(item, 10)))
    }
  }
  plot.to.plot = data.frame(to.plot)
  names(plot.to.plot) = c("edges", "runtime", "algorithm");
  plot.to.plot$edges = as.numeric(as.character(plot.to.plot$edges))
  plot.to.plot$runtime = as.numeric(as.character(plot.to.plot$runtime))
  #renaming for display
  displaynames=c("uAP [pref=0]", "uAP [pref=1]", "ArtPt",
                 "CC-PIV", "CENTER", "MCL", "MC", "PART",
                 "STAR", "CUT", "SR", "BSR",
                 "CR", "OCR")
  names(displaynames) = c(" affinity-propagation-uniform(=0) ", " affinity-propagation-uniform(=1) ", " articulation-point ",
                          " cc-pivot ", " center ", " mcl ", " merge-center ", " partition ",
                          " star-degree-overlap ", " cut ", " ricochet-sr ", " ricochet-bsr ",
                          " ricochet-cr ", " ricochet-ocr ");
  plot.to.plot$algorithm <- as.character(displaynames[as.character(plot.to.plot$algorithm)])
  #Reshape data
  #--Define axis labels:
  xlabel <- "Edge Density";
  ylabel <- "Normalized Runtime";
  ggplot(data=plot.to.plot, aes(x=edges,y=runtime,
                                linetype=algorithm, group=algorithm,
                                shape=algorithm)) +
    #coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))+
    scale_y_log10() + scale_x_log10()+
    #geom_smooth(se=FALSE, aes())+
    geom_point()+
    geom_line()+
    xlab(xlabel) + 
    ylab(ylabel) + #scale_fill_grey(start = 0, end = .9) +
    #scale_fill_brewer(palette="OrRd")+
    theme_bw() 
  #+ opts(legend.position="bottom")
  ;
}

# Plots one accuracy measure for multiple centrality measures.
plot.algo.all.normalized.centrality <-  function(data, measure='VI',
                                                  algos = c(" affinity-propagation-uniform(=0) ", " articulation-point ",
                                                            " cc-pivot ", " center ", " mcl ", " merge-center ", " partition ",
                                                            " star-degree-overlap ", " cut ", " ricochet-sr ", " ricochet-bsr ",
                                                            " ricochet-cr ", " ricochet-ocr "))
{
  #Grab data
  algos.to.plot <- data[data[['algo']] %in% algos, ];
  frame.to.plot <- data.frame(accuracy=algos.to.plot[[measure]],
                              theta=algos.to.plot$theta,
                              algo=as.character(algos.to.plot$algo));
  frame.to.plot$algo <- ordered(frame.to.plot$algo);
  by1 = list(frame.to.plot[["theta"]], frame.to.plot[["algo"]]);
  frame.to.plot.mean = aggregate(x = frame.to.plot, by = by1,
                                 FUN = "mean");
  names(frame.to.plot.mean) = c("theta", "algorithms", "accuracy", "theta.old"
                                , "algorithms.old")
  #renaming for display
  displaynames=c("uAP(pref=0)", "uAP(pref=1)", "ArtPt",
                 "CC-PIV", "CENTER", "MCL", "MC", "PART",
                 "STAR", "CUT", "SR", "BSR",
                 "CR", "OCR", "CENTER(mean)", "CENTER(sum)", "CENTER(degree)", 
                 "CENTER(markov)", "STAR(mean)", "STAR(sum)", 
                 "STAR(markov)", "STAR(mean, part.)", "STAR(sum, part.)", 
                 "STAR(degree, part.)", "STAR(markov, part.)",
                 paste(expression(theta),"AP"), "mAP",
                 paste(expression(mu),"AP"))
  names(displaynames) = c(" affinity-propagation-uniform(=0) ",
                          " affinity-propagation-uniform(=1) ", 
                          " articulation-point ", " cc-pivot ", " center ", 
                          " mcl ", " merge-center ", " partition ", 
                          " star-degree-overlap ", " cut ", " ricochet-sr ", 
                          " ricochet-bsr ", " ricochet-cr ", " ricochet-ocr ", 
                          " center-mean ", " center-sum ", " center-degree ", 
                          " center-markov ", " star-mean-overlap ", 
                          " star-sum-overlap ", " star-markov-overlap ", 
                          " star-mean-non-overlap ", " star-sum-non-overlap ",
                          " star-degree-non-overlap ", " star-markov-non-overlap ",
                          " affinity-propagation-sparse-min ",
                          " affinity-propagation-sparse-median ", 
                          " affinity-propagation-mean ");
  frame.to.plot.mean$algorithms <- as.character(displaynames[as.character(frame.to.plot.mean$algorithms)])
  #--Define axis labels:
  xlabel <- expression(theta);
  ylabel <- paste("Accuracy (", measure, ")" );
  
  ggplot(data=frame.to.plot.mean, aes(x=theta,y=accuracy,
                                 linetype=algorithms, shape=algorithms,
                                 group=algorithms, fill=algorithms)) +
    #coord_cartesian(xlim = c(0, 10), ylim = c(0, 10))+
    #scale_y_log10() + scale_x_log10()+  
    geom_line()+geom_point()+
    xlab(xlabel) + 
    ylab(ylabel) + #scale_fill_grey(start = 0, end = .9) +
    scale_fill_brewer(palette="OrRd")+
    theme_bw() 
  #+ opts(legend.position="bottom")
  ;
}