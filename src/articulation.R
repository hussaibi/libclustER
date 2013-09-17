################################################################################
### Articulation point clustering algorithm.
################################################################################
articulation.point.clustering<-function(graph){
  return(biconnected.components(graph)$components)
}
