################################################################################
### Returns the clusters for Affinity Propagation Algorithm
################################################################################
affinity.propagation.cluster <- function(g, maxits=500, convits=10,
                                         pref=NA, q=NA, normalize=TRUE,
                                         median.default.case=TRUE) {
  preferences=NA;
  if (!is.na(pref)) {
    if (pref == "uniform") {
      if (!is.na(q)) {
        preferences=q;
      } else {
        preferences=0; # this is the default due to median similarity being 0
        # for sparse graphs in dense representation (i.e. APprelim)
      }
    } else {
      vwm = vertex.weight.mapping(g, method=pref);
      preferences = unname(as.numeric(vwm[['weight']]));
      #dealing with NANs/singletons: set to one (NOTE: solution spacific to mean)
      preferences[is.nan(preferences)] = 1;# can't be zero, breaks program.
      #attempting to keep preferences in similarity units.
      if (! pref == "mean" && normalized) {
        preferences = preferences/sum(preferences) * length(preferences);
      }
    }
  }
  cmp.apcluster = cmpfun(f=apcluster);
  apc = cmp.apcluster(s=igraph.to.Matrix(g), maxits=maxits,
  			convits=convits, p=preferences, q=q
                  , median.default.case=median.default.case);
  return(apc@clusters);
}
