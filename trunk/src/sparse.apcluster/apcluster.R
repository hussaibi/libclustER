apcluster <- function(s, p=NA, q=NA, maxits=1000, convits=100,
                      lam=0.9, details=FALSE, nonoise=TRUE, seed=NA
                      , median.default.case=TRUE)
{
    if (!is.na(seed)) set.seed(seed)

    apresultObj <- new("APResult") # create the result object to be returned

    #
    # check input data
    #

    if (!is.na(p) && (!is.numeric(p) || !is.vector(p)))
        stop("p must be a number or vector")

    if (length(dim(s)) != 2 || ncol(s) != nrow(s))
        stop("s must be a square Matrix")

    N <- nrow(s)

    if (length(p) > 1)
    {
      if (length(p) < N)
            stop("vector p is shorter than number of samples")
        else if (length(p) > N){
            p <- p[1:N] # truncate unnecessarily long p
        }
    }

    if (lam > 0.9)
    {
        warning("Large damping factor in use. Turn on details\n",
                "and call plot() to monitor net similarity. The\n",
                "algorithm will change decisions slowly, so consider using\n",
                "a larger value of convits.")
    }

    # If argument p is not given, p is set to median of s
    if (is.na(p))
    {
        if (is.na(q))
        {
            if (median.default.case){
              p <- median(s[setdiff(which(s > 0), 0:(N-1) * N + 1:N)])
            } else {
              p <- min(s[setdiff(which(s > 0), 0:(N-1) * N + 1:N)])
            }
            # this use to be s > -Inf. Doesn't make sene for sparse graphs,
            # with similarity in [0,1]
        }
        else
        {
            p <- quantile(s[setdiff(which(s > 0), 0:(N-1) * N + 1:N)], q)
            # this use to be s > -Inf. Doesn't make sene for sparse graphs,
            # with similarity in [0,1]
        }
    }

    apresultObj@l <- N
    #print("N set.");

    # In case user did not remove degeneracies from the input similarities,
    # avoid degenerate solutions by adding a small amount of noise to the
    # input similarities
    if (!nonoise)
    {
        randomMat <- Matrix(rnorm(N*N),N)

        s <- s + (.Machine$double.eps * s + .Machine$double.xmin * 100) *
                 randomMat
    }

    attributes(p) <- NULL

    # Place preferences on the diagonal of s (recycled if p is scalar)
    diag(s) <- p
    # store p into result object for future reference
    apresultObj@p <- p

    # Numerical stability -- replace -Inf with -realmax
    infelem <- which(s < -.Machine$double.xmax)

    if (length(infelem) > 0)
    {
        warning("-Inf similarities detected: changing to -realmax ",
                " to ensure numerical stability")

        s[infelem] <- -.Machine$double.xmax
    }

    infelem <- which(s > .Machine$double.xmax)

    if (length(infelem) > 0)
        stop("+Inf similarities detected: change to a large positive value,",
             " but smaller than realmax")

    # create temporary storage
    e   <- Matrix(0, N, convits)
    dn  <- FALSE
    i   <- 0
    dS  <- diag(s)
    A   <- Matrix(0, N, N)
    R   <- Matrix(0, N, N)
    AS  <- Matrix(0, N, N)
    Rp  <- Matrix(0, N, N)
    old <- Matrix(0, N, N)
    t   <- 1

    if (details)
    {
        apresultObj@idxAll    <- Matrix(0, N, maxits + 1)
        apresultObj@netsimAll <- seq(0, 0, length.out=(maxits + 1))
        apresultObj@dpsimAll  <- seq(0, 0, length.out=(maxits + 1))
        apresultObj@exprefAll <- seq(0, 0, length.out=(maxits + 1))
    }

    # main loop

    #print("Entering loop.");
    while (!dn)
    {
        i <- i + 1;

        # print(i);
        # Compute responsibilities
        old <- R

        AS <- A + s
        I  <- apply(AS, 1, which.max)
        Y  <- AS[1:N + (I - 1) * N]
        AS[1:N + (I - 1) * N] <- -Inf

        Y2  <- apply(AS, 1, max)

        R <- s - Y
        R[1:N + (I - 1) * N] <- s[1:N + (I - 1) * N] - Y2

        R <- (1 - lam) * R + lam * old

        R[R > .Machine$double.xmax] <- .Machine$double.xmax

        # Compute availabilities
        old <- A
        Rp <- (R + abs(R)) / 2

        diag(Rp) <- diag(R)
        auxsum <- colSums(Rp)

        A <- t(auxsum - t(Rp)) # auxsum - Rp columnwise
        dA <- diag(A)

        A <- (A - abs(A)) / 2
        diag(A) <- dA

        A <- (1 - lam) * A + lam * old

        # determine clusters and check for convergence
        E <- as.numeric((diag(A) + diag(R)) > 0)
        e[,((i - 1) %% convits) + 1] <- E
        K <- sum(E)

        if (i >= convits || i >= maxits)
        {
            se <- rowSums(e)

            unconverged <- (sum((se == convits) + (se == 0)) != N)

            if ((!unconverged && (K > 0)) || (i == maxits))
            {
                dn <- TRUE
            }
        }

        if (K==0)
        {
            #tmpnetsim <- NaN
            #tmpdpsim <- NaN
            tmpexpref <- NaN
            tmpidx <- NaN
        }
        else
        {
            I <- which(E != 0)
            notI <- which(E == 0)

            c         <- max.col(s[,I])
            c[I]      <- 1:K
            tmpidx    <- I[c]
            ##tmpdpsim  <- sum(s[sub2ind(N, notI, tmpidx[notI])])
            #Error in s[sub2ind(N, notI, tmpidx[notI])] : 
            #error in evaluating the argument 'i' in selecting a method for function '['

            # hussaibi: just realized all tmp--- attributes aren't used for anything -_-;;
            #           Thus, commenting out tmpdpsim and tmpnetsim.
            tmpexpref <- sum(dS[I])
            #tmpnetsim <- tmpdpsim + tmpexpref
        }

        if (details)
        {
            #apresultObj@netsimAll[i] <- tmpnetsim
            #apresultObj@dpsimAll[i]  <- tmpdpsim
            apresultObj@exprefAll[i] <- tmpexpref
            apresultObj@idxAll[,i]   <- tmpidx
        }
    } # iterations
    #print("after iterations");
    I <- which((diag(A) + diag(R)) > 0)
    K <- length(I) # Identify exemplars
    #print("after examplars grabbed");
    if (K > 0)
    {
        c <- max.col(s[,I])
        #print("before return on apcluster");
        c[I] <- 1:K # Identify clusters
        c[is.na(c)] <- 0 # R inserts NAs by default, so replace them with 0s
                         # to get same result as the Matlab code
        #print("before refining clusters");
        # Refine the final set of exemplars and clusters and return results
        for (k in 1:K)
        {
            ii <- which(c == k)
            j <- which.max(colSums(s[ii,ii,drop=FALSE]))
            I[k] <- ii[j[1]]
        }
        #print("after refining clusters");
        notI <- Matrix(sort(setdiff(1:N, I)), ncol=1)
        c <- max.col(s[,I])
        c[I] <- 1:K
        tmpidx <- I[c]
        #tmpdpsim <- sum(s[sub2ind(N, notI, tmpidx[notI])])
        tmpexpref <- sum(dS[I])
        #tmpnetsim <- tmpdpsim + tmpexpref

        apresultObj@exemplars <- as.numeric(levels(factor(tmpidx)))

        apresultObj@clusters <- list()
        #print("before populating clusters");
        for (c in 1:length(apresultObj@exemplars))
        {
            apresultObj@clusters[[c]] <- which(tmpidx ==
                                               apresultObj@exemplars[c])
        }
        #print("after populating clusters");

        if (length(colnames(s)) == N)
        {
            names(apresultObj@exemplars) <- colnames(s)[apresultObj@exemplars]

            for (c in 1:length(apresultObj@exemplars))
            {
                names(apresultObj@clusters[[c]]) <-
                    colnames(s)[apresultObj@clusters[[c]]]
            }
        }
        #print("after populating names");
    }
    else
    {
        tmpidx    <- Matrix(NaN, N, 1)
        #tmpnetsim <- NaN
        tmpexpref <- NaN

        apresultObj@exemplars <- c()
        apresultObj@clusters  <- list()
    }

    #apresultObj@netsim <- tmpnetsim
    #apresultObj@dpsim  <- tmpdpsim
    apresultObj@expref <- tmpexpref
    apresultObj@idx    <- tmpidx
    apresultObj@it     <- i

    if (details)
    {
        #apresultObj@netsimAll[i+1] <- tmpnetsim
        #apresultObj@dpsimAll[i+1]  <- tmpdpsim
        apresultObj@exprefAll[i+1] <- tmpexpref
        apresultObj@idxAll[,i+1]   <- tmpidx

        apresultObj@idxAll    <- apresultObj@idxAll[,1:i+1]
        #apresultObj@netsimAll <- apresultObj@netsimAll[1:i+1]
        #apresultObj@dpsimAll  <- apresultObj@dpsimAll[1:i+1]
        apresultObj@exprefAll <- apresultObj@exprefAll[1:i+1]
    }

    if (unconverged)
        warning("Algorithm did not converge. Turn on details\n",
                "and call plot() to monitor net similarity. Consider\n",
                "increasing maxits and convits, and, if oscillations occur\n",
                "also increasing damping factor lam.")
    #print("before return on apcluster");
    apresultObj
}


# Linear index from multiple subscripts.
#   sub2ind is used to determine the equivalent single index
#   corresponding to a given set of subscript values.
sub2ind <- function(N, I, J)
{
    I + (N * (J - 1))
}
