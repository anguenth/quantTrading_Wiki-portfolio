

library(xts)
library(pryr)
library(magrittr)
library(quantmod)
library(PerformanceAnalytics)


setClass("num.with.dots")
# setAs("character", "num.with.dots", 
#       function(from) as.numeric(gsub(",", "", from, fixed=TRUE) ) )
setAs("character", "num.with.dots", 
      function(from) as.numeric(gsub(",", ".", gsub(".", "", from, fixed=TRUE), fixed=TRUE)))




# Efficient Portfolio ----------------------------------------------------

eff.frontier <- function (returns, short="no", max.allocation=NULL, risk.premium.up=.5, risk.increment=.005) {
     
     # return argument should be a m x n matrix with one column per security
     # short argument is whether short-selling is allowed; default is no (short selling prohibited)
     # max.allocation is the maximum % allowed for any one security (reduces concentration)
     # risk.premium.up is the upper limit of the risk premium modeled (see for loop below)
     # risk.increment is the increment (by) value used in the for loop
     
     require(corpcor)
     require(quadprog)
     
     covariance <- make.positive.definite(cov(returns))
     print(covariance)
     n <- ncol(covariance)
     
     # Create initial Amat and bvec assuming only equality constraint (short-selling is allowed, no allocation constraints)
     Amat <- matrix (1, nrow=n)
     bvec <- 1
     meq <- 1
     
     # Then modify the Amat and bvec if short-selling is prohibited
     if(short=="no") {
          Amat <- cbind(1, diag(n))
          bvec <- c(bvec, rep(0, n))
     }
     
     # And modify Amat and bvec if a max allocation (concentration) is specified
     if(!is.null(max.allocation)) {
          if(max.allocation > 1 | max.allocation <0){
               stop("max.allocation must be greater than 0 and less than 1")
          }
          if(max.allocation * n < 1) {
               stop("Need to set max.allocation higher; not enough assets to add to 1")
          }
          Amat <- cbind(Amat, -diag(n))
          bvec <- c(bvec, rep(-max.allocation, n))
     }
     
     # Calculate the number of loops based on how high to vary the risk premium and by what increment
     loops <- risk.premium.up / risk.increment + 1
     loop <- 1
     
     # Initialize a matrix to contain allocation and statistics
     # This is not necessary, but speeds up processing and uses less memory
     eff <- matrix(nrow=loops, ncol=n+3)
     # Now I need to give the matrix column names
     colnames(eff) <- c(colnames(returns), "Std.Dev", "Exp.Return", "sharpe")
     
     # Loop through the quadratic program solver
     for (i in seq(from=0, to=risk.premium.up, by=risk.increment))  {
          dvec <- colMeans(returns) * i # This moves the solution up along the efficient frontier
          sol <- solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
          eff[loop,"Std.Dev"] <- sqrt(sum(sol$solution *colSums((covariance * sol$solution))))
          eff[loop, "Exp.Return"] <- as.numeric(sol$solution %*% colMeans(returns))
          eff[loop, "sharpe"] <- eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
          eff[loop, 1:n] <- sol$solution
          loop <- loop+1
     }
     
     return(as.data.frame(eff))
}


# Max Drawdown Portfolio --------------------------------------------------

#modified function FRAPO::PMaxDD
portfolio.MaxDD <- function (R, MaxDD = 0.1, softBudget = FALSE, ...)  {
     
     if (is.null(dim(R))) {
          stop("Argument for 'R' must be rectangular.\\n")
     }
     if (any(is.na(R))) {
          stop("NA-values contained in object for 'R'.\\n")
     }
     if (MaxDD <= 0 || MaxDD >= 1) {
          stop("Argument for 'MaxDD' must be in the interval (0, 1).\\n")
     }
     call <- match.call()
     RC <- as.matrix(cumsum(R))
     rownames(RC) <- NULL
     N <- ncol(RC)
     J <- nrow(RC)
     w <- rep(0, N)
     u <- rep(0, J) 
     x <- c(w, u)
     obj <- c(as.numeric(RC[J, ]), rep(0, J))
     a1 <- cbind(diag(N), matrix(0, nrow = N, ncol = J))
     d1 <- rep(">=", N)
     b1 <- rep(0, N)
     a2 <- c(rep(1, N), rep(0, J))
     ifelse(softBudget, d2 <- "<=", d2 <- "==")
     b2 <- 1
     a3 <- cbind(-1 * RC, diag(J))
     d3 <- rep("<=", J)
     b3 <- rep(MaxDD, J)
     a4 <- a3
     d4 <- rep(">=", J)
     b4 <- rep(0, J)
     D1 <- -1 * diag(J)
     udiag <- embed(1:J, 2)[, c(2, 1)]
     D1[udiag] <- 1
     a5 <- cbind(matrix(0, ncol = N, nrow = J), D1)
     a5 <- a5[-J, ]
     d5 <- rep(">=", J - 1)
     b5 <- rep(0, J - 1)
     a6 <- c(rep(0, N), 1, rep(0, J - 1))
     d6 <- "=="
     b6 <- 0
     Amat <- rbind(a1, a2, a3, a4, a5, a6)
     Dvec <- c(d1, d2, d3, d4, d5, d6)
     Bvec <- c(b1, b2, b3, b4, b5, b6)
     opt <- Rglpk::Rglpk_solve_LP(obj = obj, mat = Amat, dir = Dvec, 
                           rhs = Bvec, max = TRUE)
     if (opt$status != 0) {
          warning(paste("GLPK had exit status:", opt$status))
     }
     weights <- round(opt$solution[1:N], 2)
     names(weights) <- colnames(R)
     # equity <- matrix(apply(RC, 1, function(x) sum(x * weights)), 
     #                  ncol = 1)
     # rownames(equity) <- rownames(R)
     # uvals <- opt$solution[(N + 1):(N + J)]
     # dd <- as.xts(uvals - equity)
     # colnames(dd) <- "DrawDowns"
     # obj <- new("PortMdd", weights = weights, opt = opt, type = "maximum draw-down", 
     #            call = call, MaxDD = max(dd), DrawDown = dd)
     # return(obj)
     return(weights)
}

