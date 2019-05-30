Qfn <- function(X, S, N, sigma, beta0, ntraps){
    
  beta1 <- -1*( 1/(sigma^2))
  dmat <- e2dist(X,S)^2
    
  lammat <- exp(beta0 + beta1 * dmat)                    # capts per trap | s
  lamvec <- exp(beta0 + beta1 * dmat[1:length(dmat)])    # c(lammat)
  lamJ <- as.vector(t(lammat)%*%rep(1,nrow(X)))          # sum of pr(c) across all traps | s
  pbar <- as.vector( 1-exp(-t(lammat)%*%rep(1,nrow(X)))) #
  pbar <- mean(pbar)
    
  M1  <- rep(1,ntraps*nrow(S))          # DM[,1]: the intercept
  M2  <- dmat[1:length(dmat)]           # DM[,2]: X (in Y = b_0 + b_1*X)
  I11 <- (1/nrow(S))*sum(lamvec)        # Y
  I12 <- (1/nrow(S))*sum(lamvec*M2)     # YX         | Y  | YX  |
  I21 <- (1/nrow(S))*sum(lamvec*M2)     # YX         | YX | YXX |
  I22 <- (1/nrow(S))*sum(lamvec*M2*M2)  # YXX
  I <- matrix(c(I11,I12,I21,I22),nrow=2,byrow=TRUE)
  I <- N*pbar*I
  V <- solve(I)
  Q1 <- sum(diag(V))
  sumsJ <- as.vector(
           t(lammat*lammat*(diag(V)[1]+(dmat^2)*diag(V)[2]-2*dmat*V[1,2])) %*%
           rep(1,nrow(X)))
  var.pbar <- ((1/nrow(S))^2)*sum(exp(-lamJ)*exp(-lamJ)*sumsJ) # Q4
  part1 <- (N*N*var.pbar)  ### /(pbar*pbar)
  part2 <-  N*(1-pbar)/pbar
  total <-  part1+part2
  newpart2 <- N*(1-pbar)*(var.pbar+1)/pbar
  old <- N*N*var.pbar + newpart2
  fixed <- N*pbar*((1-pbar) + N*pbar)*(var.pbar/(pbar^4))  #Q2
  Q1 <- part1
  Q2 <- newpart2
  Q3 <- total
  Q4 <- Q1
  Q5 <- fixed
  Q6 <- 1 - pbar
  Q7 <- var.pbar
  c(Q1,Q2,Q3,Q4,Q5,Q6,Q7)
}
  