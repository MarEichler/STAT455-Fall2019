##########################
# Categorical HW2 part b #
##########################


rm(list = ls(all = TRUE))


calculate_ncp <- function(row)
{
    p <- row[2:4]
    p_0 <- rep(1/3, 3)
    n <- row[1]
    
    lambda_U <- n * sum((p - p_0)^2 / p_0)
    lambda_R <- n * ((p[1] + p[2] - (2/3))^2 / (2/9))

    lambda <- c(lambda_R, lambda_U)  
    return(lambda)
}


calculate_S <- function(y, n)
{
    p_0 <- rep(1/3, 3)
    
    S_sq_U <- colSums((y - n*p_0)^2 / (n*p_0))
    S_sq_R <- (colSums(y[1:2,]) - (2*n/3))^2 / (2*n/9)
    
    S_sq <- cbind(S_sq_R, S_sq_U)
    
    return(S_sq)
}


runMCMC <- function(row)
{
    reps <- 10^6
    
    p <- row[2:4]
    n <- row[1]
    
    y_reps <- rmultinom(reps, n, p)
    
    return(y_reps)
}


doProblem <- function(row)
{
    ret_vec <- rep(0, 4)
    
    lambda <- calculate_ncp(row)
    lambda_R <- lambda[1]
    lambda_U <- lambda[2]
    
    ret_vec[2] <- pchisq(q = 3.8415, df = 1, ncp = lambda_R, lower.tail = FALSE)
    ret_vec[4] <- pchisq(q = 5.9915, df = 2, ncp = lambda_U, lower.tail = FALSE)
    
    y_reps <- runMCMC(row)
    S_sq <- calculate_S(y_reps, row[1])
    S_sq_R <- S_sq[,1]
    S_sq_U <- S_sq[,2]
    
    ret_vec[1] <- mean(S_sq_R >= 3.8415)
    ret_vec[3] <- mean(S_sq_U >= 5.9915)
    
    return(ret_vec)
}



problem <- data.frame(c(rep(75, 4), rep(250, 5)), 
                      c(1/3, 1/4, 1/6, .2, 1/3, .3, .22, .25, .22),
                      c(1/3, 1/4, 3/6, .3, 1/3, .3, .4467, .3, .4),
                      c(1/3, 2/4, 2/6, .5, 1/3, .4, .3333, .45, .38),
                      rep(0, 9), rep(0, 9), rep(0, 9), rep(0, 9))
names(problem) <- c("n", "pT_1", "pT_2", "pT_3", "PR", "aPR", "PU", "aPU")

ret_mat <- apply(problem, 1, doProblem)
problem[,5:8] <- t(ret_mat)
