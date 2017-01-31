
## The calib.f function calculates the random effects (calibrates) for plots
## where it exists height measurements
## calibration for a specific plot with 1-n measured Dbh-Height-Pairs available
## that needs not to be part of the parameterisation data base: 
calib.f <- function( ## per plot!
    ## measured heights in m
                    X
                    , model.me
                    )
{
    Z     <- matrix(cbind(1, X$centered.predictions), ncol = 2, byrow = F)#wie log(h)~...
    tZ    <- t(Z)
###
    D      <- matrix(0,ncol=2,nrow=2)
    D[1,1] <- as.double(VarCorr(model.me$lme)[1])
    D[2,2] <- as.double(VarCorr(model.me$lme)[2])
###
    R <- diag(as.numeric(VarCorr(model.me$lme)[3]),
              ncol = length(X$measured.heights),
              nrow = length(X$measured.heights)
              )
###
    ##Arne
    ##V <- Z%*%D%*%tZ+R
    ##b <- D%*%tZ%*%solve(V)%*%(log(X$measured.heights)-
    ##           predict(model.me$gam,
    ##           newdata=dat_h,type='link'))#Residuen auch auf Link-Ebene
    ##Juha:
    W <- tZ %*% solve(R) %*% Z + solve(D)
    b <- solve(W) %*% tZ %*% solve(R) %*%
        (log(X$measured.heights) - predict(model.me$gam,
                                         newdata = data.frame(
                                             h.link_fit.scam_c = X$centered.predictions
                                             ),
                                         type    = 'link')
         )#Residuen auch auf Link-Ebene
    var_b     <- solve(W)
    var_log_h <- Z %*% W %*% tZ + R
    
    random.effects <- data.frame(ranef.1 = b[1,],
                                 ranef.2 = b[2,]
                                 )
    ## THE REST SHOULD BE OUT BECAUSE IT USUES THE RANDOM EFFECTS TO PREDICT HEIGHTS
    ##
    return(random.effects)
}


