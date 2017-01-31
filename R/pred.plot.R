
## Given calculated random effects it calcualtes heights.
## This needs to be applied plot by plot
pred.plot.f <- function(X, model.me){
    D      <- matrix(0, ncol=2, nrow=2)
    D[1,1] <- as.double(VarCorr(model.me$lme)[1])
    D[2,2] <- as.double(VarCorr(model.me$lme)[2])
    
    ## unkorrigierte log.h schÃ¦tzen:
    log.fit.h <-
        (     fixef(model.me$lme)[1] + X$ranef.1 ) +
            ( fixef(model.me$lme)[2] + X$ranef.2 ) * X$centered.predictions
    
####Biaskorrektur (alle BÃ¦ume nicht nur HÃ¸henmessbÃ¦ume verwenden):
    Z  <- matrix(cbind(1, X$centered.predictions), ncol = 2, byrow = F)
    tZ <- t(Z)
    R  <- diag(as.numeric(VarCorr(model.me$lme)[3]),
               ncol = length(X$centered.predictions),
               nrow = length(X$centered.predictions))
    W <- tZ %*% solve(R) %*% Z + solve(D)
    var.log.h <- Z %*% solve(W) %*% tZ + R
    fit.h     <- exp(log.fit.h + 0.5*diag(var.log.h))
#####################################
    
#predicting tree heights using ranefs from the calibration 
    calib_gran$mixed.h<-exp( fixef(model.me$lme)[1] + calib_gran$Intercept +
                            (fixef(model.me$lme)[2] + calib_gran$Slope      ) *
                            calib_gran$h.link_fit.scam_c
                            )
    
    return(fit.h)
}
    
