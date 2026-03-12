## Replication file: Liu, Wang, Xu (2022)
# Aim: Generate simulated sample (DID & staggered adoption)

simulateData<-function(
    N, 
    TT, 
    r, # number of factors
    tr.threshold = 0.5, # e.g. tr.star>=0.5, D = 1, length(tr.threshold) = ntr - 1
    tr.start = 10, #length(tr.start) = ntr-1
    p, # number of covariates
    beta=c(1,3),
    force=0, # additive fixed effects, 1 = unit, 2 = time, 3 = 
    mu=0, # grand mean
    Rtype = "n", ## uniform (u)or normal (n)
    Ftype = "drift", ## factor type: white noise, ar1, drift
    Fsize = 1, ## influence of the factor
    eff.size = 1,
    eff.noise = 1,
    tr.noise = 0.3,
    factor = NULL,
    seed=NULL
    ) {

    # panel reshape: T*N -> TN * 1

    if (is.null(seed)==FALSE) {set.seed(seed)}

    #############################
    ## Data generating process
    #############################

    # number of treatment status
    ntr <- length(tr.threshold) + 1


    if (r > 0) {
         # loadings
        if (Rtype %in% c("u","uniform")) {
            bound <- sqrt(3)
            lambda<-matrix(runif(N*r, min = 0, max = 2*bound), N, r, byrow=TRUE)
        } else {
            lambda<-matrix(rnorm(N*r, 0.5, 1), N, r, byrow=TRUE)
        }      

        # factors
        if (is.null(factor)==TRUE) {
            factor<-matrix(NA,TT,r)   
            if (length(Ftype)==1) {Ftype <- rep(Ftype, r)}
            for (i in 1:r) {
                type <- Ftype[i]
                if (type == "ar1") {
                    ts <- arima.sim(list(order = c(1,0,0), ar = 0.5), n = TT)                
                } else if (type == "drift") {
                    ts <- (arima.sim(list(order = c(1,0,0), ar = 0.5), n = TT)
                        + 0.1 * seq(from=5, length.out = TT, by=1))
                } else {
                    ts <- rnorm(TT)
                }
                factor[,i] <- ts/sd(ts)
            }  
        } 
        if (length(Fsize)==1)  {
            Fsize <- rep(Fsize,r)
        }
        for (i in 1:ncol(factor)) {
            factor[,i] <- factor[,i]*Fsize[i]
        }       
    }
   


    ## covariates
    if (p>0) {
        X<-array(0,dim=c(TT,N,p))    # regressor matrix, must be T by N by p
        X <- array(rnorm(TT*N*p,sd=1),dim=c(TT,N,p))  # TT*N*p    
        beta <- matrix(beta, p, 1)
    } 

    # fixed effects
    if (force==1|force==3) { ## unit fixed effect
        alpha <- matrix(rep(rnorm(N,sd=1),each=TT),TT,N)
    }
    if (force==2|force==3) { ## time fixed effect
        drift <- arima.sim(list(order = c(1,0,0), ar = 0.5), n = TT) # + 0.5*seq(from=1, length.out = TT, by=0.5)
        xi <- matrix(rep(drift,N),TT,N)
    }   

    ## treatment assignment
    ps.raw <- rep(0, N)
    if (r > 0) {
        if (ncol(factor) == 1) {
           ps.raw <- lambda[,1] + rnorm(N, 0, tr.noise)
        } else {
           ps.raw <- lambda[,1] + 0.3*lambda[,2] + rnorm(N, 0, tr.noise) 
        }
    }
    
    if (force==1|force==3) {
        ps.raw <- ps.raw + 0.3* alpha[1,]
    }
    tr.star <- (ps.raw - min(ps.raw))/(max(ps.raw)-min(ps.raw)) # propensity score
    tr.rank <- rank(tr.star)
    tr.rank <- (tr.rank-1)/(max(tr.rank)-1) # bounded between from 0 to 1
    
    D <- matrix(0, TT, N)
    treat <- rep(0, N) 
    T0 <- rep(TT, N)
    for (i in 1:(ntr-1)) {
        tr.id <- which(tr.rank>=tr.threshold[i])
        treat[tr.id] <- i             
        T0[tr.id] <- tr.start[i]-1
        for (j in tr.id) {
            D[tr.start[i]:TT,j] <- 1  
        }
    }  
    
    ## disturbances
    if (r>0) {
      e <- matrix(rnorm(TT*N,sd = sqrt(2^2 - sum(Fsize^2))),TT,N)
    } else {
      e <- matrix(rnorm(TT*N,sd=2),TT,N)
    }
    # 

    
    ## outcome variable
    Y <- matrix(mu, TT, N)
    
    if (r>0) {
        Y <- Y + factor%*%t(lambda)
    }
    if (p>0) {
        for (k in 1:p) {
            Y<-Y+X[,,k]*beta[k]
        }
    }
    
    # fixed effects
    if (force==1|force==3) { ## unit fixed effect
        Y <- Y +3*alpha
    }
    if (force==2|force==3) { ## time fixed effect
        Y <- Y + xi
    }  
     

    ## treatment effect
    eff <- matrix(0,TT,N)
    for (i in 1:N){
        eff[, i] <- rnorm(TT, mean = cumsum(D[, i])/TT * eff.size, sd = eff.noise)
    }
    eff <- eff*D
    Y <- Y + eff

    ## disturbance
    Y <- Y + e


    ## panel structure
    panel<-as.data.frame(cbind(
        rep(101:(100+N),each=TT),
        rep(1:TT,N),
        c(Y),
        c(e),
        c(eff),
        rep(T0, each = TT)))
    cname<-c("id","time","Y","error","eff","T0")

    ## treatment indicator
    panel <- cbind(panel,c(D))
    cname <- c(cname,"D")

    ## covar
    if (p>0) {
        for (i in 1:p) {
            panel<-cbind(panel,c(X[,,i]))
            cname<-c(cname,paste("X",i,sep=""))
        }
    }    

    ## additive fixed effects
    if (force==1|force==3) {
        panel <- cbind(panel,c(alpha))
        cname <- c(cname,"alpha")
    }
    if (force==2|force==3) {
        panel <- cbind(panel,c(xi))
        cname <- c(cname,"xi")
    }
    
    if (r>0) {
        for (i in 1:r) {
            panel<-cbind(panel,rep(factor[,i],N))
            cname<-c(cname,paste("F",i,sep=""))
        }
        for (i in 1:r) {
            panel<-cbind(panel,rep(lambda[,i],each=TT))
            cname<-c(cname,paste("L",i,sep=""))
        }
    }
    colnames(panel)<-cname

    if (r>0) for (i in 1:r) {
        panel[,paste("FL",i,sep="")]<-panel[,paste("F",i,sep="")]*panel[,paste("L",i,sep="")]
    }

    ## return(list(panel=panel,lambda=lambda,lambda=lambda,factor=factor))
    return(panel)
}


