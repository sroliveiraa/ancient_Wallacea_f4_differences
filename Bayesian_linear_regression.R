library(rethinking)



# load the data

d=read.table("example_data.csv", sep=",", head=T)



# Bayesian linear regression model
# The model is stratified by region (East Nusa Tenggara vs. North Moluccas) and takes into account measurement error in variables A and B, which in this example correspond to the f4(Mbuti, Amis; New Guinea Highlanders, ancient Wallacean) and f4(Mbuti, test; New Guinea Highlanders, ancient Wallacean), respectively.

BLR_model=function(d){
  
  dlist <- list(
    B_obs = standardize(  d$test.f4  ),
    B_sd = d$test.stderr / sd( d$test.f4 ),
    A_obs = standardize( d$Ami.f4 ),
    A_sd = d$Ami.stderr / sd( d$Ami.f4 ),
    N = nrow(d),
    category_id = d$category_id
  )
  
  # non-centered form of model
  m <- ulam(
    alist(
      B_obs ~ dnorm( B_true , B_sd ),
      save> vector[N]:B_true <<- mu + B_true_z*sigma,
      vector[N]:B_true_z ~ dnorm(0 , 1),
      save> mu <- a[category_id] + b[category_id]*A_true[i],
      A_obs ~ dnorm( A_true , A_sd ),
      vector[N]:A_true ~ dnorm(0, 1),
      a[category_id] ~ dnorm(0, 1),
      b[category_id] ~ dnorm(0, 10),
      sigma ~ dexp(1)
    ) , data = dlist, chains = 6, iter = 4000, cores = 10 )
  return(m)
}

m = BLR_model(d)
pre = precis( m , depth=2 )
post = extract.samples(m)

B_obs = standardize( d$test.f4 )
A_obs = standardize( d$Ami.f4 )
B_true = apply( post$B_true , 2 , mean )
A_true = apply( post$A_true , 2 , mean )
Aseq = seq( from=-2, to=2, len=50 )
mu_Moluccas = sapply( Aseq , function(A) post$a[,1] + post$b[,1]*A )
mu_Tenggaras = sapply( Aseq , function(A) post$a[,2] + post$b[,2]*A )
mu_contrast = sapply( 1:length(Aseq) , function(i) mu_Tenggaras[,i] - mu_Moluccas[,i] )



# plot observed data and the unobserved true values of A and B

plot( A_obs , B_obs , xlim=c(-2,2), ylim=c(-2,2), bg=d$col , pch=d$pch, xlab="Amis (std)", ylab="test (std)" )
points( A_true , B_true , pch=d$pch)
for ( i in 1:nrow(d) ){
  lines( c( A_obs[i] , A_true[i] ) , c( B_obs[i] , B_true[i] ) )
}

# plot lines and interals of mean

lines( Aseq , apply(mu_Moluccas, 2, mean) , lwd=2 , col="#84C79A" )
shade( apply(mu_Moluccas, 2, PI, prob=0.95) , Aseq , col=col.alpha("#84C79A",0.3) )
lines( Aseq , apply(mu_Tenggaras, 2, mean) , lwd=2 , col="#FF4040" )
shade( apply(mu_Tenggaras, 2, PI, prob=0.95) , Aseq , col=col.alpha("#FF4040",0.3) )

# plot contrast between lines at each A value

plot( Aseq , apply(mu_contrast, 2, mean), ylim=c(-1.5,2.5), lwd=3 , type="l" , xlab="Amis (std)", ylab="test (NTT-NM)")
for ( p in c(0.85,0.90,0.95) ){
  shade( apply(mu_contrast, 2, PI, prob=p) , Aseq )
}
abline(h=0,lty=3)


