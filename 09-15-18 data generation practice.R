# Set random seed #
set.seed(1234567)

# Step 1. Generate data #

# Step 1.1. Generate school level and student level covariates # 
# number of schools = 1, ..., H, number of student per school = 1, ... ,K-h 
# Set the number of schools to be 2000 #
# Generate the number of student per school to be N(600,270), min = 30 and max = 4000
# Students per school is based on FL data  

H=2000
K=rep(NA,H)

for (h in 1:H){
  K[h]=as.integer(rnorm(H,600,270))
  if(K[h]<30) {
    K[h]=30}
  if(K[h]>4000) {
    K[h]=4000
  }
}

# Generate two school-level covariates, V1 & V2 
# V1~ is continuous N(0, 1), V2 is binary Ber(0.2)
v1=rep(NA,H)
v2=rep(NA,H)

for (h in 1:H){
  v1[h]=rnorm(1,0,1)
  v2[h]=rbinom(1,1,0.2)
}

# Generate two student level covariates X1 & X2 
# x1 is continuous N(V1_h, 1), in each school h, mean of x1 equals to v1  
# x2 is binary Ber(0.5)
# x1[[h]] = x1 of school h, same for x2[[h]] 

x1=NULL
x2=NULL
for(h in 1:H){
  x1[[h]]=rep(NA,K[h])
  x2[[h]]=rep(NA,K[h])
  x1[[h]]=rnorm(K[h],v1[h],1)
  x2[[h]]=rbinom(K[h],1,0.5)
}

# Step 1.2. Simulate potential outcomes for all students in all schools #

# u1-u5 are level 2 school level random errors, all follow N(0,1)#
# All residual covariaces are assumed to be zero #
u0=rnorm(H,0,1) 
u1=rnorm(H,0,1) 
u2=rnorm(H,0,1)
u3=rnorm(H,0,1)
u4=rnorm(H,0,1)
u5=rnorm(H,0,1)

# Below are coefficients for the MLM treatment effect # 
# All other coefficients that are in the draft but not listed here are either 0 or 1 #
pi30=1 # unconditional effect of treatment effect (TE) #
pi31=0.5 # impact of v1 on TE
pi32=0.5 # impact of v2 on TE
pi40=0.5 # impact of x1 on TE
pi41=0.5 # impact of x2 on TE
pi42=0.2 # impact of x1v1 on TE
pi50=0.2 # impact of x1v2 on TE
pi51=0.2 # impact of x2v1 on TE
pi52=0.2 # impact of x2v2 on TE

y0=NULL
y1=NULL
phi0=NULL
phi1=NULL
phi2=NULL
e=NULL
for (h in 1:H){
  y0[[h]]=rep(NA,K[h])
  y1[[h]]=rep(NA,K[h])
  e[[h]]=rep(NA,K[h])
  e[[h]]=rnorm(K[h],0,1) # student level random error follows N(0,1)
  
  phi0[h]=pi30+pi31*v1[h]+pi32*v2[h]+u3[h]
  phi1[h]=pi40+pi41*v1[h]+pi42*v2[h]+u4[h]
  phi2[h]=pi50+pi51*v1[h]+pi52*v2[h]+u5[h]
  
  for (k in 1:K[h]){
    y0[[h]][k]=v1[h]+v2[h]+u0[h]+x1[[h]][k]*(v1[h]+v2[h]+u1[h])+x2[[h]][k]*(v1[h]+v2[h]+u2[h])+e[[h]][k]
    y1[[h]][k]=y0[[h]][k]+phi0[h]+phi1[h]*x1[[h]][k]+phi2[h]*x2[[h]][k]+e[[h]][k]
  }
}

# Step 1.3.Generate selection probabilities 

# school selection probability 
p2=NULL
s2=NULL

# Set up school parameters. (p is approximately 0.25)
alpha0=-1
alpha1=2
alpha2=-2
  
for (h in 1:H){
  p2[h]=1/(1+exp(-(alpha0+alpha1*v1[h]+alpha2*v2[h])))
  s2[h]=rbinom(1,1,p2[h])
}

mean(p2)
hist(p2)
plot(v1,p2)
plot(v2,p2)


# student selection probability 
tau00=1 # unconditional selection probability 
tau01=0.5 # impact of v1 on selection
tau02=0.5 # impact of v2 on selection
tau10=0.5 # impact of x1
tau11=0.5 # impact of x1v1
tau12=0.2 # impact of x1v2
tau20=0.2 # impact of x2
tau21=0.2 # impact of x2v1
tau22=0.2 # impact of x2v2

# random effects
#omega0=rnorm(H,0,1) 
#omega1=rnorm(H,0,1) 
#omega2=rnorm(H,0,1)

eta0=eta1=eta2=NULL
p1=NULL
s1=NULL


for (h in 1:H){
  p1[[h]]=rep(NA,K[h]) #student level probability 
  s1[[h]]=rep(NA,K[h])
#  e[[h]]=rep(NA,K[h])
#  e[[h]]=rnorm(K[h],0,1) # student level random error follows N(0,1)
  
  eta0[h]=tau00+tau01*v1[h]+tau02*v2[h]
  eta1[h]=tau10+tau11*v1[h]+tau12*v2[h]
  eta2[h]=tau20+tau21*v1[h]+tau22*v2[h]
  
  for (k in 1:K[h]){
    p1[[h]][k]=1/(1+exp(-(eta0[h]+eta1[h]*x1[[h]][k]+eta2[h]*x2[[h]][k])))
    s1[[h]][k]=rbinom(1,1,p1[[h]][k])
  }
}


##########################################################
# Step 2. Saving all variables into a dataframe # 

# flatten all lists of variables 
y1_l=unlist(y1)
y0_l=unlist(y0)
x1_l=unlist(x1)
x2_l=unlist(x2)

#generate school IDs
schid=rep(seq(1:H),K)
# generate student IDs
studentid=sequence(K)

#repeat v1,v2 by # students in the school 
v1_l=rep(v1,K)
v2_l=rep(v2,k)

#combine all variables 
dataset=data.frame(cbind(schlid,studentid,y1,y0,x1,x2,v1,v2))


##########################################################


  