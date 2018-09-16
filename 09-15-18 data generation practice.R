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

# Below are coefficients for the MLM # 
pi30=1 # unconditional treatment effect (TE) for every student #
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


# Step 2. Saving all variables into a dataframe # 
# unlist y1,y0,x1,x2





  