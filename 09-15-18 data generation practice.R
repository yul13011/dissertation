# This is the practice coding for dissertation simulation study #
# Note all conditions are estimators are programmed # 

# Set random seed #
set.seed(1234567)

# Step 1. Generate data #################################################################

# Step 1.1. Generate school level and student level covariates # 
# number of schools = 1, ..., H, number of student per school = 1, ... ,K-h 
# Set the number of schools to be 2000 #
# Generate the number of student per school to be N(600,270), min = 30 and max = 4000
# Students per school is based on FL data  

# Generate two school-level covariates, V1 & V2 
# V1~ is continuous N(0, 1), V2 is binary Ber(0.2)

# Generate two student level covariates X1 & X2 
# x1 is continuous N(V1_h, 1), in each school h, mean of x1 equals to v1  
# x2 is binary Ber(0.5)
# x1[[h]] = x1 of school h, same for x2[[h]] 

H=2000
K=rep(NA,H)
v1=rep(NA,H)
v2=rep(NA,H)
x1=NULL
x2=NULL

for (h in 1:H){
  K[h]=as.integer(rnorm(H,600,270)) 
  # k= # number of student per school, k~N(600, 270), min = 30 and max = 4000
  if(K[h]<30) {
    K[h]=30}
  if(K[h]>4000) {
    K[h]=4000
  }
  v1[h]=rnorm(1,0,1) # v1~N(0,1)
  v2[h]=rbinom(1,1,0.2) # v2 is binary with p=0.2
  x1[[h]]=rep(NA,K[h]) # student level variable x1
  x2[[h]]=rep(NA,K[h]) # student level variable x2
  x1[[h]]=rnorm(K[h],v1[h],1) # x1~N(v1,1), so v1 is the mean of x1
  x2[[h]]=rbinom(K[h],1,0.5) # x2 is binary with p = 0.5
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

y0=NULL #potential outcome for control units
y1=NULL #potential outcome for treated units
TE=NULL #true treatment effect 
phi0=NULL
phi1=NULL
phi2=NULL
e=NULL # student level random error

for (h in 1:H){
  y0[[h]]=rep(NA,K[h])
  y1[[h]]=rep(NA,K[h])
  TE[[h]]=rep(NA,K[h])
  e[[h]]=rep(NA,K[h])
  e[[h]]=rnorm(K[h],0,1) # student level random error follows N(0,1)
  
  phi0[h]=pi30+pi31*v1[h]+pi32*v2[h]+u3[h] # level 2 intercept as outcome of v1 & v2
  phi1[h]=pi40+pi41*v1[h]+pi42*v2[h]+u4[h] # level 2 slope as outcome of v1 & v2
  phi2[h]=pi50+pi51*v1[h]+pi52*v2[h]+u5[h] # level 2 slope as outcome of v1 & v2
  
  for (k in 1:K[h]){
    y0[[h]][k]=v1[h]+v2[h]+u0[h]+x1[[h]][k]*(v1[h]+v2[h]+u1[h])+x2[[h]][k]*(v1[h]+v2[h]+u2[h])+e[[h]][k] #potential outcome for treatment
    y1[[h]][k]=y0[[h]][k]+phi0[h]+phi1[h]*x1[[h]][k]+phi2[h]*x2[[h]][k]+e[[h]][k] # potential outcome for control 
    TE[[h]][k]=phi0[h]+phi1[h]*x1[[h]][k]+phi2[h]*x2[[h]][k] # true individual treatment effect 
  }
}

# Step 1.3.Generate selection probabilities  

# Generate school selection probability and assign treatment groups 
# Set up school parameters. (p is approximately 0.2)
alpha0=-2
alpha1= 2
alpha2=-2

p2=NULL # school level selection probability
s2=NULL  # s2=1 indicates school selected into sample 
z2=NULL # z2=1 indicates a sampled school was assigned the treatment condition

for (h in 1:H){
  p2[h]=1/(1+exp(-(alpha0+alpha1*v1[h]+alpha2*v2[h]))) # model for school selection 
  s2[h]=rbinom(1,1,p2[h]) # selection follows a bernoulli distribution 
  if(s2[h]==1) z2[h]=rbinom(1,1,0.5) else z2[h]=NA  # assign schools to treatment or control group #
}

mean(p2)
plot(v1,p2)
plot(v2,p2)

# Generate student selection probability 
# *** This model assumes that every student in the population has their own selection probability, which
# *** cannot be verified with the data we have 

# Below are coefficiences for the ML logistics regression for selection
 
tau00=-2 # unconditional selection probability 
tau01=1 # impact of v1 on selection
tau02=1 # impact of v2 on selection
tau10=1 # impact of x1
tau11=0.5 # impact of x1v1
tau12=0.5 # impact of x1v2
tau20=1 # impact of x2
tau21=0.5 # impact of x2v1
tau22=0.5 # impact of x2v2

# random effects
# omega0=rnorm(H,0,1) 
# omega1=rnorm(H,0,1) 
# omega2=rnorm(H,0,1)

eta0=eta1=eta2=NULL
p1=NULL
s1=NULL

p1_mean=NULL 
s1_mean=NULL

for (h in 1:H){
  p1[[h]]=rep(NA,K[h]) # student level probability 
  s1[[h]]=rep(NA,K[h]) # Indicator for student selection
#  e[[h]]=rep(NA,K[h])
#  e[[h]]=rnorm(K[h],0,1) # student level random error follows N(0,1)
  
  eta0[h]=tau00+tau01*v1[h]+tau02*v2[h] # level 2 intercept as outcome of v1 & v2
  eta1[h]=tau10+tau11*v1[h]+tau12*v2[h] # level 2 slope as outcome of v1 & v2 
  eta2[h]=tau20+tau21*v1[h]+tau22*v2[h] # level 2 slope as outcome of v1 & v2
  
  for (k in 1:K[h]){
    p1[[h]][k]=1/(1+exp(-(eta0[h]+eta1[h]*x1[[h]][k]+eta2[h]*x2[[h]][k]))) # model for student selection 
    s1[[h]][k]=rbinom(1,1,p1[[h]][k]) # student selection follows a bernoulli distribution
  }
  s1_mean[h]=mean(s1[[h]]) # % student selected into sample for school h 
}
boxplot(s1_mean)
hist(s1_mean)

# Step 2. Saving all variables into a dataframe ########################################################## 

#generate school IDs
schid=rep(seq(1:H),K)
# generate student IDs
studentid=sequence(K)

# flatten all "lists" of variables to make them single variables (this set data to the "long" format) 
y1_l=unlist(y1)
y0_l=unlist(y0)
x1_l=unlist(x1)
x2_l=unlist(x2)
s1_l=unlist(s1)

#repeat school-level variables v1,v2,s2,z2 by # students in the school, so that each student has corresponding school level variables  
v1_l=rep(v1,K)
v2_l=rep(v2,K)
s2_l=rep(s2,K)
z2_l=rep(s2,K)

# generate outcome variable y for students in the sample schools (s2=1)
# y=y1 for treatment schools
# y=y0 for control schools 

y=NULL
for (h in 1:H){
  y[[h]]=rep(NA,K[h])
  
  if (s2[h]==1 & z2[h]==1) {
    for (k in 1:K[h]){
      if (s1[[h]][k]==1) {
        y[[h]][k]=y1[[h]][k] # if school is in the sample and in the treatment group, and student is sampled, y=y1
      }  else {
          y[[h]][k]= NA # if student is not sampled, y = NA (unobserved)
          }
    }
  }
  else if (s2[h]==1 & z2[h]==0){
    for (k in 1:K[h]){
      if (s1[[h]][k]==1) {
        y[[h]][k]=y0[[h]][k] # if school is in the sample and in the control group, and student is sampled, y=y0
      } else {
        y[[h]][k]= NA # if student is not sampled, y = NA (unobserved)
      }
    }
  }
    else {
      for (k in 1:K[h]){
        y[[h]][k]=NA # if school is not sampled, y = NA (unobserved)
    }
    }
  }

y_l=unlist(y) # flatten this list and set it to "long" format 

# Combine all variables into one dataset that is ready for estimation 
# This dataset contains school ID, student ID,
# outcome for sampled student in sampled schools, 
# level 1 variables x1 & x2, level 2 variables v1 & v2
# indicators for school selection and student selection 

dataset=data.frame(cbind(schid,studentid,y_l,s1_l,s2_l,z2_l,x1_l,x2_l,v1_l,v2_l))


# Step 3. Create estimators ###############################################################################

# Estimator 1: MLM for outcome (1)


# Estimator 2. P.S. + MLM models (DB estimator)
# 2.1 P.S. for school-level selection (2)



# 2.2 P.S. for school+student level selection (3)
# school weights estiamted by school level P.S. model, student weights estimated by individual P.S. models within school)


# 2.2 P.S. for school+student level selection (4)
# School weigths estimated by school level P.S. model, student weights estimated by ML-PS model for sampled schools)


# 2.2 P.S. for simultaneous school+student level selection (5)
# Student weights estimated by ML-PS with population data 




  