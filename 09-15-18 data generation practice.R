
# Step 1. Simulate potential outcomes for all students in all schools #
# number of schools = 1, ..., H, number of student per school = 1, ... ,K-h 
# Two school-level covariates, V1 & V2 
# V1~ is continuous N(0, 1), V2 is binary Ber(0.2)

for (h in 1:H){
  v1[h]=rnorm(1,0,1)
  v2[h]=rbinom(1,1,0.2)
}


for (i in 1:N){
  # Two pretreatment variables X1 and X2
  x1[i]=rnorm(1, 1, 1)
  x2[i]=rnorm(1, 1, 1)
  # 1 IV X3
  #x3[i]=rnorm(1, 1, 1)
  # 1 missing moderator x4
  x4[i]=rnorm(1, 1, 1)
  # Indicator of selection into experimental vs control groups, S
  # In the IV situation, X3 is predictive of sample selection but not treatment effects but X3 is not a moderator
  p[i]=1/(1+exp(-(gamma1_n*x1[i]+gamma2_n*x2[i])))
  s[i]=rbinom(1,1,p[i])
  