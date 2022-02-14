#############################################
# Part i
n = 200
p = 0.3
rho = -0.3

# subfunction needed to generate data
calcbij = function(rho,i){
  a = ((1-4*rho*rho)^0.5-1)/(2*rho)
  j = i-1
  bi = (a^(1:j) - a^(-(1:j)))/(a^-i - a^(i))
  return(bi)
}

# function to generate one dataset
# following Qaqish (2003) algorithm
gendataPW = function(n,p,rho){
  dat = c(rbinom(1,1,p),rep(NA,n-1))
  for(i in 2:n){
    j = i-1
    bi = calcbij(rho,i)
    prob = p + sum(bi*(dat[1:j]-p))
    if(prob < 0) prob = 0.0001
    if(prob > 1) prob = 0.9999
    dat[i] = rbinom(1,1,prob)
  }
  return(dat)
}

# Generate 100 replicates
x = matrix(NA,100,n)
for(r in 1:100){
  x[r,]=gendataPW(n,p,rho)
}
write(t(x),"p.30_pair.neg30_n200_r100.txt",ncol=n) # each row in this file is a different simulated song

#############################################
# Part ii
n = 600
p = 0.3
rho = -0.3

# subfunction needed to generate data
calcbij = function(rho,i){
  a = ((1-4*rho*rho)^0.5-1)/(2*rho)
  j = i-1
  bi = (a^(1:j) - a^(-(1:j)))/(a^-i - a^(i))
  return(bi)
}

# function to generate one dataset
# following Qaqish (2003) algorithm
gendataPW = function(n,p,rho){
  dat = c(rbinom(1,1,p),rep(NA,n-1))
  for(i in 2:n){
    j = i-1
    bi = calcbij(rho,i)
    prob = p + sum(bi*(dat[1:j]-p))
    if(prob < 0) prob = 0.0001
    if(prob > 1) prob = 0.9999
    dat[i] = rbinom(1,1,prob)
  }
  return(dat)
}

# Generate 100 replicates
x = matrix(NA,100,n)
for(r in 1:100){
  x[r,]=gendataPW(n,p,rho)
}
write(t(x),"p.30_pair.neg30_n600_r100.txt",ncol=n)   # each row in this file is a different simulated song
