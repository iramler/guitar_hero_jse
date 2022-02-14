############################################
# Part v
p = 0.1
rho = 0.5
n = 200


# function to generate one dataset
# following Qaqish (2003) algorithm
gendataAR1 = function(n,p,rho){
  dat = c(rbinom(1,1,p),rep(NA,n-1))
  for(i in 2:n){
    j = i-1
    prob = p + rho*(dat[j]-p)
    dat[i] = rbinom(1,1,prob)
  }
  return(dat)
}

# Generate 100 replicates
x = matrix(NA,100,n)
for(r in 1:100){
  x[r,]=gendataAR1(n,p,rho)
}

write(t(x),"p.10_ar50_n200_r100.txt",ncol=n) # each row in this file is a different simulated song


#############################################
# Part vi
p = 0.1
rho = 0.5
n = 600

# function to generate one dataset
# following Qaqish (2003) algorithm
gendataAR1 = function(n,p,rho){
  dat = c(rbinom(1,1,p),rep(NA,n-1))
  for(i in 2:n){
    j = i-1
    prob = p + rho*(dat[j]-p)
    dat[i] = rbinom(1,1,prob)
  }
  return(dat)
}

# Generate 100 replicates
x = matrix(NA,100,n)
for(r in 1:100){
  x[r,]=gendataAR1(n,p,rho)
}

write(t(x),"p.10_ar50_n600_r100.txt",ncol=n) # each row in this file is a different simulated song

#############################################
# Part vii
p = 0.1
rho = 0.3
n = 200

# function to generate one dataset
# following Qaqish (2003) algorithm
gendataAR1 = function(n,p,rho){
  dat = c(rbinom(1,1,p),rep(NA,n-1))
  for(i in 2:n){
    j = i-1
    prob = p + rho*(dat[j]-p)
    dat[i] = rbinom(1,1,prob)
  }
  return(dat)
}

# Generate 100 replicates
x = matrix(NA,100,n)
for(r in 1:100){
  x[r,]=gendataAR1(n,p,rho)
}

write(t(x),"p.10_ar30_n200_r100.txt",ncol=n) # each row in this file is a different simulated song

#############################################
# Part viii
p = 0.1
rho = 0.3
n = 600

# function to generate one dataset
# following Qaqish (2003) algorithm
gendataAR1 = function(n,p,rho){
  dat = c(rbinom(1,1,p),rep(NA,n-1))
  for(i in 2:n){
    j = i-1
    prob = p + rho*(dat[j]-p)
    dat[i] = rbinom(1,1,prob)
  }
  return(dat)
}

# Generate 100 replicates
x = matrix(NA,100,n)
for(r in 1:100){
  x[r,]=gendataAR1(n,p,rho)
}

write(t(x),"p.10_ar30_n600_r100.txt",ncol=n) # each row in this file is a different simulated song

#############################################
# Part ix
p = 0.2
rho = 0.5
n = 200

# function to generate one dataset
# following Qaqish (2003) algorithm
gendataAR1 = function(n,p,rho){
  dat = c(rbinom(1,1,p),rep(NA,n-1))
  for(i in 2:n){
    j = i-1
    prob = p + rho*(dat[j]-p)
    dat[i] = rbinom(1,1,prob)
  }
  return(dat)
}

# Generate 100 replicates
x = matrix(NA,100,n)
for(r in 1:100){
  x[r,]=gendataAR1(n,p,rho)
}

write(t(x),"p.20_ar50_n200_r100.txt",ncol=n) # each row in this file is a different simulated song

#############################################
# Part x
p = 0.2
rho = 0.5
n = 600

# function to generate one dataset
# following Qaqish (2003) algorithm
gendataAR1 = function(n,p,rho){
  dat = c(rbinom(1,1,p),rep(NA,n-1))
  for(i in 2:n){
    j = i-1
    prob = p + rho*(dat[j]-p)
    dat[i] = rbinom(1,1,prob)
  }
  return(dat)
}

# Generate 100 replicates
x = matrix(NA,100,n)
for(r in 1:100){
  x[r,]=gendataAR1(n,p,rho)
}

write(t(x),"p.20_ar50_n600_r100.txt",ncol=n) # each row in this file is a different simulated song

