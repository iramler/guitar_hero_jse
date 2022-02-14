
blockMiss = function(n){
  # n = length of song

  nsections = sqrt(n)	# quick way to determine number of sections in the song

  mix = c(0.3,0.4,0.2,0.1)	# mixture probabilities of difficulty of a section: Easy, Medium ,Hard, Very Hard
  missprop = c(0.01,0.05,0.25,0.5)	# Probability of missing a note for each of the four types of sections

  sectiontype = sample(1:4,nsections,mix,replace=T)	# randomly assign a difficulty to each section with probability from the mix vector

  sectionsize = rmultinom(1,n,rep(1,nsections))[,1]	# randomly allocate section sizes for song using multinomial distribution

  song = numeric(n)

  notes = 0
  for(i in 1:nsections){
    song[(notes+1):(notes + sectionsize[i])] = rbinom(sectionsize[i],1,missprop[sectiontype[i]])
    notes = notes + sectionsize[i]
  }

  return(song)
  
}


x = matrix(0,200,100)

for(j in 1:100){
	x[,j] = blockMiss(200)
}

write(t(x),"p.varies_block_n200_r100.txt",ncol=200)  # each row in this file is a different simulated song

y = matrix(0,600,100)
for(j in 1:100){
	y[,j] = blockMiss(600)
}

write(t(y),"p.varies_block_n600_r100.txt",ncol=600)  # each row in this file is a different simulated song
