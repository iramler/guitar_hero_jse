# This file contains R code for the Cmax estimator used as an example in section 3.
# Although not designed to be the most efficient R program, it is simply here to 
# assist students in writing their own functions by illustrating several useful concepts.

# Function to calculate the statistic
Cmax=function(song){
# song is a vector of 0's and 1's that represents the hits (0) and 
# misses (1) of the notes in order  

  n = length(song)  # determine the number of notes in the song

  c_temp=0  # this is a temporary counter which tallies the strings of misses

  c_max=0   # this holds the current length of the longest streak of misses

  for(i in 1:n){  # set up a for loop to scan through entire song
    if(song[i]==1){  # use this section only if a miss on note i
      c_temp=c_temp+1   # increase miss streak by 1
      if(c_temp>c_max) c_max=c_temp  # check if new streak is longer than current max 
    }
    if(song[i]==0){  # use this section only if a hit on note i
      c_temp=0       # reset current streak of misses
    }
  }

  return(c_max)  # returns the maximum consecutive number of misses
}

# Hypothesis test based on the Cmax method
Cmax_test = function(song,B=1000){
    # song is a vector of 0's and 1's that represents the hits (0) 
	# and misses (1) of the notes in order
	# B is the number of new samples to generate
	
	test_stat = Cmax(song)
	n = length(song)
	tstar_b = numeric(B)
	for(b in 1:B){
		song_tmp = sample(song)  # for permutation test
#		song_tmp = sample(song,replace=T) # for non-parametric bootstrap
#		song_tmp = rbinom(n,1,mean(song)) # for parametric bootstrap
		tstar_b[b] = Cmax(song_tmp)
	}
	
	# calculate the two-sided p-value from the sampling distribution
	pval = 2*min(sum(tstar_b >= test_stat),sum(tstar_b <= test_stat))/B
	
	# calculate a one sided p-value
#	pval = sum(tstar_b >= test_stat)/B
	
	return(pval)
}


# Examples using Songs A and B from Table 1

songA = c(1,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0)
Cmax(songA)
Cmax_test(songA,5000)


songB = c(0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,0,0)
Cmax(songB)
Cmax_test(songB,5000)