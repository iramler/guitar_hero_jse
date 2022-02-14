
nstreaks=function(song)
{
# song is a vector of 0's and 1's that represents the hits (0) and 
# misses (1) of the notes in order  

# determine the total number of hit streaks
  streaks = sum(diff(c(song,1)) == 1)

  # About these commands:
  # diff(c(song,1) calculates the difference between consecutive entries in this vector; we don't actually care about 
  #			 the locations of the misses but how many hits occur between misses; we start the process of finding this 
  #			 by finding the difference between consecutive indices in the vector.  The end of hit streaks are indicated
  # 		 by this difference being 1 (i.e., going from a hit of 0 to a miss of 1).  Attaching an additional 1 at
  # 		 the end of the song will ensure that if the song ends in a steak of hits it is included in the total.
  # sum( above vector == 1) counts the number of hit streaks

  return(streaks)
}

# Hypothesis test based on the nstreaks test method
nstreaks_test = function(song,B=1000){
    # song is a vector of 0's and 1's that represents the hits (0) 
	# and misses (1) of the notes in order
	# B is the number of new samples to generate
	
	test_stat = nstreaks(song)
	n = length(song)
	tstar_b = numeric(B)
	for(b in 1:B){
		song_tmp = sample(song)  # for permutation test
#		song_tmp = sample(song,replace=T) # for non-parametric bootstrap
#		song_tmp = rbinom(n,1,mean(song)) # for parametric bootstrap
		tstar_b[b] = nstreaks(song_tmp)
	}
	
	# calculate the two-sided p-value from the sampling distribution
	pval = 2*min(sum(tstar_b >= test_stat),sum(tstar_b <= test_stat))/B
		# calculate a one sided p-value
#	pval = sum(tstar_b >= test_stat)/B
	return(pval)
}

# Examples using Songs A and B from Table 1

songA = c(1,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0)
nstreaks(songA)
nstreaks_test(songA,5000)


songB = c(0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,0,0)
nstreaks(songB)
nstreaks_test(songB,5000)