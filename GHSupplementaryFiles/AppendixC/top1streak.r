
MaxHit=function(song){
# song is a vector of 0's and 1's that represents the hits (0) and 
# misses (1) of the notes in order  

  streak_lengths=diff(c(0,which(song==1),length(song)+1))-1
  max_streak=max(streak_lengths)
  
  # About these commands:
  # which(song==1) returns a vector indicating which on which notes (indices) a miss occurs
  #                this is important for you because it will indicate the end of a streak
  # c(0,which(song==1),length(song)+1) creates a vector starting with 0 (indicates the beginning of the song),
  #                 then the locations of misses, and then the length of the song + 1 (indicates when the song has ended)
  
  return(max_streak)
}

# Hypothesis test based on the MaxHit method
MaxHit_test = function(song,B=1000){
    # song is a vector of 0's and 1's that represents the hits (0) 
	# and misses (1) of the notes in order
	# B is the number of new samples to generate
	
	test_stat = MaxHit(song)
	n = length(song)
	tstar_b = numeric(B)
	for(b in 1:B){
		song_tmp = sample(song)  # for permutation test
#		song_tmp = sample(song,replace=T) # for non-parametric bootstrap
#		song_tmp = rbinom(n,1,mean(song)) # for parametric bootstrap
		tstar_b[b] = MaxHit(song_tmp)
	}
	
	# calculate the two-sided p-value from the sampling distribution
	pval = 2*min(sum(tstar_b >= test_stat),sum(tstar_b <= test_stat))/B
		# calculate a one sided p-value
#	pval = sum(tstar_b >= test_stat)/B
	return(pval)
}

# Examples using Songs A and B from Table 1

songA = c(1,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0)
MaxHit(songA)
MaxHit_test(songA,5000)


songB = c(0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,0,0)
MaxHit(songB)
MaxHit_test(songB,5000)
