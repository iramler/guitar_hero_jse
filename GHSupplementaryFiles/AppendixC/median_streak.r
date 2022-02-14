notestreak=function(song){
# song is a vector of 0's and 1's that represents the hits (0) and 
#   misses (1) of the notes in order  

  temp=diff(c(0,which(song==1),length(song)+1))-1
  streak_lengths=subset(temp,temp>0)

  # About these commands:
  # which(song==1) returns a vector indicating which on which notes (indices) a miss occurs
  #                this is important for you because it will indicate the end of a streak
  # c(0,which(song==1),length(song)+1) creates a vector starting with 0 (indicates the beginning of the song),
  #                 then the locations of misses, and then the length of the song + 1 (indicates when the song has ended)
  # diff(above vector) calculates the difference between consecutive entries in this vector; we don't actually care about 
  #			 the locations of the misses but how many hits occur between misses; we start the process of finding this 
  #			 by finding the difference between consecutive indices in the vector (note that this is not the length of
  #			 the streak, but actually the length + 1, which is why the last part of the first line subtracts 1); the 
  #			 resulting lengths are stored in "temp"


  m = median(streak_lengths)
  return (m)

}

# Hypothesis test based on the note streak method
notestreak_test = function(song,B=1000){
    # song is a vector of 0's and 1's that represents the hits (0) 
	# and misses (1) of the notes in order
	# B is the number of new samples to generate
	
	test_stat = notestreak(song)
	n = length(song)
	tstar_b = numeric(B)
	for(b in 1:B){
		song_tmp = sample(song)  # for permutation test
#		song_tmp = sample(song,replace=T) # for non-parametric bootstrap
#		song_tmp = rbinom(n,1,mean(song)) # for parametric bootstrap
		tstar_b[b] = notestreak(song_tmp)
	}
	
	# calculate the two-sided p-value from the sampling distribution
	pval = 2*min(sum(tstar_b >= test_stat),sum(tstar_b <= test_stat))/B
		# calculate a one sided p-value
#	pval = sum(tstar_b >= test_stat)/B
	return(pval)
}


# Examples using Songs A and B from Table 1

songA = c(1,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0)
notestreak(songA)
notestreak_test(songA,5000)


songB = c(0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,0,0)
notestreak(songB)
notestreak_test(songB,5000)
