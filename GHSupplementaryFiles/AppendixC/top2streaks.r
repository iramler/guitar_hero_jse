
twostreaks=function(song){
# song is a vector of 0's and 1's that represents the hits (0) and 
# misses (1) of the notes in order  

  temp = diff(c(0,which(song == 1),length(song)+1))-1
  top2 = -sort.int(-temp,partial=1:min(2,length(temp)))[1:2]
  len_top2 = sum(top2,na.rm = T)

  # About these commands:
  # which(song == 1) returns a vector indicating which on which notes (indices) a miss occurs
  #                this is important for you because it will indicate the end of a streak
  # c(0,which(song == 1),length(song)+1) creates a vector starting with 0 (indicates the beginning of the song),
  #                 then the locations of misses, and then the length of the song + 1 (indicates when the song has ended)
  # By doing a partial sort of the above vector we can quickly find to longest streaks. However, since the sort.int function 
  #   doesn't sort in decreasing order you need to first negate the vector of hit streaks so an 
  #   increasing sort finds the correct streaks, then negate the results to obtain the actual values.
  #   While other methods could be used to obtain the same information (possibly by sorting the entire "temp" vector)
  #   this provides a nice illustration of how to use some of the more computationally efficient tools in R that may 
  #   otherwise go unnoticed.  
  #
  # The combination of min(2,length(temp)) and the na.rm = T in the sum are needed when only one hit streak occurs.
  
  return(len_top2)
}

# Hypothesis test based on the twostreaks method
twostreaks_test = function(song,B=1000){
    # song is a vector of 0's and 1's that represents the hits (0) 
	# and misses (1) of the notes in order
	# B is the number of new samples to generate
	
	test_stat = twostreaks(song)
	n = length(song)
	tstar_b = numeric(B)
	for(b in 1:B){
		song_tmp = sample(song)  # for permutation test
#		song_tmp = sample(song,replace=T) # for non-parametric bootstrap
#		song_tmp = rbinom(n,1,mean(song)) # for parametric bootstrap
		tstar_b[b] = twostreaks(song_tmp)
	}
	
	# calculate the two-sided p-value from the sampling distribution
	pval = 2*min(sum(tstar_b >= test_stat),sum(tstar_b <= test_stat))/B
		# calculate a one sided p-value
#	pval = sum(tstar_b >= test_stat)/B
	return(pval)
}
 
# Examples using Songs A and B from Table 1

songA = c(1,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0)
twostreaks(songA)
twostreaks_test(songA,5000)


songB = c(0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,0,0)
twostreaks(songB)
twostreaks_test(songB,5000)
