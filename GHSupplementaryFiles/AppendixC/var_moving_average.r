var_moving_avg=function(song,segment){
# song is a vector of 0's and 1's that represents the hits (0) and 
#   misses (1) of the notes in order  
# segment is the number of values to use in the moving average.

  var(filter(song,filter = rep(1/segment,segment)),na.rm=T)

  # It is recommended to use the R help menu help(filter) to use the uses of the filter function.
  # Note that using rep(1/segment,segment) uses equal weights of the values involved in the moving average.
}

# Hypothesis test based on the variance of the moving average method
var_moving_avg_test = function(song,segment,B=1000){
    # song is a vector of 0's and 1's that represents the hits (0) 
	# and misses (1) of the notes in order
	# B is the number of new samples to generate
	
	test_stat = var_moving_avg(song,segment)
	n = length(song)
	tstar_b = numeric(B)
	for(b in 1:B){
		song_tmp = sample(song)  # for permutation test
#		song_tmp = sample(song,replace=T) # for non-parametric bootstrap
#		song_tmp = rbinom(n,1,mean(song)) # for parametric bootstrap
		tstar_b[b] = var_moving_avg(song_tmp,segment)
	}
	
	# calculate the two-sided p-value from the sampling distribution
	pval = 2*min(sum(tstar_b >= test_stat),sum(tstar_b <= test_stat))/B
		# calculate a one sided p-value
#	pval = sum(tstar_b >= test_stat)/B
	return(pval)
}

# Examples using Songs A and B from Table 1

songA = c(1,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0)
var_moving_avg(songA,floor(sqrt(length(songA))))
var_moving_avg_test(songA,floor(sqrt(length(songA))),5000)


songB = c(0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,0,0)
var_moving_avg(songB,floor(sqrt(length(songA))))
var_moving_avg_test(songB,floor(sqrt(length(songA))),5000)
