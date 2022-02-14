# Idea behind the KS test for this problem:
# The KS test is a hypothesis test used to determine if a sample of 
# data came from a specific probability distribution,
# though theoretically this test only works for continuous distributions. 
# The test works by comparing the CDF for a 
# given reference distribution to the empirical CDF for the sample of data.
# In this setting, under the null hypothesis of misses occurring at random, 
# the notes (locations) of the misses can be
# modeled with a discrete uniform distribution.


ks=function(song){
# song is a vector of 0's and 1's that represents the hits (0) and 
# misses (1) of the notes in order  

  misses = which(song==1) # determines the locations/indices of misses and stores those in the vector "misses"
                          # which(song==1) returns a vector indicating which on which notes (indices) a miss occurs

  n_misses = length(misses) # number of misses in a song (basically the sample size for the sample being tested)

 # empirical cdf is really just number of misses at or before the current miss on note i divided by the total number of misses

 e_cdf = 1:n_misses/n_misses # a vector holding the empirical cdf evaluated at each miss (each index in misses)
   # Note that for the first miss (entry in misses) the e_cdf is 1/n_misses, it is 2/n_misses for the second,..., n_misses/n_misses=1 for the last

 # the reference cdf for a discrete uniform is F(x) = x/(# of possible values); here that will be the length of the song

 n = length(song)

 r_cdf = misses/n  # a vector that stores the reference cdf evaluated at the location of each miss

 # to calculate the KS test statistic subtract the empirical and reference cdfs for each note (and take the absolute value of that difference)
 # the test stat is the largest one of these absolute differences
 ks_ts = max(abs(e_cdf-r_cdf))

 return(ks_ts)  # return the KS test statistic

}

# Hypothesis test based on the KS test method
ks_test = function(song, B = 1000){
    # song is a vector of 0's and 1's that represents the hits (0) 
	# and misses (1) of the notes in order
	# B is the number of new samples to generate

	test_stat = ks(song)
	n = length(song)
	tstar_b = numeric(B)
	for(b in 1:B){
		song_tmp = sample(song)  # for permutation test
#		song_tmp = sample(song,replace=T) # for non-parametric bootstrap
#		song_tmp = rbinom(n,1,mean(song)) # for parametric bootstrap
		tstar_b[b] = ks(song_tmp)
	}
	
	pval = sum(tstar_b > test_stat)/B
	
	return(pval)
}



# Examples using Songs A and B from Table 1

songA = c(1,0,0,0,0,0,1,0,1,0,0,1,0,0,0,0,1,0,0,0)
ks(songA)
ks_test(songA,5000)


songB = c(0,0,0,1,0,0,0,0,0,0,0,1,1,1,1,0,0)
ks(songB)
ks_test(songB,5000)