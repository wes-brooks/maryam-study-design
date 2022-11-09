# Analyze the result of the genetic algorithm

# load the saved result
load( "output/GA1.rdata" )

# loop through all the individuals in the final population and calculate the mean frequency of each cell in the table:
res = lapply( seq_len(nrow(GA1@population)), function(k) parse_individual(GA1@population[k,]) )

