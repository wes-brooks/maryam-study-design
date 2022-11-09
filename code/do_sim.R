system.time( GA1 <- ga(type = "binary", fitness = fitness, 
          nBits = 200, trt=trt_short,  age_effect=0.2, male_effect=0.3, trt_effect=1, B=100,
          popSize = 100, maxiter = 1200,  parallel=TRUE) )
