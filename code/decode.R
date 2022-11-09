decode = function(bits) {
  
  indx = c(1:50, rep(51:100, each=3) )
  
  bin = split( bits, indx )
  
  grp = sapply( bin[51:100], function(x) x |> gray2binary() |> binary2decimal() )
  
  
  data.frame( age_grp=grp, male=unlist(bin[1:50]))
}