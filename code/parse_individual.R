parse_individual = function( vec ) {
  control = decode( vec )
  
  result = matrix(0, 8, 2)
  
  for ( i in seq_len(nrow(control)) ) {
    result[ control[i,1]+1, control[i,2]+1 ] = result[ control[i,1]+1, control[i,2]+1 ] + 1
  }
  
  result
}