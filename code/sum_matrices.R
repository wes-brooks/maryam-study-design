sum_matrices = function( matrix_list ) {
  len = length( matrix_list )
  
  if (len == 1)
    return( matrix_list[[1]] )
  
  return( matrix_list[[1]] + sum_matrices( matrix_list[ seq_len(len)[-1] ])  )
}