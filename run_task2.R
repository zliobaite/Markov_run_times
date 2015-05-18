#6.5.2015 I.Zliobaite

#parameters
N <- 1000
tt <- 1000
W <- c(1,2,4,8,16,32,64,128,256,512)

generate_matrix <- function(N)
{
  hundredths <- seq(from=0, to=1, by=.01)
  mat <- c()
  for (sk in 1:N)
  {
    mat <- rbind(mat,sample(hundredths, size=N, replace=TRUE))
  }
  return(mat)
}

convert_matrix_to_sparse <- function(mat,W)
{
  mat_sp <- mat*0
  N <- dim(mat_sp)[2]
  for (sk in 1:N)
  {
    ind_non_zero <- sample(c(1:N), W, replace = FALSE)
    mat_sp[sk,ind_non_zero] <- mat[sk,ind_non_zero]
  }
  return(mat_sp)
}

generate_vector <- function(N)
{
  hundredths <- seq(from=0, to=1, by=.01)
  vec <- sample(hundredths, size=N, replace=TRUE)
  return(vec)
}

compute_time_raw <- function(vec,mat,tt)
{
  #start timing
  tstart <- proc.time()
  for (tm in 1:tt)
  {
    new_vec <- c()
    for (sk in 1:dim(mat)[1])
    {
      new_el <- 0
      for (sk2 in 1:dim(mat)[2])
      {
        new_el <- new_el + mat[sk,sk2]*vec[sk2]
      }
      new_vec <- c(new_vec,new_el)
    }
    vec <- new_vec  
  }
  tend <- proc.time()
  total_time <- tend - tstart
  return(round(total_time,digits=4))
}

compute_time_lib <- function(vec,mat,tt)
{
  #start timing
  tstart <- proc.time()
  for (tm in 1:tt)
  {
    vec <- mat %*% vec
  }
  tend <- proc.time()
  total_time <- tend - tstart
  return(round(total_time,digits=4))
}


vec <- generate_vector(N)
mat <- generate_matrix(N)

library(Matrix) #for sparse matrix
results <- c()
for (sk in 1:length(W))
{
  print(W[sk])
  mat_sp <- convert_matrix_to_sparse(mat,W)
  mat_sp_R <- Matrix(mat_sp)
  print(Sys.time())
  tout_sp <- compute_time_lib(vec,mat_sp,tt)['elapsed']
  print(Sys.time())
  tout_sp_R <- compute_time_lib(vec,mat_sp_R,tt)['elapsed']
  results <- rbind(results,c(W[sk],tout_sp,tout_sp_R))
  colnames(results) <- c('W','EX3','EX4')
  write.table(results,file = 'B_IZ.csv',quote = FALSE,row.names = FALSE,sep=',')
}



