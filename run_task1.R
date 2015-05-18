#6.5.2015 I.Zliobaite

#parameters
N <- c(5,10,20,40,80,160,320,640,1280,2560,5120,10240,20000,40000,80000)
tt <- 1000
W <- 5

maxN1 <- 2000
maxN2 <- 5000
maxN3 <- 2000
maxN4 <- 5000
maxN4raw <- 50

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


library(Matrix) #for sparse matrix
results <- c()
for (sk in 1:length(N))
{
  print(N[sk])
  vec <- generate_vector(N[sk])
  mat <- generate_matrix(N[sk])
  mat_sp <- convert_matrix_to_sparse(mat,W)
  mat_sp_R <- Matrix(mat_sp)
  sz_raw <- object.size(mat)
  sz_sp <- object.size(mat_sp)
  sz_sp_R <- object.size(mat_sp_R)
  if (N[sk]<=maxN1)
  {
    print(Sys.time())
    tout_raw <- compute_time_raw(vec,mat,tt)['elapsed']
  } else {
    tout_raw <- 'DNF'
  }
  if (N[sk]<=maxN2)
  {
    print(Sys.time())
    tout_lib <- compute_time_lib(vec,mat,tt)['elapsed'] 
  } else {
    tout_lib <- 'DNF'
  }
  if (N[sk]<=maxN3)
  {
    print(Sys.time())
    tout_sp <- compute_time_lib(vec,mat_sp,tt)['elapsed']
  } else {
    tout_sp <- 'DNF'
  }
  if (N[sk]<=maxN4)
  {
    print(Sys.time())
    tout_sp_R <- compute_time_lib(vec,mat_sp_R,tt)['elapsed']
  } else {
    tout_sp_R <- 'DNF'
  }
  if (N[sk]<=maxN4raw)
  {
    print(Sys.time())
    tout_sp_R_raw <- compute_time_raw(vec,mat_sp_R,tt)['elapsed']
  } else {
    tout_sp_R_raw <- 'DNF'
  }
  results <- rbind(results,c(N[sk],tout_raw,tout_lib,tout_sp,tout_sp_R,tout_sp_R_raw,sz_raw,sz_sp,sz_sp_R))
  colnames(results) <- c('N','EX1','EX2','EX3','EX4','EX4raw','size_full','size_sparse','size_lib_sparse')
  write.table(results,file = 'A_IZ.csv',quote = FALSE,row.names = FALSE,sep=',')
}


