# Markov_run_times
Tests running times for Markov chains with different number of states and different sparsity levels. The setting is intended for traffic modeling and route prediction applications.

data/A_IZ.csv has a header, and an entry (running time, in seconds) for each for each of the five exercises, for N (number of states) 5 10 20 40 80 160 320 640 1280 2560 5120 10240 20000 40000 80000, constant T=1000 (number of time steps) and W=5 (number of columns when sparsity is considered)

data/B_IZ.csv has a header, and an entry (running time, in seconds) for the two exercises to do with sparse data, for W (number of columns) 1 2 4 8 16 32 64 128 256 512, constant T=1000 and N=1000.

Tasks:

	EX1 - raw implementation, no libraries
	EX2 - R native matrix multiplication
	EX3 - sparse matrix with R native matrix multiplication
	EX4 - sparse matrix with R native multiplication and R sparce format
	EX4raw - attempt to do raw implementation with R sparce format, very slow