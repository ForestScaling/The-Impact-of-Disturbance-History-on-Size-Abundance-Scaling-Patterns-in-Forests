data {
	int<lower=0> N;
	real<lower=0> x_min;
	vector<lower=0>[N] x;
	vector<lower=0>[N] y;
}

parameters {
	// Pareto density
	real<lower=0, upper=5> alpha;
}

model {
	// Prior: Pareto density
	alpha ~ lognormal(1, 1) T[0, 5];
	// x_min uniform on its interval.
	
	// Likelihood: Pareto density
	x ~ pareto(x_min, alpha);
}

