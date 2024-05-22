functions {
	// Definition of two part density function (new version created 12 Nov 2019)
	real logtwopart_lpdf(real x, real alpha_low, real alpha_high, real tau, real x_min) {
		real prob;
		real lprob;
		
		real C_con; // Continuity constant to make sure the two pieces meet
		real C_norm; // Normalization constant to make sure the pdf integrates to 1
		
		C_con = tau ^ (alpha_low - alpha_high);
		C_norm = ( (C_con / alpha_low) * (x_min ^ (-alpha_low) - tau ^ (-alpha_low)) + ( tau ^ (-alpha_high) ) / alpha_high ) ^ -1;
		
		if (x < tau) prob = C_con * C_norm * ( x ^ - (alpha_low + 1) );
		if (x >= tau) prob = C_norm * ( x ^ - (alpha_high + 1) );

		lprob = log(prob);

		return lprob;
	}
}

data {
	int<lower=0> N;
	vector<lower=0>[N] x;
    real<lower=0> x_min;
    real<lower=0> x_max;
}

parameters {
	// Two part density
	real<lower=x_min, upper=x_max> tau;
	real<lower=0, upper=5> alpha_low;
	real<lower=0, upper=10> alpha_high;
}

model {
	// Prior: two part density
	// No prior set for tau (uniform on its interval)
	alpha_low ~ lognormal(1, 1) T[0, 5];	
	alpha_high ~ lognormal(1, 1) T[0, 10];
	
	// Likelihood: two part density
	for (i in 1:N) {
		x[i] ~ logtwopart(alpha_low, alpha_high, tau, x_min);
	}
}

generated quantities {
	vector[N] log_lik; // Log-likelihood for getting info criteria later
		
	for (i in 1:N) {
		log_lik[i] = logtwopart_lpdf(x[i] | alpha_low, alpha_high, tau, x_min);
	}
}
