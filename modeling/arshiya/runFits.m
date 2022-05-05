%% load simulated data
% this data (20 agents) was simulated using the WAD model (see the naturalistic multiattribute choice paper).
% the parameter structure is defined in "param_struct", 
% and the actual parameters used are stored in "params_WAD".

% the first parameter is the "inverse temperature"; it controls how
% deterministic (high inverse temperature) or stochastic (low inverse
% temperature) the agent's choices are.
% each agent's inverse temperature parameter was sampled from Gamma(1,5)

% the other parameters are the weights for the 10 attributes. they were
% sampled from Normal(0,1).
load('simulated_data.mat')

%% fit WAD model to data
results_WAD = fitWAD(param_struct, data_WAD);

%% did we recover accurate parameters?

% let's plot the true inverse temperature against the fitted inverse
% temperature
scatter(params_WAD(:,1), results_WAD.x(:,1))
corr(params_WAD(:,1), results_WAD.x(:,1))

% same with a couple of the weights
scatter(params_WAD(:,2), results_WAD.x(:,2))
corr(params_WAD(:,2), results_WAD.x(:,2))

scatter(params_WAD(:,3), results_WAD.x(:,3))
corr(params_WAD(:,3), results_WAD.x(:,3)) 

scatter(params_WAD(:,4), results_WAD.x(:,4))
corr(params_WAD(:,4), results_WAD.x(:,4))

% these correlations should be > 0.6 (it's fine if they're not perfect)