%% fits the WAD model
function [results_WAD] = fitWAD(param_struct, data)

% get number of subjects & params
numSubj = length(data);
numParams = length(param_struct);

% our log likelihood function
lik = @(x,d) getLogLik(x, d);

% set up results matrices
logpost_WAD = zeros(numSubj, 1);
loglik_WAD = zeros(numSubj, 1);
best_fit_params_WAD = zeros(numSubj, numParams);
BICs_WAD = zeros(numSubj, 1);
AICs_WAD = zeros(numSubj, 1);

% loop through each subject
% (you can change this to "for" if you don't want to do parallelization; parallelization is faster, but messes up some debugging stuff)
%parfor s = 1:numSubj
for s = 1:1
    disp(['Fitting WAD for subject ', num2str(s)]);

    % our posterior (the log likelihood + the log prior)
    % we make it negative because the "ga" function minimizes its
    % objective
    WAD_post = @(x) -(lik(x,data(s)) + getPriorSum(x, param_struct));

    % do optimization with the genetic algorithm (ga) function
    [x,logpost] = ga(WAD_post, length(param_struct),[],[],[],[],vertcat(param_struct.lb), ...
        vertcat(param_struct.ub),[],find(vertcat(param_struct.int)));

    % store best fit params
    best_fit_params_WAD(s,:) = x;
    % store the maximum likelihood
    loglik_WAD(s) = lik(x, data(s));
    % store the maximum posterior
    logpost_WAD(s) = -logpost;
    % store the BIC and AIC
    BICs_WAD(s) = numParams*log(data(s).N) - 2*loglik_WAD(s);
    AICs_WAD(s) = numParams*2 - 2*loglik_WAD(s);

    disp(['Completed optimization for WAD for subject ', num2str(s)]);
end

% make the results struct
results_template.K = numParams;
results_template.S = numSubj;
results_template.param = param_struct;

results_WAD = results_template;
results_WAD.logpost = logpost_WAD';
results_WAD.loglik = loglik_WAD;
results_WAD.x = best_fit_params_WAD;
results_WAD.bic = BICs_WAD;
results_WAD.aic = AICs_WAD;
results_WAD.likfun = lik;

end