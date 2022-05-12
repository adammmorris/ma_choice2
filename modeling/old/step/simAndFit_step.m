%% SIMULATES AGENTS FROM EACH MODEL MAKING CHOICES & THEN FITS THE MODELS TO THEM

%% setup
clear
addpath("mfit/");

numAgents = 25;
numAtts = 10;
numChoices = 75;
numValuesPerAtt = 5;

%% generate agent parameters
weight_params = [0 1];
weights = normrnd(weight_params(1),weight_params(2),numAgents,numAtts);
[~, max_weight_ind] = max(abs(weights),[],2);
weights_signed = sign(weights);

%steps_template = [1 2 3 0 0 0 0 0 0 0 0 0 0 0 0];
steps_template = [1 1 2 2 3 3 0 0 0 0 0 0 0 0 0];
%steps_template = ones(1,numAtts);
steps = zeros(numAgents, numAtts);
for i = 1:numAgents
    [weights_sorted, weights_order] = sort(abs(weights(i,:)),'descend');
    
    for j = 1:numAtts
        steps(i, weights_order(j)) = steps_template(j);
    end
end

gamma_bounds = [1 5];
inv_temp = gamrnd(gamma_bounds(1),gamma_bounds(2),numAgents,1);

for agent = 1:numAgents
    params_WAD(agent, :) = [inv_temp(agent) weights(agent,:)];
    params_WP(agent, :) = [inv_temp(agent) weights(agent,:)];
    params_EW(agent, :) = [inv_temp(agent) weights_signed(agent,:)];
    params_TAL(agent, :) = [inv_temp(agent) weights_signed(agent,:)];
    params_WAD_step(agent, :) = [inv_temp(agent) weights(agent,:) steps(agent,:)];
    params_WP_step(agent, :) = [inv_temp(agent) weights(agent,:) steps(agent,:)];
    params_EW_step(agent, :) = [inv_temp(agent) weights_signed(agent,:) steps(agent,:)];
    params_TAL_step(agent, :) = [inv_temp(agent) weights_signed(agent,:) steps(agent,:)];
end

%% make param structures
% set up inv temp
param_struct(1).name = 'inverse temperature';
param_struct(1).logpdf = @(x) sum(log(gampdf(x,gamma_bounds(1),gamma_bounds(2))));  % log density function for prior
param_struct(1).lb = 0;    % lower bound
param_struct(1).ub = 50;   % upper bound
param_struct(1).int = 0;   % constrained to be an integer?

% set up weights
for i = 1:numAtts
    param_struct(i+1).name = strcat('weight',string(i));

    % for full
    param_struct(i+1).logpdf = @(x) sum(log(normpdf(x,weight_params(1),weight_params(2))));  % log density function for prior
    param_struct(i+1).lb = -25;    % lower bound
    param_struct(i+1).ub = 25;   % upper bound
    param_struct(i+1).int = 0;
end

param_struct_signedweights = param_struct;
for i = 1:numAtts
    param_struct_signedweights(i+1).logpdf = @(x) 1/3; 
    param_struct_signedweights(i+1).lb = -1; 
    param_struct_signedweights(i+1).ub = 1;
    param_struct_signedweights(i+1).int = 1;
end

% set up step parameters
param_struct_step = param_struct;
param_struct_signedweights_step = param_struct_signedweights;
for i = 1:numAtts
    param_struct_step(i+1+numAtts).name = strcat('step',string(i));
    param_struct_step(i+1+numAtts).lb = 0;    % lower bound
    param_struct_step(i+1+numAtts).ub = 3;   % upper bound
    param_struct_step(i+1+numAtts).logpdf = @(x) (1/2 * (x == 0) + 1/6 * (x ~= 0));
    param_struct_step(i+1+numAtts).int = 1;

    param_struct_signedweights_step(i+1+numAtts).name = strcat('step',string(i));
    param_struct_signedweights_step(i+1+numAtts).lb = 0;    % lower bound
    param_struct_signedweights_step(i+1+numAtts).ub = 3;   % upper bound
    param_struct_signedweights_step(i+1+numAtts).logpdf = @(x) (1/2 * (x == 0) + 1/6 * (x ~= 0));
    param_struct_signedweights_step(i+1+numAtts).int = 1;
end

nstarts = 1;
numParams = length(param_struct);
numParams_step = length(param_struct_step);

%% simulate training & test data
[data_WAD] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_WAD, false);
[data_WAD_test] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_WAD, false);

[data_WP] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_WP, true);
[data_EW] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_EW, false);
[data_TAL] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_TAL, true);

[data_WAD_step, probs_gen] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_WAD_step, false);
[data_WAD_step_test] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_WAD_step, false);

[data_WP_step] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_WP_step, true);
[data_EW_step] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_EW_step, false);
[data_TAL_step] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_TAL_step, true);

%% fit models

[results_WADs_WADs, results_WADs_WPs, results_WADs_EWs, results_WADs_TALs] = ...
    fitModels_step(param_struct_step, param_struct_signedweights_step, data_WAD_step, {'WAD'});%, results_WADs_WADs.x(1,:), params_WAD_step(1,:), probs_gen);
[results_WADs_WAD, results_WADs_WP, results_WADs_EW, results_WADs_TAL] = ...
    fitModels(param_struct, param_struct_signedweights, data_WAD_step, nstarts, {'WAD', 'WP'});
mean(mfit_predict(data_WAD_step_test, results_WADs_WADs))
mean(mfit_predict(data_WAD_step_test, results_WADs_WAD))
mfit_predict(data_WAD_step_test, results_WADs_WADs) - mfit_predict(data_WAD_step_test, results_WADs_WAD)

[results_WP_WAD, results_WP_WP, results_WP_EW, results_WP_TAL, results_WP_LEX] = ...
    fitModels(param_struct, data_WP, nstarts, numAtts);
[results_EW_WAD, results_EW_WP, results_EW_EW, results_EW_TAL, results_EW_LEX] = ...
    fitModels(param_struct, data_EW, nstarts, numAtts);
[results_TAL_WAD, results_TAL_WP, results_TAL_EW, results_TAL_TAL, results_TAL_LEX] = ...
    fitModels(param_struct, data_TAL, nstarts, numAtts);
[results_LEX_WAD, results_LEX_WP, results_LEX_EW, results_LEX_TAL, results_LEX_LEX] = ...
    fitModels(param_struct, data_LEX, nstarts, numAtts);

results_WAD_all = [results_WAD_WAD, results_WAD_WP, results_WAD_EW, results_WAD_TAL, results_WAD_LEX];
results_WP_all = [results_WP_WAD, results_WP_WP, results_WP_EW, results_WP_TAL, results_WP_LEX];
results_EW_all = [results_EW_WAD, results_EW_WP, results_EW_EW, results_EW_TAL, results_EW_LEX];
results_TAL_all = [results_TAL_WAD, results_TAL_WP, results_TAL_EW, results_TAL_TAL, results_TAL_LEX];
results_LEX_all = [results_LEX_WAD, results_LEX_WP, results_LEX_EW, results_LEX_TAL, results_LEX_LEX];

save('fitting.mat');

%% Test fits
cors_WAD = zeros(numParams,1);
diffs_WAD = zeros(numParams,1);
for i = 1:numParams
    cors_WAD(i) = corr(params_WAD(:,i), results_WAD_WAD.x(:,i));
    diffs_WAD(i) = mean(abs(params_WAD(:,i) - results_WAD_WAD.x(:,i)));
end
cors_WAD(1)
mean(cors_WAD(2:end))
mean(diffs_WAD(1))
mean(diffs_WAD(2:end))

cors_WAD_EW = zeros(numParams,1);
diffs_WAD_EW = zeros(numParams,1);
for i = 1:numParams
    cors_WAD_EW(i) = corr(params_WAD(:,i), results_WAD_EW.x(:,i));
    diffs_WAD_EW(i) = mean(abs(params_WAD(:,i) - results_WAD_EW.x(:,i)));
end
cors_WAD_EW(1)
mean(cors_WAD_EW(2:end))
mean(diffs_WAD_EW(1))
mean(diffs_WAD_EW(2:end))

cors_WP = zeros(numParams,1);
for i = 1:numParams
    cors_WP(i) = corr(params_WP(:,i), results_WP_WP.x(:,i));
end
cors_WP(1)
mean(cors_WP(2:end))

cors_EW = zeros(numParams,1);
for i = 1:numParams
    cors_EW(i) = corr(params_EW(:,i), results_EW_EW.x(:,i));
end
cors_EW(1)
mean(cors_EW(2:end))

cors_TAL = zeros(numParams,1);
for i = 1:numParams
    cors_TAL(i) = corr(params_TAL(:,i), results_TAL_TAL.x(:,i));
end
cors_TAL(1)
mean(cors_TAL(2:end))

% step
cors_WAD_step = zeros(numParams_step,1);
diffs_WAD_step = zeros(numParams_step,1);
equal_WAD_step = zeros(numParams_step,1);
for i = 1:numParams_step
    cors_WAD_step(i) = corr(params_WAD_step(:,i), results_WADs_WADs.x(:,i));
    diffs_WAD_step(i) = mean(abs(params_WAD_step(:,i) - results_WADs_WADs.x(:,i)));
    equal_WAD_step(i) = mean(params_WAD_step(:,i) == results_WADs_WADs.x(:,i));
end
mean(cors_WAD_step(2:(numAtts+1)))
mean(cors_WAD_step((numAtts+2):end))
equal_WAD_step((numAtts+2):end)
scatter(params_WAD_step(:,(numAtts+2):end), results_WAD_WAD.x(:,(numAtts+2):end), 'jitter', 'on')

bms_results_WAD = mfit_bms(results_WAD_all, 1);
bms_results_WAD.pxp
bms_results_WP = mfit_bms(results_WP_all, 1);
bms_results_WP.pxp
bms_results_EW = mfit_bms(results_EW_all, 1);
bms_results_EW.pxp
bms_results_TAL = mfit_bms(results_TAL_all, 1);
bms_results_TAL.pxp
bms_results_LEX = mfit_bms(results_LEX_all, 1);
bms_results_LEX.pxp

%% do cross-validation
[cv_WAD_WAD, cv_WAD_WP, cv_WAD_EW, cv_WAD_TAL, cv_WAD_LEX] = crossValidateModels(data_WAD_test, results_WAD_all);
[cv_WP_WAD, cv_WP_WP, cv_WP_EW, cv_WP_TAL, cv_WP_LEX] = crossValidateModels(data_WP_test, results_WP_all);
[cv_EW_WAD, cv_EW_WP, cv_EW_EW, cv_EW_TAL, cv_EW_LEX] = crossValidateModels(data_EW_test, results_EW_all);
[cv_TAL_WAD, cv_TAL_WP, cv_TAL_EW, cv_TAL_TAL, cv_TAL_LEX] = crossValidateModels(data_TAL_test, results_TAL_all);
[cv_LEX_WAD, cv_LEX_WP, cv_LEX_EW, cv_LEX_TAL, cv_LEX_LEX] = crossValidateModels(data_LEX_test, results_LEX_all);

cv_WAD_all = [cv_WAD_WAD, cv_WAD_WP, cv_WAD_EW, cv_WAD_TAL, cv_WAD_LEX];
cv_WP_all = [cv_WP_WAD, cv_WP_WP, cv_WP_EW, cv_WP_TAL, cv_WP_LEX];
cv_EW_all = [cv_EW_WAD, cv_EW_WP, cv_EW_EW, cv_EW_TAL, cv_EW_LEX];
cv_TAL_all = [cv_TAL_WAD, cv_TAL_WP, cv_TAL_EW, cv_TAL_TAL, cv_TAL_LEX];
cv_LEX_all = [cv_LEX_WAD, cv_LEX_WP, cv_LEX_EW, cv_LEX_TAL, cv_LEX_LEX];

cv_all = [mean(cv_WAD_all); mean(cv_WP_all); mean(cv_EW_all); mean(cv_TAL_all); mean(cv_LEX_all)];
cv_all

mean(cv_WAD_all(:,1) > max(cv_WAD_all(:,2:end),[],2))

which_cv = cv_LEX_all;
which_models = [5,1];

graphCV(which_cv, which_models);
