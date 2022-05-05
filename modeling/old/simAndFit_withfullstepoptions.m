%% setup
clear
addpath("mfit/");

numAgents = 50;
numAtts = 15;
numParams = numAtts*4 + 1;
numChoices = 75;
numValuesPerAtt = 5;

%% generate agent parameters
weight_params = [0 1];
weights = normrnd(weight_params(1),weight_params(2),numAgents,numAtts);
[~, max_weight_ind] = max(abs(weights),[],2);
weights_signed = sign(weights);

% I know there's a vectorized way to do this, but can't figure it out rn
% weights_lex = zeros(numAgents, numAtts);
% for i = 1:numAgents
%     weights_lex(i, max_weight_ind(i)) = sign(weights(i, max_weight_ind(i)));
% end

steps_template = [1 1 1 2 2 2 3 3 3 0 0 0 0 0 0];
steps = zeros(numAgents, numAtts);
for i = 1:numAgents
    [weights_sorted, weights_order] = sort(weights(i,:),'descend');
    
    for j = 1:numAtts
        steps(i, weights_order(j)) = steps_template(j);
    end
end

sign_weights = randi(2, numAgents, numAtts) - 1;
sign_optionvals = randi(2, numAgents, numAtts) - 1;

gamma_bounds = [1 5];
inv_temp = gamrnd(gamma_bounds(1),gamma_bounds(2),numAgents,1);

%params_3step = zeros(numAgents, numParams);
% params_WAD = zeros(numAgents, numParams);
% params_WP = zeros(numAgents, numParams);
% params_EW = zeros(numAgents, numParams);
% params_TAL = zeros(numAgents, numParams);
%params_LEX = zeros(numAgents, numParams);

for agent = 1:numAgents
    params_3step(agent,:) = [inv_temp(agent) weights(agent,:) steps(agent,:) sign_weights(agent,:) sign_optionvals(agent,:)];
    params_WAD(agent, :) = [inv_temp(agent) weights(agent,:) ones(1,numAtts) zeros(1,numAtts) zeros(1,numAtts)];
    params_WP(agent, :) = [inv_temp(agent) weights(agent,:) ones(1,numAtts) ones(1,numAtts) zeros(1,numAtts)];
    params_EW(agent, :) = [inv_temp(agent) weights(agent,:) ones(1,numAtts) zeros(1,numAtts) ones(1,numAtts)];
    params_TAL(agent, :) = [inv_temp(agent) weights(agent,:) ones(1,numAtts) ones(1,numAtts) ones(1,numAtts)];
end

%% simulate training & test data
[data_WAD] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_WAD);
[data_WAD_test] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params_WAD);

%% fit models
% set up inv temp
param(1).name = 'inverse temperature';
param(1).logpdf = @(x) sum(log(gampdf(x,gamma_bounds(1),gamma_bounds(2))));  % log density function for prior
param(1).lb = 0;    % lower bound
param(1).ub = 50;   % upper bound

% set up weights
for i = 1:numAtts
    param(i+1).name = strcat('weight',string(i));
    param(i+1).logpdf = @(x) sum(log(normpdf(x,weight_params(1),weight_params(2))));  % log density function for prior
    %param(i+1).logpdf = @(x) sum(log(normpdf(x,0,5)));
    param(i+1).lb = -25;    % lower bound
    param(i+1).ub = 25;   % upper bound
    param(i+1).logpdf2 = @(x) 1/3;  % if ew
    param(i+1).lb2 = -1;    % lower bound
    param(i+1).ub2 = 1;   % upper bound
end

% set up step parameters
for i = 1:numAtts
    param(i+1+numAtts).name = strcat('step',string(i));
    %param(i+1+numAtts).logpdf = @(x) sum(log(geopdf(x,0.5)));  % log density function for prior
    param(i+1+numAtts).logpdf = @(x) 1/(numAtts+1);
    param(i+1+numAtts).lb = 0;    % lower bound
    param(i+1+numAtts).ub = numAtts;   % upper bound
end

% set up sign_weight params
for i = 1:numAtts
    param(i+1+numAtts*2).name = strcat('sign_weight',string(i));
    param(i+1+numAtts*2).logpdf = @(x) 1/2;  % log density function for prior
    param(i+1+numAtts*2).lb = 0;    % lower bound
    param(i+1+numAtts*2).ub = 1;   % upper bound
end

% set up sign_optionval params
for i = 1:numAtts
    param(i+1+numAtts*3).name = strcat('sign_optionval',string(i));
    param(i+1+numAtts*3).logpdf = @(x) 1/2;  % log density function for prior
    param(i+1+numAtts*3).lb = 0;    % lower bound
    param(i+1+numAtts*3).ub = 1;   % upper bound
end

nstarts = 5;

[results_WAD_WAD, results_WAD_WP, results_WAD_EW, results_WAD_TAL, results_WAD_LEX] = ...
    fitModels(param, data_WAD, nstarts, numAtts);
[results_WP_WAD, results_WP_WP, results_WP_EW, results_WP_TAL, results_WP_LEX] = ...
    fitModels(param, data_WP, nstarts, numAtts);
[results_EW_WAD, results_EW_WP, results_EW_EW, results_EW_TAL, results_EW_LEX] = ...
    fitModels(param, data_EW, nstarts, numAtts);
[results_TAL_WAD, results_TAL_WP, results_TAL_EW, results_TAL_TAL, results_TAL_LEX] = ...
    fitModels(param, data_TAL, nstarts, numAtts);
[results_LEX_WAD, results_LEX_WP, results_LEX_EW, results_LEX_TAL, results_LEX_LEX] = ...
    fitModels(param, data_LEX, nstarts, numAtts);

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

cors_LEX = zeros(numParams,1);
for i = 1:numParams
    cors_LEX(i) = corr(params_LEX(:,i), results_LEX_LEX.x(:,i));
end
cors_LEX(1)
mean(cors_LEX(2:end))

% 2 possible simplifications: making all the attribute values equal, and
% making all the weights equal
% WAD does no simplifications
% WP does just the first
% EW does just the second
% TAL does both
% LEX does its own thing (where it assumes all weights are 0 except for 1
% attribute value)

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
