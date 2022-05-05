%% setup
clear
addpath("mfit/");

numAgents = 100;
numAtts = 15;
numParams = numAtts + 1;
numChoices = 75;
numValuesPerAtt = 5;

%% generate options
options = randi([1 numValuesPerAtt], numAtts, 2, numChoices, numAgents) / numValuesPerAtt;
options_signed = signOptions(options);

%% generate agent parameters
weight_params = [0 1];
weights = normrnd(weight_params(1),weight_params(2),numAgents,numAtts);
[~, max_weight_ind] = max(abs(weights),[],2);
weights_signed = sign(weights);

% I know there's a vectorized way to do this, but can't figure it out rn
weights_lex = zeros(numAgents, numAtts);
for i = 1:numAgents
    weights_lex(i, max_weight_ind(i)) = sign(weights(i, max_weight_ind(i)));
end

gamma_bounds = [1 5];
inv_temp = gamrnd(gamma_bounds(1),gamma_bounds(2),numAgents,1);

struct_template.N = numChoices;
struct_template.options = options;

for agent = 1:numAgents
    data_WAD(agent) = struct_template;
    data_WP(agent) = struct_template;
    data_EW(agent) = struct_template;
    data_TAL(agent) = struct_template;
    data_LEX(agent) = struct_template;
end

params_WAD = zeros(numAgents, numParams);
params_WP = zeros(numAgents, numParams);
params_EW = zeros(numAgents, numParams);
params_TAL = zeros(numAgents, numParams);
params_LEX = zeros(numAgents, numParams);

%% simulate data
for agent = 1:numAgents

    data_WAD(agent).options = options(:,:,:,agent);
    data_WAD(agent).params = [inv_temp(agent) weights(agent,:)];
    params_WAD(agent, :) = data_WAD(agent).params;
    data_WAD(agent).choices = makeChoice_WAD(data_WAD(agent).params, data_WAD(agent).options);

    data_WP(agent).options = options(:,:,:,agent);
    data_WP(agent).params = [inv_temp(agent) weights(agent,:)];
    params_WP(agent, :) = data_WP(agent).params;
    data_WP(agent).choices = makeChoice_WAD(data_WP(agent).params, signOptions(data_WP(agent).options));

    data_EW(agent).options = options(:,:,:,agent);
    data_EW(agent).params = [inv_temp(agent) weights_signed(agent,:)];
    params_EW(agent, :) = data_EW(agent).params;
    data_EW(agent).choices = makeChoice_WAD(data_EW(agent).params, data_EW(agent).options);

    data_TAL(agent).options = options(:,:,:,agent);
    data_TAL(agent).params = [inv_temp(agent) weights_signed(agent,:)];
    params_TAL(agent, :) = data_TAL(agent).params;
    data_TAL(agent).choices = makeChoice_WAD(data_TAL(agent).params, signOptions(data_TAL(agent).options));

    data_LEX(agent).options = options(:,:,:,agent);
    data_LEX(agent).params = [inv_temp(agent) weights_lex(agent,:)];
    params_LEX(agent, :) = data_LEX(agent).params;
    data_LEX(agent).choices = makeChoice_WAD(data_LEX(agent).params, data_LEX(agent).options);
end

save('simdata.mat');

%% fit models
param(1).name = 'inverse temperature';
param(1).logpdf = @(x) sum(log(gampdf(x,gamma_bounds(1),gamma_bounds(2))));  % log density function for prior
param(1).lb = 0;    % lower bound
param(1).ub = 50;   % upper bound

for i = 1:numAtts
    param(i+1).name = strcat('weight',string(i));
    param(i+1).logpdf = @(x) sum(log(normpdf(x,weight_params(1),weight_params(2))));  % log density function for prior
    param(i+1).lb = -5;    % lower bound
    param(i+1).ub = 5;   % upper bound
end

nstarts = 5;

% fit WAD data with WAD model
results_WAD_WAD = mfit_optimize_parallel(@getLogLik_WAD,param,data_WAD,nstarts);

% fit WAD data with WP model
results_WAD_WP = mfit_optimize_parallel(@getLogLik_WP,param,data_WAD,nstarts);

% fit WAD data with EW model

% generate all possible signed weights
% ind = 1;
% all_signed_weights = zeros(2^numAtts - 1,numAtts);
% for i = 1:numAtts
%     which_neg = nchoosek(1:numAtts, i); % choose i of the attributes to be positive; get all ways of doing this
%     for j = 1:size(which_neg,1)
%         weights_temp = ones(numAtts,1);
%         weights_temp(which_neg(j,:)) = -1;
% 
%         all_signed_weights(ind, :) = weights_temp';
%         ind = ind + 1;
%     end
% end
% 
% ind = 1;
% clear results_WAD_EW_all
% num_weight_combos = size(all_signed_weights,1);
% logposts = zeros(length(data_WAD), num_weight_combos);
% parfor s = 1:length(data_WAD)
%     post = @(x) -mfit_post(x,param,data_WAD(s),@getLogLik_WAD);
% 
%     for weights_ind = 1:num_weight_combos
%         tic
%         logposts(s, weights_ind) = post([5 all_signed_weights(weights_ind, :)]);
%         toc
%     end
% 
%     %results_WAD_EW_all(ind) = mfit_optimize_parallel(@(x,d) getLogLik_WAD([x weights_temp'],d),param(1),data_WAD,1);    
% end

%opts = optimoptions('ga','PlotFcn',@gaplotbestf);
lbs = [param(1).lb repelem(-1, numAtts)];
ubs = [param(1).ub repelem(1, numAtts)];
numSubj = length(data_WAD);
logpost = zeros(numSubj, 1);
loglik = zeros(numSubj, 1);
hessians = cell(numSubj, 1);
best_fit_params = zeros(numSubj, numParams);
BICs = zeros(numSubj, 1);
AICs = zeros(numSubj, 1);
for s = 1:1
    EW_post = @(x) getLogLik_WAD(x,data_WAD(s)) + param(1).logpdf(x(1)) + log((1/3) ^ numAtts);
    [x,fval,exitflag] = ga(EW_post, ...
        numParams,[],[],[],[],...
        lbs,ubs,[],2:numParams);
    best_fit_params(s,:) = x;
    loglik(s) = getLogLik_WAD(x, data_WAD(s));
    logpost(s) = fval;

    BICs(s) = numParams*log(data_WAD(s).N) - 2*loglik(s);
    AICs(s) = numParams*2 - 2*loglik(s);
end

results_WAD_EW = results_WAD_WAD;
results_WAD_EW.logpost = best_logpost;
results_WAD_EW.loglik = loglik;
results_WAD_EW.H = hessians;
results_WAD_EW.x = best_fit_params;
results_WAD_EW.K = numParams;
results_WAD_EW.S = numSubj;
results_WAD_EW.bic = BICs;
results_WAD_EW.aic = AICs;

% fit WAD data with LEX model

% ind = 1;
% clear results_WAD_LEX_all;
% num_weights_lex = numAtts * 2;
% weights_lex_all = zeros(num_weights_lex, numAtts);
% for att = 1:numAtts
%     for att_sign = [-1, 1]
%         weights_lex_all(ind, att) = att_sign;
%         results_WAD_LEX_all(ind) = mfit_optimize_parallel(@(x,d) getLogLik_WAD([x weights_lex_all(ind,:)],d),param(1),data_WAD,1);
%         ind = ind + 1;
%     end
% end
% 
% numSubj = length(data_WAD);
% best_logpost = -Inf * ones(numSubj, 1);
% best_ind = zeros(numSubj, 1);
% loglik = zeros(numSubj, 1);
% hessians = cell(numSubj, 1);
% best_fit_params = zeros(numSubj, numParams);
% BICs = zeros(numSubj, 1);
% AICs = zeros(numSubj, 1);
% LMEs = zeros(numSubj, 1);
% nContFreeParams = 1;
% for s = 1:numSubj
%     % get best fit per subject
%     all_logpost = zeros(num_weights_lex,1);
%     all_hessian = cell(num_weights_lex,1);
% 
%     for i = 1:length(results_WAD_LEX_all)
%         temp = results_WAD_LEX_all(i);
%         if temp.logpost(s) > best_logpost(s)
%             best_ind(s) = i;
%             best_logpost(s) = temp.logpost(s);
%         end
% 
%         all_logpost(i) = temp.logpost(s);
%         all_hessian(i) = temp.H(s);
%     end
% 
%     best_logpost(s) = results_WAD_LEX_all(best_ind(s)).logpost(s);
%     loglik(s) = results_WAD_LEX_all(best_ind(s)).loglik(s);
%     hessians(s) = results_WAD_LEX_all(best_ind(s)).H(s);
%     best_fit_params(s,:) = [results_WAD_LEX_all(best_ind(s)).x(s) weights_lex_all(best_ind(s),:)];
% 
%     BICs(s) = numParams*log(data_WAD(s).N) - 2*loglik(s);
%     AICs(s) = numParams*2 - 2*loglik(s);
%     LMEs(s) = log((2*pi)^(nContFreeParams / 2) * sum(exp(all_logpost) .* (cellfun(@det, all_hessian) .^ (-1/2))));
% end
% 
% results_WAD_LEX = results_WAD_WAD;
% results_WAD_LEX.logpost = best_logpost;
% results_WAD_LEX.loglik = loglik;
% results_WAD_LEX.H = hessians;
% results_WAD_LEX.x = best_fit_params;
% results_WAD_LEX.K = numParams;
% results_WAD_LEX.S = numSubj;
% results_WAD_LEX.bic = BICs;
% results_WAD_LEX.aic = AICs;
% results_WAD_LEX.lme = LMEs;

lbs = [param(1).lb repelem(-1, numAtts)];
ubs = [param(1).ub repelem(1, numAtts)];
numSubj = length(data_WAD);
logpost = zeros(numSubj, 1);
loglik = zeros(numSubj, 1);
hessians = cell(numSubj, 1);
best_fit_params = zeros(numSubj, numParams);
BICs = zeros(numSubj, 1);
AICs = zeros(numSubj, 1);
for s = 1:1
    LEX_post = @(x) getLogLik_WAD(x,data_WAD(s)) + param(1).logpdf(x(1)) + log(1 / (numAtts * 2));
    [x,fval,exitflag] = ga(LEX_post, ...
        numParams,[],[],[],[],...
        lbs,ubs,[],2:numParams);
    best_fit_params(s,:) = x;
    loglik(s) = getLogLik_WAD(x, data_WAD(s));
    logpost(s) = fval;

    BICs(s) = numParams*log(data_WAD(s).N) - 2*loglik(s);
    AICs(s) = numParams*2 - 2*loglik(s);
end

results_WAD_LEX = results_WAD_WAD;
results_WAD_LEX.logpost = best_logpost;
results_WAD_LEX.loglik = loglik;
results_WAD_LEX.H = hessians;
results_WAD_LEX.x = best_fit_params;
results_WAD_LEX.K = numParams;
results_WAD_LEX.S = numSubj;
results_WAD_LEX.bic = BICs;
results_WAD_LEX.aic = AICs;

% fit WP data with WAD model
results_WP_WAD = mfit_optimize_parallel(@getLogLik_WAD,param,data_WP,nstarts);

% fit WP data with WP model
results_WP_WP = mfit_optimize_parallel(@getLogLik_WP,param,data_WP,nstarts);

save('fitting.mat');

%% Test fits
cors_WAD = zeros(numParams,1);
for i = 1:numParams
    cors_WAD(i) = corr(params_WAD(:,i), results_WAD_WAD.x(:,i));
end
cors_WAD(1)
mean(cors_WAD(2:end))

cors_WP = zeros(numParams,1);
for i = 1:numParams
    cors_WP(i) = corr(params_WP(:,i), results_WP_WP.x(:,i));
end
cors_WP(1)
mean(cors_WP(2:end))

bms_results_WAD = mfit_bms([results_WAD_WAD, results_WAD_WP]);
bms_results_WAD.pxp
bms_results_WP = mfit_bms([results_WP_WAD, results_WP_WP]);
bms_results_WP.pxp