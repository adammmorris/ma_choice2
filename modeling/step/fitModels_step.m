function [results_WADs, results_WPs, results_EWs, results_TALs] = ...
    fitModels_step(param_struct_step, param_struct_signedweights_step, data, which_models)%, fittedpars, truepars, probs_gen)

if nargin < 4, which_models = {'WAD', 'WP', 'EW', 'TAL'}; end

numSubj = length(data);
hessians = cell(numSubj, 1);
numParams = length(param_struct_step);

logpost_WAD = zeros(numSubj, 1);
loglik_WAD = zeros(numSubj, 1);
best_fit_params_WAD = zeros(numSubj, numParams);
BICs_WAD = zeros(numSubj, 1);
AICs_WAD = zeros(numSubj, 1);

logpost_WP = zeros(numSubj, 1);
loglik_WP = zeros(numSubj, 1);
best_fit_params_WP = zeros(numSubj, numParams);
BICs_WP = zeros(numSubj, 1);
AICs_WP = zeros(numSubj, 1);

logpost_EW = zeros(numSubj, 1);
loglik_EW = zeros(numSubj, 1);
best_fit_params_EW = zeros(numSubj, numParams);
BICs_EW = zeros(numSubj, 1);
AICs_EW = zeros(numSubj, 1);

logpost_TAL = zeros(numSubj, 1);
loglik_TAL = zeros(numSubj, 1);
best_fit_params_TAL = zeros(numSubj, numParams);
BICs_TAL = zeros(numSubj, 1);
AICs_TAL = zeros(numSubj, 1);
 
lik = @(x,d) getLogLik_step(x, d, false);
lik_signedopts = @(x,d) getLogLik_step(x, d, true);

parfor s = 1:numSubj
        %chance = fittedpars; chance(1) = 0; chance2 = fittedpars; chance2(17:31) = 0;
        %[~, probs_ll] = getLogLik_step(truepars, data(s), false);
        %[~, probs_ll_fitted] = getLogLik_step(fittedpars, data(s), false);
        %[~, probs_choice] = makeChoice(truepars, data(s).options, data(s).avail_atts);
        %for i = 1:length(probs_ll), prob_real(i) = probs_ll(i,data(s).choices(i)); end
        %for i = 1:length(probs_ll), prob_real_fitted(i) = probs_ll_fitted(i,data(s).choices(i)); end

    if any(strcmp(which_models, 'WAD'))
        % WAD
        disp(['Fitting WAD_step for subject ', num2str(s)]);

        WAD_post = @(x) -(lik(x,data(s)) + getPriorSum(x, param_struct_step));
        [x,logpost] = ga(WAD_post, numParams,[],[],[],[],vertcat(param_struct_step.lb),...
            vertcat(param_struct_step.ub),[],find(vertcat(param_struct_step.int)));
        best_fit_params_WAD(s,:) = x;
        loglik_WAD(s) = lik(x, data(s));
        logpost_WAD(s) = -logpost;
        BICs_WAD(s) = numParams*log(data(s).N) - 2*loglik_WAD(s);
        AICs_WAD(s) = numParams*2 - 2*loglik_WAD(s);

        disp(['Completed optimization for WAD_step for subject ', num2str(s)]);
    end

    % WP
    if any(strcmp(which_models, 'WP'))
        disp(['Fitting WP_step for subject ', num2str(s)]);

        WP_post = @(x) -(lik_signedopts(x,data(s)) + getPriorSum(x, param_struct_step));
        [x,logpost] = ga(WP_post, numParams,[],[],[],[],vertcat(param_struct_step.lb),...
            vertcat(param_struct_step.ub),[],find(vertcat(param_struct_step.int)));
        best_fit_params_WP(s,:) = x;
        loglik_WP(s) = lik_signedopts(x, data(s));
        logpost_WP(s) = -logpost;
        BICs_WP(s) = numParams*log(data(s).N) - 2*loglik_WP(s);
        AICs_WP(s) = numParams*2 - 2*loglik_WP(s);

        disp(['Completed optimization for WP_step for subject ', num2str(s)]);
    end

    % EW
    if any(strcmp(which_models, 'EW'))
        disp(['Fitting EW_step for subject ', num2str(s)]);

        EW_post = @(x) -(lik(x,data(s)) + getPriorSum(x, param_struct_signedweights_step));
        [x,logpost] = ga(EW_post, numParams,[],[],[],[],vertcat(param_struct_signedweights_step.lb),...
            vertcat(param_struct_signedweights_step.ub),[],find(vertcat(param_struct_signedweights_step.int)));
        best_fit_params_EW(s,:) = x;
        loglik_EW(s) = lik(x, data(s));
        logpost_EW(s) = -logpost;
        BICs_EW(s) = numParams*log(data(s).N) - 2*loglik_EW(s);
        AICs_EW(s) = numParams*2 - 2*loglik_EW(s);

        disp(['Completed optimization for EW_step for subject ', num2str(s)]);
    end

    % TAL
    if any(strcmp(which_models, 'TAL'))
        disp(['Fitting TAL_step for subject ', num2str(s)]);

        TAL_post = @(x) -(lik_signedopts(x,data(s)) + getPriorSum(x, param_struct_signedweights_step));
        [x,logpost] = ga(TAL_post, numParams,[],[],[],[],vertcat(param_struct_signedweights_step.lb),...
            vertcat(param_struct_signedweights_step.ub),[],find(vertcat(param_struct_signedweights_step.int)));
        best_fit_params_TAL(s,:) = x;
        loglik_TAL(s) = lik_signedopts(x, data(s));
        logpost_TAL(s) = -logpost;
        BICs_TAL(s) = numParams*log(data(s).N) - 2*loglik_TAL(s);
        AICs_TAL(s) = numParams*2 - 2*loglik_TAL(s);
    end
end

results_template.K = numParams;
results_template.S = numSubj;
results_template.param = param_struct_step;
results_template.H = hessians;

results_WADs = results_template;
results_WADs.logpost = logpost_WAD';
results_WADs.loglik = loglik_WAD;
results_WADs.x = best_fit_params_WAD;
results_WADs.bic = BICs_WAD;
results_WADs.aic = AICs_WAD;
results_WADs.likfun = lik;

results_WPs = results_template;
results_WPs.logpost = logpost_WP';
results_WPs.loglik = loglik_WP;
results_WPs.x = best_fit_params_WP;
results_WPs.bic = BICs_WP;
results_WPs.aic = AICs_WP;
results_WPs.likfun = lik_signedopts;

results_EWs = results_template;
results_EWs.logpost = logpost_EW';
results_EWs.loglik = loglik_EW;
results_EWs.x = best_fit_params_EW;
results_EWs.bic = BICs_EW;
results_EWs.aic = AICs_EW;
results_EWs.likfun = lik;

results_TALs = results_template;
results_TALs.logpost = logpost_TAL';
results_TALs.loglik = loglik_TAL;
results_TALs.x = best_fit_params_TAL;
results_TALs.bic = BICs_TAL;
results_TALs.aic = AICs_TAL;
results_TALs.likfun = lik_signedopts;

end