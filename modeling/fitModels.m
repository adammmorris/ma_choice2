function [results] = fitModels(param_structs, data)

numSubj = length(data);
hessians = cell(numSubj, 1);

%if any(strcmp(which_models, 'WAD')), results_WAD = mfit_optimize_parallel(lik,param_struct,data,nstarts); end
%if any(strcmp(which_models, 'WP')), results_WP = mfit_optimize_parallel(lik_signedopts,param_struct,data,nstarts); end

numModels = length(param_structs);

for m = 1:numModels
    param_struct = param_structs{m};
    lik_fn = param_struct(1).lik;
    numParams = length(param_struct);

    %if ~any(vertcat(param_struct.int))
    if false
        disp(['Fitting model ', num2str(m),' for all subjects.']);
        results(m) = mfit_optimize_parallel(lik_fn,param_struct,data,5);
        disp(['Completed model ', num2str(m),' for all subjects.']);
    else
        logposts = zeros(numSubj, 1);
        logliks = zeros(numSubj, 1);
        best_fit_params = zeros(numSubj, length(param_struct));
        BICs = zeros(numSubj, 1);
        AICs = zeros(numSubj, 1);
    
        parfor s = 1:numSubj
            disp(['Fitting model ', num2str(m),' for subject ', num2str(s)]);
            
            post_fn = @(x) -(lik_fn(x,data(s)) + getPriorSum(x, param_struct));
            [x,logpost] = ga(post_fn, length(param_struct),[],[],[],[],vertcat(param_struct.lb), ...
                vertcat(param_struct.ub),[],find(vertcat(param_struct.int)), ...
                optimoptions('ga','UseVectorized', true, ...
                'MaxStallGenerations', 200, 'MaxGenerations', 5000));
            best_fit_params(s,:) = x;
            logliks(s) = lik_fn(x, data(s));
            logposts(s) = -logpost;
            BICs(s) = numParams*log(data(s).N) - 2*logliks(s);
            AICs(s) = numParams*2 - 2*logliks(s);
    
            disp(['Completed model ', num2str(m),' for subject ', num2str(s)]);
        end
        
        results(m).K = length(param_struct);
        results(m).S = numSubj;
        results(m).H = hessians;
        results(m).logpost = logposts';
        results(m).loglik = logliks;
        results(m).x = best_fit_params;
        results(m).bic = BICs;
        results(m).aic = AICs;
        results(m).likfun = lik_fn;
        results(m).param = param_struct;
        results(m).latents = [];
    end
end

end