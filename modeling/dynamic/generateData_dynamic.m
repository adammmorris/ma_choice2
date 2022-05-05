function [data] = generateData_dynamic(numChoices, numAgents, numAtts, numValuesPerAtt, ...
    params, param_struct, param_struct_signedweights, param_struct_step, param_struct_signedweights_step, sign_optionvals)

numChoicesPerReset = 10;
lik = @(x,d) getLogLik(x, d, false);
lik_signedopts = @(x,d) getLogLik(x, d, true);
lik_step = @(x,d) getLogLik_step(x, d, false);
lik_signedopts_step = @(x,d) getLogLik_step(x, d, true);

for agent = 1:numAgents
    data(agent).N = numChoices;
    data(agent).params = params(agent,:);

    % generate first set of options
    options = signOptions(randi([1 numValuesPerAtt], numAtts, 2, numChoicesPerReset) / numValuesPerAtt, ...
        sign_optionvals, false);
    avail_atts = generateAvailAtts(numChoicesPerReset, numAtts, 1, 7);
    data(agent).options = options;
    data(agent).avail_atts = avail_atts;
    data(agent).choices = makeChoice(data(agent).params, options, avail_atts);

    for i = 2:(numChoices/numChoicesPerReset)
        % do fitting
        % WAD
        WAD_post = @(x) -(lik(x,data(agent)) + getPriorSum(x, param_struct));
        [results_WAD.x,~] = ga(WAD_post, length(param_struct),[],[],[],[],vertcat(param_struct.lb),...
            vertcat(param_struct.ub),[]);
        results_WAD.likfun = lik;
    
        % WP
        WP_post = @(x) -(lik_signedopts(x,data(agent)) + getPriorSum(x, param_struct));
        [results_WP.x,~] = ga(WP_post, length(param_struct),[],[],[],[],vertcat(param_struct.lb),...
            vertcat(param_struct.ub),[]);
        results_WP.likfun = lik_signedopts;

        % EW
        EW_post = @(x) -(lik(x,data(agent)) + getPriorSum(x, param_struct_signedweights));
        [results_EW.x,~] = ga(EW_post, length(param_struct_signedweights),[],[],[],[],vertcat(param_struct_signedweights.lb),...
            vertcat(param_struct_signedweights.ub),[],find(vertcat(param_struct_signedweights.int)));
        results_EW.likfun = lik;

        % TAL
        TAL_post = @(x) -(lik_signedopts(x,data(agent)) + getPriorSum(x, param_struct_signedweights));
        [results_TAL.x,~] = ga(TAL_post, length(param_struct_signedweights),[],[],[],[],vertcat(param_struct_signedweights.lb),...
            vertcat(param_struct_signedweights.ub),[],find(vertcat(param_struct_signedweights.int)));
        results_TAL.likfun = lik_signedopts;

        % WAD_step
        WAD_post_step = @(x) -(lik_step(x,data(agent)) + getPriorSum(x, param_struct_step));
        [results_WAD_step.x,~] = ga(WAD_post_step, length(param_struct_step),[],[],[],[],vertcat(param_struct_step.lb),...
            vertcat(param_struct_step.ub),[],find(vertcat(param_struct_step.int)));
        results_WAD_step.likfun = lik_step;
    
        % WP_step
        WP_post_step = @(x) -(lik_signedopts_step(x,data(agent)) + getPriorSum(x, param_struct));
        [results_WP_step.x,~] = ga(WP_post_step, length(param_struct),[],[],[],[],vertcat(param_struct.lb),...
            vertcat(param_struct.ub),[],find(vertcat(param_struct_step.int)));
        results_WP_step.likfun = lik_signedopts_step;

        % EW_step
        EW_post_step = @(x) -(lik_step(x,data(agent)) + getPriorSum(x, param_struct_signedweights_step));
        [results_EW_step.x,~] = ga(EW_post_step, length(param_struct_signedweights_step),[],[],[],[],vertcat(param_struct_signedweights_step.lb),...
            vertcat(param_struct_signedweights_step.ub),[],find(vertcat(param_struct_signedweights_step.int)));
        results_EW_step.likfun = lik_step;

        % TAL_step
        TAL_post_step = @(x) -(lik_signedopts_step(x,data(agent)) + getPriorSum(x, param_struct_signedweights_step));
        [results_TAL_step.x,~] = ga(TAL_post_step, length(param_struct_signedweights_step),[],[],[],[],vertcat(param_struct_signedweights_step.lb),...
            vertcat(param_struct_signedweights.ub),[],find(vertcat(param_struct_signedweights_step.int)));
        results_TAL_step.likfun = lik_signedopts_step;

        results = {results_WAD, results_WP, results_EW, results_TAL, ...
            results_WAD_step, results_WP_step, results_EW_step, results_TAL_step};

        % generate options
        to_optimize = @(x) -getPredictiveVariance(x, results);
        options = ga(to_optimize, numParams,[],[],[],[],lbs,ubs,[],whichInt);
        avail_atts = ...;

        % simulate choices
        %cur_choice_index = (1+numChoicesPerReset*(i-1)):(numChoicesPerReset*i);

        data(agent).N = numChoicesPerReset*i;
        data(agent).options = cat(3, data(agent).options, options);
        data(agent).avail_atts = cat(1, data(agent).avail_atts, avail_atts);
        data(agent).choices = cat(1, data(agent).choices, makeChoice(data(agent).params, options, avail_atts));
    end
end
end

function objective = getPredictiveVariance(opts,results)
data.options = reshape(opts); % DO THIS
data.choices = ones(size(data.options,3),1);
data.avail_atts = 1; % DO THIS

liks = zeros(length(liksfuns),1);
for i = 1:length(likfuns)
    liks(i) = results(i).likfun(results(i).x, data);
end
objective = std(liks);
end