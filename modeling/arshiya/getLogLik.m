function [loglik] = getLogLik(parameters, data)

% parameters: [invTemp weights]
% data: structure with .choices and .options

% get the task structure & choices
options = data.options;
choices = data.choices;
options_dim = size(options);
numChoices = options_dim(3);

% extract parameters
inv_temp = parameters(1);
weights = parameters(2:end);

% initialize log likelihood to zero
loglik = 0;

% loop through choices
for i = 1:numChoices
    % for the WAD model, we just multiply the weights by the options.
    utilities = weights * options(:,:,i);
    
    % to convert utilities -> probabilities, the model used a "softmax" (or
    % Boltzmann) function: e^(inverse_temperature * utility(chosen_option))
    % / sum_over_all_options(e^(inverse_temperature * utility(option))
    % so this formula gives us the likelihood of the person's choice, given
    % the utilities calculated above.
    % we want the LOG likelihood, so we take the log of that, which gives
    % us: inv_temp * utility(chosen_option) - log(sum(e^inverse_temperature
    % * utilities))). to compute that latter term efficiently, we use the
    % "logsumexp" function.
    loglik = loglik + inv_temp * utilities(choices(i)) - logsumexp(inv_temp * utilities, 2);
end

end