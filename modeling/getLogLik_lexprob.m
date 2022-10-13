function [loglik] = getLogLik(parameters, data, sign_optionvals)

% parameters: invTemp weights
% data: structure with .choices and .options

%options = signOptions(data.options, sign_optionvals, false);
options = reshape(data.options(:,1,:), size(data.options,1),size(data.options,3));
if sign_optionvals, options = sign(options); end
choices = data.choices;
inv_temp = parameters(:,1);
weights = parameters(:,2:end);
numPop = size(parameters,1);
numChoices = size(options,2);

utilities = exp(inv_temp .* options);
utilities(isinf(utilities)) = realmax - 1
utilities_num = utilities;
utilities_num(:, choices == 2) = 1;
combined = weights * (utilities_num ./ (utilities + 1));
loglik = sum(log(combined,2));

%loglik = mean(combined > .5, 2); % UNCOMMENT THIS IF YOU WANT ROUNDED CV
