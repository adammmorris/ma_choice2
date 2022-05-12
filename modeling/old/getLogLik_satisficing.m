function [loglik] = getLogLik_satisficing(parameters, data, sign_optionvals)

% parameters: invTemp weights
% data: structure with .choices and .options

%options = signOptions(data.options, sign_optionvals, false);
options = reshape(data.options(:,1,:), size(data.options,1),size(data.options,3));
if sign_optionvals, options = sign(options); end
choices = data.choices;
numAtts = size(options,1);
inv_temp = parameters(:,1);
weights = parameters(:,2:(numAtts+1));
threshold = parameters(:,3);

utilities = (weights * options) > threshold;

utilities_num = utilities;
utilities_num(:,choices == 2) = 0;
loglik_denom = log(exp(utilities) + 1);
loglik_denom_isinf = isinf(loglik_denom);
loglik_denom(loglik_denom_isinf) = utilities(loglik_denom_isinf);
loglik = sum(utilities_num - loglik_denom,2);
%loglik = mean(utilities_num >= 0); % UNCOMMENT THIS IF YOU WANT ROUNDED CV