function [loglik] = getLogLik(parameters, data, sign_optionvals)

% parameters: invTemp weights
% data: structure with .choices and .options

%options = signOptions(data.options, sign_optionvals, false);
options = reshape(data.options(:,1,:), size(data.options,1),size(data.options,3));
if sign_optionvals, options = sign(options); end
choices = data.choices;
inv_temp = parameters(:,1);
weights = parameters(:,2:end);

utilities = inv_temp .* (weights * options);

utilities_num = utilities;
utilities_num(:,choices == 2) = 0;
loglik_denom = log(exp(utilities) + 1);
loglik_denom_isinf = isinf(loglik_denom);
loglik_denom(loglik_denom_isinf) = utilities(loglik_denom_isinf);
combined = utilities_num - loglik_denom;
%loglik = sum(combined,2);
loglik = mean(combined > log(.5), 2); % UNCOMMENT THIS IF YOU WANT ROUNDED CV