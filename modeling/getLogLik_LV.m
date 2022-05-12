function [loglik] = getLogLik_LV(parameters, data)

% parameters: invTemp weights
% data: structure with .choices and .options

%options = signOptions(data.options, sign_optionvals, false);
options = reshape(var(data.options), size(data.options,2),size(data.options,3));
options = sign(options(1,:) - options(2,:));
choices = data.choices;
inv_temp = parameters(:,1);

utilities = inv_temp .* options;

utilities_num = utilities;
utilities_num(:,choices == 2) = 0; % why is this multi-dimensional? O_O
loglik_denom = log(exp(utilities) + 1);
loglik_denom_isinf = isinf(loglik_denom);
loglik_denom(loglik_denom_isinf) = utilities(loglik_denom_isinf);
loglik = sum(utilities_num - loglik_denom,2);
%loglik = mean(utilities_num >= 0); % UNCOMMENT THIS IF YOU WANT ROUNDED CV