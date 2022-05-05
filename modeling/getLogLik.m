function [loglik] = getLogLik(parameters, data, sign_optionvals)

% parameters: invTemp weights
% data: structure with .choices and .options

%options = signOptions(data.options, sign_optionvals, false);
options = reshape(data.options(:,1,:), size(data.options,1),size(data.options,3));
if sign_optionvals, options = sign(options); end
choices = data.choices;
%options_dim = size(options);
%numAtts = options_dim(1);
%numChoices = options_dim(3);
% if isfield(data, 'avail_atts')
%     avail_atts = logical(data.avail_atts);
% else
%     avail_atts = true(numChoices, numAtts);
% end
%loglik = 0;
inv_temp = parameters(:,1);
weights = parameters(:,2:end);

utilities = inv_temp .* (weights * options);

utilities_num = utilities;
utilities_num(:,choices == 2) = 0; % why is this multi-dimensional? O_O
loglik_denom = log(exp(utilities) + 1);
loglik_denom_isinf = isinf(loglik_denom);
loglik_denom(loglik_denom_isinf) = utilities(loglik_denom_isinf);
loglik = sum(utilities_num - loglik_denom,2);
%loglik = mean(utilities_num >= 0); % UNCOMMENT THIS IF YOU WANT ROUNDED CV

% for i = 1:numChoices
%     %utilities = weights(avail_atts(i,:)) * options(avail_atts(i,:),:,i);
%     utilities = weights * options(:,:,i);
%     loglik = loglik + inv_temp * utilities(choices(i)) - logsumexp(inv_temp * utilities, 2);
% end