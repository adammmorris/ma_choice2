function [loglik] = getLogLik_attpertrial(parameters, data, signed_attributes, which_att)

% parameters: invTemp weights
% data: structure with .choices and .options

%options = signOptions(data.options, sign_optionvals, false);
options = reshape(data.options(:,1,:), size(data.options,1),size(data.options,3));
if signed_attributes, options = sign(options); end
choices = data.choices;
inv_temp = parameters(:,1);
directions = parameters(:,2:end);
numAtts = size(options,1);
numTrials = size(options,2);

options_indexed = options(sub2ind(size(options), which_att', 1:numTrials));
test = zeros(numTrials,numAtts);
test(sub2ind([numTrials numAtts], 1:numTrials, which_att')) = 1;

utilities = inv_temp .* ((directions * test') .* options_indexed);

utilities_num = utilities;
utilities_num(:,choices == 2) = 0;
loglik_denom = log(exp(utilities) + 1);
loglik_denom_isinf = isinf(loglik_denom);
loglik_denom(loglik_denom_isinf) = utilities(loglik_denom_isinf);
loglik = sum(utilities_num - loglik_denom,2);
%loglik = mean(utilities_num >= 0); % UNCOMMENT THIS IF YOU WANT ROUNDED CV