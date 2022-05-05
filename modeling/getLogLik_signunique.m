function [loglik] = getLogLik_signunique(parameters, data)

% parameters: invTemp weights
% data: structure with .choices and .options

options = signOptions(data.options, false, false);
choices = data.choices;
options_dim = size(options);
numAtts = options_dim(1);
numChoices = options_dim(3);
if isfield(data, 'avail_atts')
    avail_atts = logical(data.avail_atts);
else
    avail_atts = true(numChoices, numAtts);
end
loglik = 0;
inv_temp = parameters(1);
weights = parameters(2:(numAtts+1));
sign_atts = parameters((numAtts+2):end);

for i = 1:numChoices
    utilities = weights(avail_atts(i,:)) * (sign_atts' .* sign(options(avail_atts(i,:),:,i)) + ~sign_atts' .* options(avail_atts(i,:),:,i));
    loglik = loglik + inv_temp * utilities(choices(i)) - logsumexp(inv_temp * utilities, 2);
end

end