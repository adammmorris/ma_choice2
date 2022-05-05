function [loglik] = getLogLik_WP(parameters, data)

% parameters: invTemp weights
% data: structure with .choices and .options

options = signOptions(data.options, true, true);
choices = data.choices;
options_dim = size(options);
if isfield(data, 'avail_atts')
    avail_atts = data.avail_atts;
else
    avail_atts = true(options_dim(3), options_dim(1));
end
loglik = 0;
inv_temp = parameters(1);
weights = parameters(2:end);

% compute utilities & make choices
for i = 1:options_dim(3)
    utilities = weights(avail_atts(i,:)) * options(avail_atts(i,:),:,i);
    loglik = loglik + inv_temp * utilities(choices(i)) - logsumexp(inv_temp * utilities, 2);
end

end