function [choices] = makeChoice(parameters, options, avail_atts, sign_optionvals)

% parameters: [invTemp weights steps sign]
% options: numAtt x numOptionsPerChoice x numChoices
% avail_atts: numChoices x numAtt (logicals)
options = signOptions(options, sign_optionvals, false);
options_dim = size(options);
numAtts = options_dim(1);
numChoices = options_dim(3);
numParams = size(parameters, 2);

if nargin < 3 || isempty(avail_atts); avail_atts = true(numChoices, numAtts); end
choices = zeros(options_dim(3), 1);
inv_temp = parameters(1);
weights = parameters(2:(numAtts+1));

if numParams == (numAtts + 1)
    % compute utilities & make choices
    for i = 1:numChoices
        utilities = weights(avail_atts(i,:)) * options(avail_atts(i,:),:,i);
        probs = exp(inv_temp * utilities - logsumexp(inv_temp * utilities, 2));
        choices(i) = fastrandsample(probs);
    end
else
    error('Unexpected number of parameters.');
end

end