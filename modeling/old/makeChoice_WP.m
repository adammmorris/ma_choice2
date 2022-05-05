function choices = makeChoice_WP(options_signed, weights, inv_temp)
% weights: numAtt x 1
% options_signed: numAtt x numChoices

[~, numChoices] = size(options_signed);

choices = zeros(numChoices, 1);

% compute utilities & make choices
for i = 1:numChoices
    utilities = [weights' * options_signed(:,i), 0];
    probs = exp(inv_temp * utilities - logsumexp(inv_temp * utilities, 2));
    choices(i) = fastrandsample(probs);
end

end