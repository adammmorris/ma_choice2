function choices = makeChoice_EW(options, weights_signed, inv_temp)
% weights: numAtt x 1
% options_signed: numAtt x numChoices

[~, numChoices] = size(options);

choices = zeros(numChoices, 1);

% compute utilities & make choices
for i = 1:numChoices
    utilities = [weights_signed' * options(:,i), 0];
    probs = exp(inv_temp * utilities - logsumexp(inv_temp * utilities, 2));
    choices(i) = fastrandsample(probs);
end

end