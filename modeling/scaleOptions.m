function [options_signed] = scaleOptions(options)
% options should be numAtts x 2 x numChoices x numAgents
[numAtts, ~, numChoices, numAgents] = size(options);
options_signed = options;
for i = 1:numAgents
    for k = 1:numAtts
        options_signed(k,1,:,i) = rescale(options_signed(k,1,:,i), -1, 1);
    end
end
end