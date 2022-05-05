function [options_signed] = signOptions(options, do_sign, do_scale)
% options should be numAtts x 2 x numChoices x numAgents
if nargin < 3, do_scale = true; end
if nargin < 2, do_sign = true; end
[numAtts, ~, numChoices, numAgents] = size(options);
options_signed = zeros(numAtts, 2, numChoices, numAgents);
for i = 1:numAgents
    for j = 1:numChoices
        options_signed(:,1,j,i) = options(:,1,j,i) - options(:,2,j,i);
        if do_sign
            options_signed(:,1,j,i) = sign(options_signed(:,1,j,i));
        end
    end

    for k = 1:numAtts
        if do_scale
            options_signed(k,1,:,i) = rescale(options_signed(k,1,:,i), -1, 1);
        end
    end
end
end