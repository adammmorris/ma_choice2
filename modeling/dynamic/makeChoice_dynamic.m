function [choices, probs_out] = makeChoice(parameters, options, avail_atts)

% parameters: [invTemp weights steps sign]
% options: numAtt x numOptionsPerChoice x numChoices
% avail_atts: numChoices x numAtt (logicals)

options_dim = size(options);
numAtts = options_dim(1);
numChoices = options_dim(3);
numParams = size(parameters, 2);

if nargin < 3 || isempty(avail_atts); avail_atts = true(numChoices, numAtts); end
choices = zeros(options_dim(3), 1);
inv_temp = parameters(1);
weights = parameters(2:(numAtts+1));

if numParams == (numAtts + 1) % non-step
    % compute utilities & make choices
    for i = 1:numChoices
        utilities = weights(avail_atts(i,:)) * options(avail_atts(i,:),:,i);
        probs = exp(inv_temp * utilities - logsumexp(inv_temp * utilities, 2));
        choices(i) = fastrandsample(probs);
    end
elseif numParams == (numAtts * 2 + 1) % step
    steps = parameters((numAtts+2):(numAtts*2+1));

    numSteps = max(steps);

    % compute utilities & make choices
    for i = 1:numChoices
        step = 1;
        while true
            % try to make a decision based on selected attributes
            which_atts = avail_atts(i,:) & (steps == step);

            % if there are attributes available...
            if any(which_atts)
                utilities = weights(which_atts) * options(which_atts,:,i);
                probs = exp(inv_temp * utilities - logsumexp(inv_temp * utilities, 2));
                probs_out(i,:) = probs;

                % if they're decisive...
                if any(utilities ~= utilities(1))
                    choices(i) = fastrandsample(probs);
                    break;
                end
            end

            % if we didn't make a choice, are there more steps?
            if step < numSteps, step = step + 1; % if so, increment
            else % if not, make a random choice and break
                choices(i) = fastrandsample(.5 * ones(1,options_dim(2)));
                probs_out(i,:) = [.5 .5];
                break;
            end
        end
    end
else
    error('Unexpected number of parameters.');
end

end