function [choices] = makeChoice(parameters, options, avail_atts)

% parameters: [invTemp weights steps sign]
% options: numAtt x numOptionsPerChoice x numChoices
% avail_atts: numChoices x numAtt (logicals)

options_dim = size(options);

if nargin < 3 || isempty(avail_atts); avail_atts = true(options_dim(3), options_dim(1)); end

numAtts = options_dim(1);
numChoices = options_dim(3);

choices = zeros(numChoices, 1);
inv_temp = parameters(1);
weights = parameters(2:(numAtts+1));
steps = parameters((numAtts+2):(numAtts*2+1));
signs_weights = parameters((numAtts*2+2):(numAtts*3+1));
signs_optionvals = parameters((numAtts*3+2):end);

numSteps = max(steps);

% compute utilities & make choices
for i = 1:numChoices
    step = 1;
    %choice_made = false;
    while true
        % try to make a decision based on selected attributes
        which_atts = avail_atts(i,:) & (steps == step);

        % if there are attributes available...
        if any(which_atts)
            if signs_weights(step), weights_to_use = sign(weights(which_atts));
            else, weights_to_use = weights(which_atts);
            end

            utilities = weights_to_use * signOptions(options(which_atts,:,i), signs_optionvals(step), false);
            probs = exp(inv_temp * utilities - logsumexp(inv_temp * utilities, 2));

            % if they're decisive...
            if any(probs ~= .5)
                choices(i) = fastrandsample(probs);
                %choice_made = true;
                break;
            end
        end

        % if we didn't make a choice, are there more steps?
        if step < numSteps, step = step + 1; % if so, increment
        else % if not, make a random choice and break
            choices(i) = fastrandsample(probs);
            break;
        end
    end
end

end