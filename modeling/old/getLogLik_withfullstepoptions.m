function [loglik] = getLogLik(parameters, data)

% parameters: invTemp weights
% data: structure with .choices and .options

options = data.options;
choices = data.choices;
options_dim = size(options);
numAtts = options_dim(1);
numChoices = options_dim(3);
if isfield(data, 'avail_atts')
    avail_atts = data.avail_atts;
else
    avail_atts = true(numChoices, numAtts);
end
loglik = 0;
inv_temp = parameters(1);
weights = parameters(2:end);
steps = parameters((numAtts+2):(numAtts*2+1));
signs_weights = parameters((numAtts*2+2):(numAtts*3+1));
signs_optionvals = parameters((numAtts*3+2):end);

numSteps = max(steps);

for i = 1:numChoices
    step = 1;
    while true
        % try to make a decision based on selected attributes
        which_atts = avail_atts(i,:) & (steps == step);

        % if there are attributes available...
        if any(which_atts)
            if signs_weights(step), weights_to_use = sign(weights(which_atts));
            else, weights_to_use = weights(which_atts);
            end

            utilities = weights_to_use * signOptions(options(which_atts,:,i), signs_optionvals(step), false);

            % if they're decisive...
            if any(utilities ~= utilities(1))
                loglik = loglik + inv_temp * utilities(choices(i)) - logsumexp(inv_temp * utilities, 2);
                break;
            end
        end

        % if we didn't make a choice, are there more steps?
        if step < numSteps, step = step + 1; % if so, increment
        else % if not, make a random choice and break
            loglik = loglik + log(.5);
            break;
        end
    end
end

end