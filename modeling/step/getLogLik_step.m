function [loglik, probs] = getLogLik_step(parameters, data, sign_optionvals)

% parameters: invTemp weights
% data: structure with .choices and .options

options = signOptions(data.options, sign_optionvals, false);
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
steps = parameters((numAtts+2):(numAtts*2+1));

numSteps = max(steps);

for i = 1:numChoices
    step = 1;
    while true
        % try to make a decision based on selected attributes
        which_atts = avail_atts(i,:) & (steps == step);

        % if there are attributes available...
        if any(which_atts)
            utilities = weights(which_atts) * options(which_atts,:,i);
            %probs(i,:) = exp(inv_temp * utilities - logsumexp(inv_temp * utilities, 2));

            % if they're decisive...
            if any(utilities ~= utilities(1))
                loglik = loglik + inv_temp * utilities(choices(i)) - logsumexp(inv_temp * utilities, 2);
                break;
            end
        end

        % if we didn't make a choice, are there more steps?
        if step < numSteps, step = step + 1; % if so, increment
        else % if not, make a random choice and break
            %probs(i,:) = [.5 .5];
            loglik = loglik + log(.5);
            break;
        end
    end
end

end