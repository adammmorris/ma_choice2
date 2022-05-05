function [data] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, params, sign_optionvals)

options = randi([1 numValuesPerAtt], numAtts, 2, numChoices, numAgents) / numValuesPerAtt;
options = signOptions(options, sign_optionvals, false);
%avail_atts = logical(randi([0 1], numChoices, numAtts, numAgents));
avail_atts = true(numChoices, numAtts, numAgents);
%avail_atts = [ones(numChoices, floor(numAtts / 2), numAgents) zeros(numChoices, ceil(numAtts / 2), numAgents)];

shuffle = @(v) v(randperm(length(v)));
for i = 1:numAgents
    for j = 1:numChoices
        avail_atts(j,:,i) = shuffle(avail_atts(j,:,i));
    end
end

struct_template.N = numChoices;

for agent = 1:numAgents
    data(agent) = struct_template;
end

for agent = 1:numAgents
    data(agent).options = options(:,:,:,agent);
    data(agent).params = params(agent,:);
    data(agent).avail_atts = avail_atts(:,:,agent);
    [data(agent).choices] = makeChoice(data(agent).params, data(agent).options, data(agent).avail_atts);
end