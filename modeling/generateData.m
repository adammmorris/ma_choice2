function [data] = generateData(choiceFn, numChoices, numAtts, numValuesPerAtt, params)

numAgents = size(params,1);
options = randi([1 numValuesPerAtt], numAtts, 2, numChoices, numAgents) / numValuesPerAtt;
avail_atts = true(numChoices, numAtts, numAgents);

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
    [data(agent).choices] = choiceFn(data(agent).params, data(agent).options, data(agent).avail_atts);
end