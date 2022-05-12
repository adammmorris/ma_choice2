function avail_atts = generateAvailAtts(numChoices, numAtts, numAgents, numAvailPerChoice)
shuffle = @(v) v(randperm(length(v)));
avail_atts = [ones(numChoices, numAvailPerChoice, numAgents) zeros(numChoices, numAtts - numAvailPerChoice, numAgents)];
for i = 1:numAgents
    for j = 1:numChoices
        avail_atts(j,:,i) = shuffle(avail_atts(j,:,i));
    end
end
end