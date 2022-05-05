function [cv] = crossValidateModels(data, results)
numModels = length(results);
numSubj = length(data);
cv = zeros(numSubj,numModels);
for i = 1:numModels
    for s = 1:numSubj
        cv(s,i) = results(i).likfun(results(i).x(s,:),data(s));
    end
end