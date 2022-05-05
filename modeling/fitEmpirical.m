%% setup
clear
datapath = '../data/v3_nosteps/real1/';

addpath("mfit/");
load(strcat(datapath, 'imported_data.mat'));
load('param_structs.mat');

%% fit
models_to_fit = 1:5;
version = '';

results = fitModels(param_structs(models_to_fit), data_real_scaled);
save(strcat(datapath, 'fitting_results', version, '.mat'), 'results');

for i = 1:length(models_to_fit)
    writematrix(results(i).x(:,2:end), strcat(datapath, 'fitted_empirical_weights_', model_names{models_to_fit(i)} ,'.csv'))
    writematrix(results(i).x(:,1), strcat(datapath, 'fitted_empirical_temps_', model_names{models_to_fit(i)} ,'.csv'))
end

%% do cross-validation

numFolds = 10;
numModels = length(models_to_fit);

for subj = 1:numSubj
    subj_cv_info(subj).numChoices = data_real_scaled(subj).N;
    subj_cv_info(subj).numTrainTrials = ceil(subj_cv_info(subj).numChoices - subj_cv_info(subj).numChoices / numFolds);
    subj_cv_info(subj).numTestTrials = floor(subj_cv_info(subj).numChoices / numFolds);
    subj_cv_info(subj).choices_unshuffled = 1:subj_cv_info(subj).numChoices;
    subj_cv_info(subj).choices_shuffled = subj_cv_info(subj).choices_unshuffled(randperm(subj_cv_info(subj).numChoices));
end

for i = 1:numFolds
    traindata = struct();
    testdata = struct();

    for subj = 1:numSubj
        test_trials = subj_cv_info(subj).choices_shuffled((subj_cv_info(subj).numTestTrials*(i-1)+1):(subj_cv_info(subj).numTestTrials*i));
        train_trials = subj_cv_info(subj).choices_unshuffled;
        train_trials(test_trials) = [];

        traindata(subj).N = subj_cv_info(subj).numTrainTrials;
        traindata(subj).options = data_real_scaled(subj).options(:,:,train_trials);
        traindata(subj).avail_atts = data_real_scaled(subj).avail_atts(train_trials,:);
        traindata(subj).choices = data_real_scaled(subj).choices(train_trials,:);

        testdata(subj).N = subj_cv_info(subj).numTestTrials;
        testdata(subj).options = data_real_scaled(subj).options(:,:,test_trials);
        testdata(subj).avail_atts = data_real_scaled(subj).avail_atts(test_trials,:);
        testdata(subj).choices = data_real_scaled(subj).choices(test_trials,:);
    end

    folds(i).traindata = traindata;
    folds(i).testdata = testdata;
end

cv_results = zeros(numSubj, numModels, numFolds);
results_train = cell(numFolds);
for i = 1:numFolds
    results_train{i} = fitModels(param_structs(models_to_fit), folds(i).traindata);
    cv_results(:,:,i) = crossValidateModels(folds(i).testdata, results_train{i});
end

% debugging
% traindata_debug = folds(2).traindata(14);
% testdata_debug = folds(2).testdata(14);
% results_train_debug = fitModels(param_structs(3), traindata_debug);
% results_test_debug = crossValidateModels(testdata_debug, results_train_debug);
%  

cv_results_avg = mean(cv_results,3);
mean(mean(cv_results,3))
median(mean(cv_results,3))

best_model = zeros(numSubj, 1);
worst_model = zeros(numSubj, 1);
for i = 1:numSubj
    [~, best_model(i)] = max(cv_results_avg(i,:));
    [~, worst_model(i)] = min(cv_results_avg(i,:));
    %best_model(i) = find(abs(cv_results_avg(i,:) - cv_results_avg(i,best_model(i))) < 1e-8,1,'last');
end
graphCV(cv_results_avg, [1 3]);

hist(best_model)
hist(worst_model)

save(strcat(datapath,'cv_results', version, '.mat'));

%% normalize
cv_results_normalized = zeros(numSubj, numModels);
cv_results_best = zeros(numSubj, 5);
cv_results_nonnormalized = zeros(numSubj, numModels);
for subj = 1:numSubj
    best = cv_results_avg(subj,best_model(subj));
    worst = cv_results_avg(subj,worst_model(subj));
    numTestTrials = subj_cv_info(subj).numTestTrials;
    best_exp = exp(best / numTestTrials);
    worst_exp = exp(worst / numTestTrials);
    chance = log(.5 ^ numTestTrials);
    %cv_results_normalized(subj,:) = (cv_results_avg(subj,:) - chance) / (best - chance);
    %cv_results_normalized(subj,:) = best ./ cv_results_avg(subj,:);
    cv_results_nonnormalized(subj,:) = exp(cv_results_avg(subj,:) / numTestTrials); 
    cv_results_normalized(subj,:) = (exp(cv_results_avg(subj,:) / numTestTrials) - worst_exp) / (best_exp - worst_exp); 
    cv_results_best(subj,:) = [best, best < chance, best_exp, exp(chance / numTestTrials), numTestTrials];
end

writematrix(cv_results_nonnormalized, strcat(datapath, 'cv_results', version, '.csv'));
writematrix(cv_results_normalized, strcat(datapath, 'cv_results_normalized', version, '.csv'));
writematrix(cv_results_best, strcat(datapath, 'cv_results_best', version, '.csv'));

%% do rounded
% switch getLogLik first.. janky, I know

cv_results_rounded = zeros(numSubj, numModels, numFolds);
for i = 1:numFolds
   cv_results_rounded(:,:,i) = crossValidateModels(folds(i).testdata, results_train{i});
end
cv_results_rounded_avg = mean(cv_results_rounded, 3);
writematrix(cv_results_rounded_avg, strcat(datapath, 'cv_results_rounded', version, '.csv'));