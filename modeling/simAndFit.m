%% SIMULATES AGENTS FROM EACH MODEL MAKING CHOICES & THEN FITS THE MODELS TO THEM

%% setup
clear
addpath("mfit/");
load('param_structs_9att.mat');

numAgents = 100;
numAtts = 9;
numChoices = 100;
numValuesPerAtt = 5;

version = '9att_100_5fold';

%% generate agent parameters
weight_params = [0 1];
weights = normrnd(weight_params(1),weight_params(2),numAgents,numAtts);
weights_signed = sign(weights);

gamma_bounds = [4 1];
inv_temp = gamrnd(gamma_bounds(1),gamma_bounds(2),numAgents,1);

for agent = 1:numAgents
    params_WAD(agent, :) = [inv_temp(agent) weights(agent,:)];
    params_WP(agent, :) = [inv_temp(agent) weights(agent,:)];
    params_EW(agent, :) = [inv_temp(agent) weights_signed(agent,:)];
    params_TAL(agent, :) = [inv_temp(agent) weights_signed(agent,:)];
    params_LEXNB(agent, :) = [inv_temp(agent) randi(numAtts) randi([0 1])];
    params_LEXB(agent, :) = [inv_temp(agent) randi(numAtts) randi([0 1])];
    %params_WAD_signunique(agent, :) = [inv_temp(agent) weights(agent,:) randi([0 1], 1, numAtts)];
end

params_all = {params_WAD, params_WP, params_EW, params_TAL, params_LEXNB, params_LEXB};

%% simulate training & test data
data_to_fit = 1:6;
numGeneratingModels = length(data_to_fit);

if length(params_all) == numGeneratingModels
    for i = 1:length(params_all)
        data_generated{i} = generateData(param_structs{i}(1).choicefn, numChoices, numAtts, numValuesPerAtt, params_all{i});
        %data_test{i} = generateData(param_structs{i}(1).choicefn, numChoices, numAtts, numValuesPerAtt, params_all{i});
    end
else
    error('Need parameters for each model');
end

%[data_WAD_signunique] = generateData(@makeChoice_signunique, numChoices, numAtts, numValuesPerAtt, params_WAD_signunique, false);
%[data_WAD_signunique_test] = generateData(@makeChoice_signunique, numChoices, numAtts, numValuesPerAtt, params_WAD_signunique, false);

%% fit models
models_to_fit = 1:6;
numFittingModels = length(models_to_fit);

for i = 1:length(data_to_fit)
    results{i} = fitModels(param_structs(models_to_fit), data_generated{data_to_fit(i)});
end

%results_WAD_signunique = fitModels({param_struct_WAD, param_struct_WP, param_struct_WADsignunique}, data_WAD_signunique);

%% Test parameter fits
which_fitted_model = 2;
true_params = params_all{models_to_fit(which_fitted_model)};
fitted_params = results{which_fitted_model}(which_fitted_model).x;
numParams = size(true_params,2);

cors = zeros(numParams,1);
mse = zeros(numParams,1);
pctequal = zeros(numParams,1);
for i = 1:numParams
    cors(i) = corr(true_params(:,i), fitted_params(:,i));
    mse(i) = mean((true_params(:,i) - fitted_params(:,i)) .^ 2);
    pctequal(i) = mean(abs(true_params(:,i) - fitted_params(:,i)) < .0001);
end

%cors(1)
%mean(cors(2:(numAtts+1)))
%mean(mse(1))
%mean(mse(2:end))
%mean(pctequal((numAtts+2):end))


%% Model selection via cross-validation
cv_matrix = zeros(numGeneratingModels,numFittingModels);

numFolds = 5;

for subj = 1:numAgents
    subj_cv_info(subj).numChoices = numChoices;
    subj_cv_info(subj).numTrainTrials = ceil(subj_cv_info(subj).numChoices - subj_cv_info(subj).numChoices / numFolds);
    subj_cv_info(subj).numTestTrials = floor(subj_cv_info(subj).numChoices / numFolds);
    subj_cv_info(subj).choices_unshuffled = 1:subj_cv_info(subj).numChoices;
    subj_cv_info(subj).choices_shuffled = subj_cv_info(subj).choices_unshuffled(randperm(subj_cv_info(subj).numChoices));
end

for data_generating_model = 1:numGeneratingModels
    cur_data = data_generated{data_to_fit(data_generating_model)};

    % conduct CVs
    %cv_temp = crossValidateModels(data_test{data_to_fit(data_generating_model)}, results{data_generating_model});

    for i = 1:numFolds
        traindata = struct();
        testdata = struct();

        for subj = 1:numAgents
            test_trials = subj_cv_info(subj).choices_shuffled((subj_cv_info(subj).numTestTrials*(i-1)+1):(subj_cv_info(subj).numTestTrials*i));
            train_trials = subj_cv_info(subj).choices_unshuffled;
            train_trials(test_trials) = [];

            traindata(subj).N = subj_cv_info(subj).numTrainTrials;
            traindata(subj).options = cur_data(subj).options(:,:,train_trials);
            traindata(subj).choices = cur_data(subj).choices(train_trials,:);

            testdata(subj).N = subj_cv_info(subj).numTestTrials;
            testdata(subj).options = cur_data(subj).options(:,:,test_trials);
            testdata(subj).choices = cur_data(subj).choices(test_trials,:);
        end

        folds(i).traindata = traindata;
        folds(i).testdata = testdata;
    end

    cv_results = zeros(numAgents, numFittingModels, numFolds);
    results_train = cell(numFolds,1);
    for i = 1:numFolds
        results_train{i} = fitModels(param_structs(models_to_fit), folds(i).traindata);
        cv_results(:,:,i) = crossValidateModels(folds(i).testdata, results_train{i});
    end

    cv_results_avg = mean(cv_results,3);

    % analyze cv results
    best_model = zeros(numAgents, 1);
    for agent = 1:numAgents
        [~, best_model(agent)] = max(cv_results_avg(agent, :));
    end

    for fitted_model = 1:numFittingModels
        cv_matrix(data_generating_model, fitted_model) = sum(best_model == models_to_fit(fitted_model));
    end
end

cv_matrix_normed1 = zeros(numGeneratingModels,numFittingModels); % prob(fitted models | true models)
cv_matrix_normed2 = zeros(numGeneratingModels,numFittingModels); % prob(true models | fitted models)
for i = 1:numGeneratingModels
    cv_matrix_normed1(i,:) = cv_matrix(i,:) / sum(cv_matrix(i,:));
end
for i = 1:numFittingModels
    cv_matrix_normed2(:,i) = cv_matrix(:,i) / sum(cv_matrix(:,i));
end

mean(diag(cv_matrix_normed2))

test = exp(cv_results_avg);
for i = 1:100
    test(i,:) = test(i,:) / sum(test(i,:));
end
mean(test)

writematrix(cv_matrix_normed2, ['normed_' version '.csv'])
save(['simulation_fitting_' version '.mat']);

%% debugging
%getLogLik(results{2}(2).x(10,:),data_test{2}(10),true)
%getLogLik(params_WP(10,:),data_test{2}(10),true)

% choice 11: 97% for left
%makeChoice(params_WP(10,:),data_test{2}(10).options,data_test{2}(10).avail_atts,true)

%crossValidateModels(folds(1).testdata(100),results_train{1}(6))