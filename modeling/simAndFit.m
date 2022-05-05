%% SIMULATES AGENTS FROM EACH MODEL MAKING CHOICES & THEN FITS THE MODELS TO THEM

%% setup
clear
addpath("mfit/");
load('param_structs.mat');

numAgents = 50;
numAtts = 10;
numChoices = 75;
numValuesPerAtt = 5;

%% generate agent parameters
weight_params = [0 1];
weights = normrnd(weight_params(1),weight_params(2),numAgents,numAtts);
weights_signed = sign(weights);

gamma_bounds = [1 5];
inv_temp = gamrnd(gamma_bounds(1),gamma_bounds(2),numAgents,1);

for agent = 1:numAgents
    params_WAD(agent, :) = [inv_temp(agent) weights(agent,:)];
    params_WP(agent, :) = [inv_temp(agent) weights(agent,:)];
    params_EW(agent, :) = [inv_temp(agent) weights_signed(agent,:)];
    params_TAL(agent, :) = [inv_temp(agent) weights_signed(agent,:)];
    params_LEX(agent, :) = [inv_temp(agent) randi(numAtts) randi([0 1])];
    %params_WAD_signunique(agent, :) = [inv_temp(agent) weights(agent,:) randi([0 1], 1, numAtts)];
end

params_all = {params_WAD, params_WP, params_EW, params_TAL, params_LEX};

%% simulate training & test data
for i = 1:length(param_structs)
    data_train{i} = generateData(param_structs{i}(1).choicefn, numChoices, numAtts, numValuesPerAtt, params_all{i});
    data_test{i} = generateData(param_structs{i}(1).choicefn, numChoices, numAtts, numValuesPerAtt, params_all{i});
end

%[data_WAD_signunique] = generateData(@makeChoice_signunique, numChoices, numAtts, numValuesPerAtt, params_WAD_signunique, false);
%[data_WAD_signunique_test] = generateData(@makeChoice_signunique, numChoices, numAtts, numValuesPerAtt, params_WAD_signunique, false);

%% fit models
models_to_fit = 1:5;
data_to_fit = 1:5;

for i = 1:length(data_to_fit)
    results{i} = fitModels(param_structs(models_to_fit), data_train{data_to_fit(i)});
end

%results_WAD_signunique = fitModels({param_struct_WAD, param_struct_WP, param_struct_WADsignunique}, data_WAD_signunique);

save('fitting.mat');

%% Test parameter fits
which_fitted_model = 1;
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
which_fitted_data = 5;
cv_WAD = crossValidateModels(data_test{data_to_fit(which_fitted_data)}, results{which_fitted_data});
mean(cv_WAD)