%% setup
clear
datapath = '../data/v3_nosteps/real1/';

addpath("mfit/");
load(strcat(datapath, 'cv_results.mat'));
load('param_structs.mat');

%% compute subj value diff
numTrials = 75;
numAtts = 12;

inv_temps = zeros(numSubj, 1);
best_params = zeros(numSubj, numAtts);
option_values = zeros(numSubj, numTrials);

for subj = 1:numSubj
    subj_data = data_real_scaled(subj);
    subj_model = best_model(subj);
    subj_results = results(subj_model);
    if subj_model == find(strcmp(model_names,'LEX'))
        temp = convertLEXparams(subj_results.x(subj,:), numAtts);
        best_params(subj,:) = temp(2:end);
    else
        best_params(subj,:) = subj_results.x(subj,2:end);
    end
    inv_temps(subj) = subj_results.x(subj,1);

    option_values(subj,:) = best_params(subj,:) * squeeze(subj_data.options(:,1,1:numTrials));
end

%% check out results
option_values_abs = abs(option_values);

option_values_bysubj = mean(option_values_abs,2);
hist(option_values_bysubj)
option_values_bysubj_sd = std(option_values_abs,0,2);
hist(option_values_bysubj_sd)

choice_probs = exp(inv_temps .* option_values_bysubj) ./ (exp(inv_temps .* option_values_bysubj) + 1);
hist(choice_probs)

scatter(inv_temps, option_values_bysubj)
scatter(inv_temps, option_values_bysubj_sd)

tosave = [option_values_bysubj, choice_probs, option_values_bysubj_sd];
writematrix(tosave, strcat(datapath, 'option_diffs', version, '.csv'));

%% for each generated trial set, look at the value diffs it would produce if used for all subjects

option_values2_bysubj = zeros(numSubj, numSubj);
option_values2_bysubj_sd = zeros(numSubj, numSubj);
option_values2_total_bysubj = zeros(numSubj, numSubj);
option_values2_total_bysubj_sd = zeros(numSubj, numSubj);
att_cors = zeros(numSubj, 1);

for subj = 1:numSubj
    subj_options = squeeze(data_real_scaled(subj).options(:,1,1:numTrials));
    option_values2 = abs(best_params * subj_options);
    option_values2_bysubj(subj,:) = mean(option_values2,2)';
    option_values2_bysubj_sd(subj,:) = std(option_values2,0,2)';

    subj_options_total = data_real(subj).options(:,:,1:numTrials);
    for att = 1:numAtts
        subj_options_total(att,:,:) = rescale(squeeze(subj_options_total(att,:,:)),-1,1);
    end
    option_values2_total = best_params * squeeze(subj_options_total(:,1,:)) + best_params * squeeze(subj_options_total(:,2,:));
    option_values2_total_bysubj(subj,:) = mean(option_values2_total,2)';
    option_values2_total_bysubj_sd(subj,:) = std(option_values2_total,0,2)';
    
    temp = corrcoef(subj_options');
    att_cors(subj,:) = mean(mean(triu(temp,1)));
end

% Want mean and sd of subjective value difference b/w 2 options to be roughly the same (i.e. have a low sd) across ppl
sd_of_meandiff = std(option_values2_bysubj,0,2); % sd of mean diff
hist(sd_of_meandiff) % which option set doesn't make much difference for how 
[a,b] = min(sd_of_meandiff);
sd_of_sddiff = std(option_values2_bysubj_sd,0,2); % sd of sd diff
hist(sd_of_sddiff) % which option set makes very little difference for how varied the value diffs are across trials
[a,b] = min(sd_of_sddiff);

% Want sd of value differences within each subject to be high
mean_of_sddiff = mean(option_values2_bysubj_sd,2); % mean of sd diff
hist(mean_of_sddiff) % which option set makes very little difference for how varied the value diffs are across trials

sd_of_meandiff_total = std(option_values2_total_bysubj,0,2); % sd of mean diff
sd_of_sddiff_total = std(option_values2_total_bysubj_sd,0,2); % sd of sd diff
hist(sd_of_sddiff_total) % which option set makes very little difference for how varied the value diffs are across trials
[a,b] = min(sd_of_meandiff_total);

% want correlation b/w atts to be low
hist(att_cors)

cur_options = squeeze(data_real_scaled(72).options(:,1,1:numTrials));
option_values2 = abs(best_params * cur_options);
hist(option_values2_bysubj(72,:)) % even in best case, pretty big range of avg value diff across people
hist(option_values2_bysubj_sd(72,:)) % even in best case, pretty big range of range of value diffs across people
hist(option_values2_total_bysubj(72,:)) % low range of total value diffs across people
hist(option_values2_total_bysubj_sd(72,:)) % pretty big range of range of total value diffs across people
att_cors(72,:) % low correlations

%% generate 10k new option sets

for att = 1:numAtts
    att_ranges{att} = unique(subj_options_total(att,1,:));
end

numSets = 1000;
new_options = zeros(numAtts, 2, numTrials, numSets);
for set = 1:numSets
    for att = 1:numAtts
        for trial = 1:numTrials
            new_options(att, :, trial, set) = randsample(att_ranges{att},2,false);
        end
    end
end

option_values3_bysubj = zeros(numSets, numSubj);
option_values3_bysubj_sd = zeros(numSets, numSubj);
option_values3_total_bysubj = zeros(numSets, numSubj);
option_values3_total_bysubj_sd = zeros(numSets, numSubj);
att_cors3 = zeros(numSubj, 1);

for set = 1:numSets
    set_options = signOptions(new_options(:,:,:,set),false,true);
    set_options = squeeze(set_options(:,1,:));
    option_values3 = abs(best_params * set_options);
    option_values3_bysubj(set,:) = mean(option_values3,2)';
    option_values3_bysubj_sd(set,:) = std(option_values3,0,2)';

    set_options_total = new_options(:,:,:,set);
    option_values3_total = best_params * squeeze(set_options_total(:,1,:)) + best_params * squeeze(set_options_total(:,2,:));
    option_values3_total_bysubj(set,:) = mean(option_values3_total,2)';
    option_values3_total_bysubj_sd(set,:) = std(option_values3_total,0,2)';
    
    temp = corrcoef(set_options');
    att_cors3(set,:) = mean(mean(triu(temp,1)));
end

% Want mean and sd of subjective value difference b/w 2 options to be roughly the same (i.e. have a low sd) across ppl
sd_of_meandiff = std(option_values3_bysubj,0,2); % sd of mean diff
hist(sd_of_meandiff) % which option set doesn't make much difference for how 
[a,b] = min(sd_of_meandiff);
sd_of_sddiff = std(option_values3_bysubj_sd,0,2); % sd of sd diff
hist(sd_of_sddiff) % which option set makes very little difference for how varied the value diffs are across trials
[a,b] = min(sd_of_sddiff);

% Want sd of value differences within each subject to be high
mean_of_sddiff = mean(option_values3_bysubj_sd,2); % mean of sd diff
hist(mean_of_sddiff) % which option set makes very little difference for how varied the value diffs are across trials

sd_of_meandiff_total = std(option_values3_total_bysubj,0,2); % sd of mean diff
sd_of_sddiff_total = std(option_values3_total_bysubj_sd,0,2); % sd of sd diff
hist(sd_of_sddiff_total) % which option set makes very little difference for how varied the value diffs are across trials
[a,b] = min(sd_of_meandiff_total);

% want correlation b/w atts to be low
hist(att_cors)