clear
datapath = '../data/v3_nosteps/real1/';
choices = readmatrix(strcat(datapath, 'modeling_choice.csv'));
opts1 = readmatrix(strcat(datapath, 'modeling_opts1.csv'));
opts2 = readmatrix(strcat(datapath, 'modeling_opts2.csv'));
avail_atts = readmatrix(strcat(datapath, 'modeling_avail_atts.csv'));
first_atts = readmatrix(strcat(datapath, 'first_atts.csv'));
first_maxdiff_atts = readmatrix(strcat(datapath, 'first_maxdiff_atts.csv'));

subjects = unique(choices(:,1));
numSubj = length(subjects);
numAtts = size(avail_atts,2) - 1;

for subj_ind = 1:numSubj
    subj = subjects(subj_ind);
    first_att = first_atts(first_atts(:,1) == subj,2);
    first_maxdiff_att = first_maxdiff_atts(first_maxdiff_atts(:,1) == subj,2);
    cur_choices = choices(choices(:,1) == subj,2);
    cur_opts1 = opts1(opts1(:,1) == subj,2:end);
    cur_opts2 = opts2(opts2(:,1) == subj,2:end);
    cur_avail_atts = logical(avail_atts(avail_atts(:,1) == subj,2:end));
    
    numChoices = length(cur_choices);

    cur_options = nan(numAtts, 2, numChoices);
    for choice = 1:numChoices
        cur_options(:,:,choice) = [cur_opts1(choice,:)' cur_opts2(choice,:)'];
    end

    data_real(subj).N = numChoices;
    data_real(subj).first_att = first_att;
    data_real(subj).first_maxdiff_att = first_maxdiff_att;
    data_real(subj).options = cur_options;
    data_real(subj).avail_atts = cur_avail_atts;
    data_real(subj).choices = cur_choices;
end

data_real_scaled = data_real;
for i = 1:length(data_real_scaled)
    data_real_scaled(i).options = signOptions(data_real_scaled(i).options,false,true);
end

save(strcat(datapath, 'imported_data'));