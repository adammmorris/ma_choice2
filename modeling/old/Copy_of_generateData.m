function [data_WAD, data_WP, data_EW, data_TAL] = generateData(numChoices, numAgents, numAtts, numValuesPerAtt, inv_temp, weights, weights_signed)

options = randi([1 numValuesPerAtt], numAtts, 2, numChoices, numAgents) / numValuesPerAtt;
%avail_atts = logical(randi([0 1], numChoices, numAtts, numAgents));
avail_atts = true(numChoices, numAtts, numAgents);

struct_template.N = numChoices;

for agent = 1:numAgents
    data_WAD(agent) = struct_template;
    data_WP(agent) = struct_template;
    data_EW(agent) = struct_template;
    data_TAL(agent) = struct_template;
    %data_LEX(agent) = struct_template;
end

for agent = 1:numAgents

    data_WAD(agent).options = options(:,:,:,agent);
    data_WAD(agent).params = [inv_temp(agent) weights(agent,:)];
    data_WAD(agent).avail_atts = avail_atts(:,:,agent);
    data_WAD(agent).choices = makeChoice(data_WAD(agent).params, data_WAD(agent).options);

    data_WP(agent).options = options(:,:,:,agent);
    data_WP(agent).params = [inv_temp(agent) weights(agent,:)];
    data_WP(agent).avail_atts = avail_atts(:,:,agent);
    data_WP(agent).choices = makeChoice(data_WP(agent).params, signOptions(data_WP(agent).options));

    data_EW(agent).options = options(:,:,:,agent);
    data_EW(agent).params = [inv_temp(agent) weights_signed(agent,:)];
    data_EW(agent).avail_atts = avail_atts(:,:,agent);
    data_EW(agent).choices = makeChoice(data_EW(agent).params, data_EW(agent).options);

    data_TAL(agent).options = options(:,:,:,agent);
    data_TAL(agent).params = [inv_temp(agent) weights_signed(agent,:)];
    data_TAL(agent).avail_atts = avail_atts(:,:,agent);
    data_TAL(agent).choices = makeChoice(data_TAL(agent).params, signOptions(data_TAL(agent).options));

%     data_LEX(agent).options = options(:,:,:,agent);
%     data_LEX(agent).params = [inv_temp(agent) weights_lex(agent,:)];
%     data_LEX(agent).avail_atts = avail_atts(:,:,agent);
%     params_LEX(agent, :) = data_LEX(agent).params;
%     data_LEX(agent).choices = makeChoice(data_LEX(agent).params, data_LEX(agent).options);
end