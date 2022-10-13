%% setup
clear
datapath = '../data/v4_social/pilot2/';
load(strcat(datapath, 'imported_data.mat'));

%% make param structures
% WAD
param_struct_WAD(1).lik = @(x,d) getLogLik(x, d, false);
%param_struct_WAD(1).lik_rounded = @(x,d) getLogLik(x, d, false, true);
param_struct_WAD(1).choicefn = @(x,opts,atts) makeChoice(x,opts,atts,false);
param_struct_WAD(1).nonlcon = [];

gamma_bounds = [4 1];
param_struct_WAD(1).name = 'inverse temperature';
param_struct_WAD(1).logpdf = @(x) log(gampdf(x,gamma_bounds(1),gamma_bounds(2)));  % log density function for prior
%param_struct_WAD(1).logpdf = @(x) 0;
param_struct_WAD(1).lb = 0;    % lower bound
param_struct_WAD(1).ub = 20;   % upper bound
param_struct_WAD(1).int = 0;   % constrained to be an integer? 0

weight_params = [0 1];
for i = 1:numAtts
    param_struct_WAD(i+1).name = strcat('weight',string(i));

    % for full
    param_struct_WAD(i+1).logpdf = @(x) log(normpdf(x,weight_params(1),weight_params(2)));  % log density function for prior
    %param_struct_WAD(i+1).logpdf = @(x) 0;
    param_struct_WAD(i+1).lb = -5;    % lower bound
    param_struct_WAD(i+1).ub = 5;   % upper bound
    param_struct_WAD(i+1).int = 0;
end

% WP
param_struct_WP = param_struct_WAD;
param_struct_WP(1).lik = @(x,d) getLogLik(x, d, true);
%param_struct_WP(1).lik_rounded = @(x,d) getLogLik(x, d, true, true);
param_struct_WP(1).choicefn = @(x,opts,atts) makeChoice(x,opts,atts,true);

% EW
param_struct_EW = param_struct_WAD;
for i = 1:numAtts
    param_struct_EW(i+1).logpdf = @(x) log(1/3);
    param_struct_EW(i+1).lb = -1;
    param_struct_EW(i+1).ub = 1;
    param_struct_EW(i+1).int = 1;
end

% TAL
param_struct_TAL = param_struct_EW;
param_struct_TAL(1).lik = @(x,d) getLogLik(x, d, true);
%param_struct_TAL(1).lik_rounded = @(x,d) getLogLik(x, d, false, true);
param_struct_TAL(1).choicefn = @(x,opts,atts) makeChoice(x,opts,atts,true);

%% LEXNB (non-binary atts)
param_struct_LEXNB = param_struct_WAD;
param_struct_LEXNB(2:end) = [];
param_struct_LEXNB(2).name = 'which_attribute';
param_struct_LEXNB(2).logpdf = @(x) log(1/numAtts);
param_struct_LEXNB(2).lb = 1;
param_struct_LEXNB(2).ub = numAtts;
param_struct_LEXNB(2).int = 1;

param_struct_LEXNB(3).name = 'direction';
param_struct_LEXNB(3).logpdf = @(x) log(1/2);
param_struct_LEXNB(3).lb = 0;
param_struct_LEXNB(3).ub = 1;
param_struct_LEXNB(3).int = 1;

param_struct_LEXNB(1).lik = @(x,d) getLogLik(convertLEXparams(x, numAtts), d, false);
%param_struct_LEX(1).lik_rounded = @(x,d) getLogLik(convertLEXparams(x, numAtts), d, false, true);
param_struct_LEXNB(1).choicefn = @(x,opts,atts) makeChoice(convertLEXparams(x, numAtts), opts, atts, false);

%% LEXB (binary atts)
param_struct_LEXB = param_struct_LEXNB;
param_struct_LEXB(1).lik = @(x,d) getLogLik(convertLEXparams(x, numAtts), d, true);
%param_struct_LEXNBA(1).lik_rounded = @(x,d) getLogLik(convertLEXparams(x, numAtts), d, false, false);
param_struct_LEXB(1).choicefn = @(x,opts,atts) makeChoice(convertLEXparams(x, numAtts), opts, atts, true);

%% LEXPROB
param_struct_LEXPROB = param_struct_WAD;
for i = 1:numAtts
    param_struct_LEXPROB(i+1).logpdf = @(x) 0;
    param_struct_LEXPROB(i+1).lb = 0;
    param_struct_LEXPROB(i+1).ub = 1;
    param_struct_LEXPROB(i+1).int = 0;
end

% % WAD signunique
% param_struct_WADsignunique = param_struct_WAD;
% param_struct_WADsignunique(1).lik = @getLogLik_signunique;
% for i = 1:numAtts
%     param_struct_WADsignunique(numAtts+1+i).name = 'attribute_signed';
%     param_struct_WADsignunique(numAtts+1+i).logpdf = @(x) log(1/2);
%     param_struct_WADsignunique(numAtts+1+i).lb = 0;
%     param_struct_WADsignunique(numAtts+1+i).ub = 1;
%     param_struct_WADsignunique(numAtts+1+i).int = 1;
% end

%% first-att
param_struct_FA = param_struct_EW;
param_struct_FA(1).lik = @(x,d) getLogLik_attpertrial(x, d, false, d.first_att);
param_struct_FA(1).choicefn = [];

%% first-maxdiff-att
param_struct_FMDA = param_struct_FA;
param_struct_FMDA(1).lik = @(x,d) getLogLik_attpertrial(x, d, false, d.first_maxdiff_att);
param_struct_FMDA(1).choicefn = [];

%% least variance
param_struct_LV = param_struct_WAD;
param_struct_LV(2:end) = [];
param_struct_LV(1).lik = @(x,d) getLogLik_LV(x, d);
param_struct_LV(1).choicefn = [];

%% total list
param_structs = {param_struct_WAD, param_struct_WP, param_struct_EW, param_struct_TAL, param_struct_LEXNB, param_struct_LEXB, param_struct_FA, param_struct_FMDA, param_struct_LV};
model_names = {'WAD', 'WP', 'EW', 'TAL', 'LEXNB', 'LEXB', 'FA', 'FMDA', 'LV'};

save(['param_structs_' num2str(numAtts) 'att.mat'], "param_structs", "model_names")
