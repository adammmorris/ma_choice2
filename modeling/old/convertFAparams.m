function full_params = convertFAparams(condensed_params, numAtts, first_att)
num_pop = size(condensed_params,1);
weights = zeros(num_pop,numAtts);

x = condensed_params(:,2); x(x == 0) = -1;
weights(:,first_att) = x;
full_params = [condensed_params(:,1) weights];