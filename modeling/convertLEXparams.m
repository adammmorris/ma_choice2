function full_params = convertLEXparams(condensed_params, numAtts)
num_pop = size(condensed_params,1);
weights = zeros(num_pop,numAtts);

weight_indices = sub2ind(size(weights), 1:num_pop, condensed_params(:,2)');
x = condensed_params(:,3); x(x == 0) = -1;
weights(weight_indices) = x;
full_params = [condensed_params(:,1) weights];