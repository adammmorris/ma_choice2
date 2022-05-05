function priorsum = getPriorSum(params, param_struct)
num_pop = size(params,1);
priorsum = zeros(num_pop, 1);
for i = 1:length(param_struct)
    priorsum = priorsum + param_struct(i).logpdf(params(:,i));
end