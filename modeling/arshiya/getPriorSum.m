function priorsum = getPriorSum(params, param_struct)
priorsum = 0;
for i = 1:length(param_struct)
    priorsum = priorsum + param_struct(i).logpdf(params(i));
end