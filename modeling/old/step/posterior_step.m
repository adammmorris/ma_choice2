function post = posterior_step(param_vals, data, param_structure)

post = getLogLik(param_vals,data);

inv_temp = param_structure(1);
weights = param_structure(2:end);
steps = param_structure((numAtts+2):(numAtts*2+1));
signs_weights = param_structure((numAtts*2+2):(numAtts*3+1));
signs_optionvals = param_structure((numAtts*3+2):end);

post = post + inv_temp.logpdf(param_vals(1));

for i = 1:numAtts
    if signs_weights(steps(i)), post = post + weights(i).logpdf2(param_vals(i+1));
    else, post = post + weights(i).logpdf(param_vals(i+1));
    end

    post = post + steps(i).logpdf(param_vals(i+1+numAtts)) + signs_weights(i).logpdf(param_vals(i+1+numAtts*2)) +...
        signs_optionvals(i).logpdf(param_vals(i+1+numAtts*3));
end
end