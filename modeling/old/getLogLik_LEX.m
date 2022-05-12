function [ll] = getLogLik_LEX(x,d,numAtts)
weights = zeros(1,numAtts);
weights(x(2)) = 1 * (x(3) == 1) - 1 * (x(3) == 0);
ll = getLogLik([x(1) weights], d, true);