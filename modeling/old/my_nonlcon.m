function [c,ceq] = my_nonlcon(x)
c = sum(abs(x(2:end))) - 1;
ceq = [];
end