# This is a sample Python script.

# Press Shift+F10 to execute it or replace it with your code.
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.


# def print_hi(name):
# Use a breakpoint in the code line below to debug your script.
#    print(f'Hi, {name}')  # Press Ctrl+F8 to toggle the breakpoint.


# Press the green button in the gutter to run the script.


# See PyCharm help at https://www.jetbrains.com/help/pycharm/

import scipy.io
from scipy.stats import norm
from scipy.stats import gamma
import time
import numpy as np
import matplotlib.pyplot as plt
from geneticalgorithm import geneticalgorithm as ga

simulated_data = scipy.io.loadmat('simulated_data.mat')
data_WAD = simulated_data['data_WAD']
param_struct = simulated_data['param_struct']
numAgents = simulated_data['numAgents']
numAtts = simulated_data['numAtts']
numChoices = simulated_data['numChoices']
numParams = simulated_data['numParams']
numValuesPerAtt = simulated_data['numValuesPerAtt']
params_WAD = simulated_data['params_WAD']


def logsumexp(x, dim=0):
    # Returns log(sum(exp(x),dim)) while avoiding numerical underflow.
    # Default is dim = 1 (columns). 0 in python

    # subtract the largest in each column
    y = np.amax(x, dim)
    x = x - y
    s = y + np.log(np.sum(np.exp(x), dim))
    i = np.argwhere(~np.isfinite(y))
    if not (i.size == 0):
        s[i] = y[i]
    return s


def getLogLik(parameters, data):
    # parameters: [invTemp weights]
    # data: structure with .choices and .options

    # get the task structure & choices
    options = data['options']
    choices = data['choices']
    options_dim = np.shape(options)
    numChoices = options_dim[2]

    # extract parameters
    inv_temp = np.array([parameters[0]])
    weights = np.array(parameters[1:len(parameters)])

    # initialize log likelihood to zero
    loglik = 0

    # loop through choices
    for i in range(0, numChoices):
        # for the WAD model, we just multiply the weights by the options.
        utilities = weights @ options[:, :, i]

        # to convert utilities -> probabilities, the model used a "softmax" (or
        # Boltzmann) function: e^(inverse_temperature * utility(chosen_option))
        # / sum_over_all_options(e^(inverse_temperature * utility(option))
        # so this formula gives us the likelihood of the person's choice, given
        # the utilities calculated above.
        # we want the LOG likelihood, so we take the log of that, which gives
        # us: inv_temp * utility(chosen_option) - log(sum(e^inverse_temperature
        # * utilities))). to compute that latter term efficiently, we use the
        # "logsumexp" function.
        ut = utilities.reshape(1, 2)
        ut = inv_temp @ ut
        ut = ut.reshape(1, 2)
        loglik = loglik + inv_temp @ utilities[choices[i] - 1] - logsumexp(ut, 1)

    return loglik


def getPriorsum(params, param_struct):
    # print(params)
    # print(len(param_struct))
    priorsum = 0
    weight_params = np.array([[0., 1.]])
    gamma_bounds = np.array([[1., 5.]])
    for i in range(0, len(param_struct)):
        if i == 0:
            logpdf = lambda x: np.sum(np.log(gamma.pdf(x, gamma_bounds[0][0], gamma_bounds[0][1])))
        else:
            logpdf = lambda x: np.sum(np.log(norm.pdf(x, weight_params[0][0], weight_params[0][1])))
        priorsum = priorsum + logpdf(params[i])
    return priorsum


# fit WAD model
# data = data_WAD
def fitWAD(param_struct, data):
    # get number of subjects & params
    numSubj = len(data[0])
    numParams = len(param_struct[0])

    # our log likelihood function
    lik = lambda x, d: getLogLik(x, d)

    # set up results matrices
    logpost_WAD = np.zeros((numSubj, 1))
    loglik_WAD = np.zeros((numSubj, 1))
    best_fit_params_WAD = np.zeros((numSubj, numParams))
    BICs_WAD = np.zeros((numSubj, 1))
    AICs_WAD = np.zeros((numSubj, 1))

    # loop through each subject-- will check parallel loop later
    for s in range(0, 5):
        print('Fitting WAD for subject ' + str(s))

        # our posterior (the log likelihood + the log prior)
        # we make it negative because the "ga" function minimizes its objective
        WAD_post = lambda x: -(lik(x, data[0][s]) + getPriorsum(x, param_struct[0]))

        # do optimization with the genetic algorithm (ga) function

        # create varbound array for ga algorithm
        vb = []
        for i in range(0, len(param_struct[0])):
            lb = np.vstack(param_struct[0]['lb'])
            ub = np.vstack(param_struct[0]['ub'])
            vb.append(np.concatenate((lb[i], ub[i])))
        varbound = np.array(vb)

        # create vartype arr for ga algorithm
        index = []
        arr = np.argwhere(np.vstack(param_struct[0]['int']))
        for i in range(0, len(np.argwhere(np.vstack(param_struct[0]['int'])))):
            index.append(arr[i][0])
        arr = list(range(0, len(param_struct[0])))
        for i in range(0, len(param_struct[0])):
            if i in index:
                arr[i] = ['int']
            else:
                arr[i] = ['real']
        vartype = np.array(arr)

        # run model

        # simulated annealing global optimization for a multimodal objective function
        from scipy.optimize import dual_annealing

        # objective function
        def objective(v):
            x, y = v
            return (x ** 2 + y - 11) ** 2 + (x + y ** 2 - 7) ** 2

        # define range for input
        r_min, r_max = -5.0, 5.0
        # define the bounds on the search
        bounds = [[r_min, r_max], [r_min, r_max]]
        # perform the simulated annealing search
        result = dual_annealing(objective, bounds)
        # summarize the result
        #print('Status : %s' % result['message'])
        #print('Total Evaluations: %d' % result['nfev'])
        # evaluate solution
        solution = result['x']
        evaluation = objective(solution)
        #print('Solution: f(%s) = %.5f' % (solution, evaluation))
        #scipy optimize
        #from scipy.optimize import Bounds
        #bounds = Bounds(varbound)
        #from scipy.optimize import LinearConstraint
        #linear_constraint = LinearConstraint() #not needed for WAD
        # res = minimize(rosen, x0, method='trust-constr', jac=rosen_der, hess=rosen_hess,
        #                constraints=[linear_constraint, nonlinear_constraint],
        #                options={'verbose': 1}, bounds=bounds)
        #genetic-algorithm
        #model = ga(function=WAD_post,
        #           dimension=len(param_struct[0]),
        #           variable_boundaries=varbound,
        #           variable_type_mixed=vartype)
        #model.run()
        #x = model.output_dict['variable']
        #logpost = model.output_dict['function']

        # store best fit params
        #best_fit_params_WAD[s, :] = x
        # store the maximum likelihood
        #loglik_WAD[s] = lik(x, data[0][s])
        # store the maximum posterior
        #logpost_WAD[s] = -logpost
        # store the BIC and AIC
        temp = np.array([2])
        nump = np.array([numParams])
        BICs_WAD[s] = nump @ np.log(data[0][s]['N'])[0] - temp @ loglik_WAD[s]
        AICs_WAD[s] = nump @ temp - temp @ loglik_WAD[s]

        print('Completed optimization for WAD for subject ' + str(s))

    # make the results struct--> dictionary in python
    results_template = {'K': numParams, 'S': numSubj, 'param': param_struct[0]}

    results_WAD = results_template
    results_WAD['logpost'] = logpost_WAD
    results_WAD['loglik'] = loglik_WAD
    results_WAD['x'] = best_fit_params_WAD
    results_WAD['bic'] = BICs_WAD
    results_WAD['aic'] = AICs_WAD
    results_WAD['likfun'] = lik
    return results_WAD


if __name__ == '__main__':
    results_WAD = fitWAD(param_struct, data_WAD)
    print(np.corrcoef(params_WAD[0, :], result_WAD[0, :]))
    print(np.corrcoef(params_WAD[1, :], result_WAD[1, :]))
    print(np.corrcoef(params_WAD[2, :], result_WAD[2, :]))
    print(np.corrcoef(params_WAD[3, :], result_WAD[3, :]))
    print(np.corrcoef(params_WAD[4, :], result_WAD[4, :]))
