import numpy as np
from activation import ActivationLayer

class Tanh(ActivationLayer):
    ''' 
    Optional Tanh function if you'd like to try alternatives and see what happens.
    '''
    def __init__(self):
        def tanh(x):
            return np.tanh(x)

        def tanh_prime(x):
            return 1 - np.tanh(x) ** 2

        super().__init__(tanh, tanh_prime)

class SigmoidLayer(ActivationLayer):
    def __init__(self):
        def sigmoid(x):
            return 1 / (1 + np.exp(-x))

        def sigmoid_prime(x):
            s = sigmoid(x) * (1 - sigmoid(x))
            return s 

        super().__init__(sigmoid, sigmoid_prime)