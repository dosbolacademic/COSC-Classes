import numpy as np
from layer import Layer


class ReshapeLayer(Layer):
    def __init__(self, input_shape, output_shape) -> None:
        self.input_shape = input_shape
        self.output_shape = output_shape

    def forward(self, input):
        # TODO: reshape the input to the output_shape and return it.
        self.input = input
        return np.reshape(input, self.output_shape)

    def backward(self, output_gradient, learning_rate):
        # TODO: reshape the output to the input_shape and return it.
        return np.reshape(output_gradient, self.input_shape)
