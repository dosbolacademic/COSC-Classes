import numpy as np
from keras.datasets import mnist
from keras.utils import np_utils

from convolutional import ConvolutionalLayer
from dense import DenseLayer
from reshape import ReshapeLayer
from activations import SigmoidLayer
from softmax import SoftmaxLayer
from losses import binary_cross_entropy, binary_cross_entropy_prime


def preprocess_data(x, y, limit):
    ''' 
    Will limit our data since using the whole thing will take forever on a cpu especially since we're
    implementing this from scratch.
    '''
    x = x.reshape(len(x), 1, 28, 28)
    x = x.astype("float32") / 255
    y = np_utils.to_categorical(y)
    y = y.reshape(len(y), 10, 1)
    return x[:limit], y[:limit]

# load MNIST from server, limit to 100 images per class since we're not training on GPU
(x_train, y_train), (x_test, y_test) = mnist.load_data()
x_train, y_train = preprocess_data(x_train, y_train, 100)
x_test, y_test = preprocess_data(x_test, y_test, 100)

# TODO: Add our layers and the flow of input into this list. 
network = [
    ConvolutionalLayer((1, 28, 28), 3, 5),
    SigmoidLayer(),
    ReshapeLayer((5, 26, 26), (5 * 26 * 26, 1)),
    DenseLayer(5 * 26 * 26, 100),
    SigmoidLayer(),
    DenseLayer(100, 100),
    SigmoidLayer(),
    DenseLayer(100, 10),
    SoftmaxLayer(),
]

#train
def process(network, x):
    output = x
    for layer in network:
        output = layer.forward(output)
    return output


def train(
    network,
    loss,
    loss_prime,
    x_train,
    y_train,
    epochs=1000,
    learning_rate=0.01,
    verbose=True,
):
    for e in range(epochs):
        error = 0
        for x, y in zip(x_train, y_train):
            # forward
            output = process(network, x)


            # TODO: update our error
            error += loss(y, output)
            grad = loss_prime(y, output)


            # TODO: perform back prop 
            for layer in reversed(network):
                grad = layer.backward(grad, learning_rate)

        error /= len(x_train)
        print(f"{e + 1}/{epochs}, error={error}")


train(
    network,
    binary_cross_entropy,
    binary_cross_entropy_prime,
    x_train,
    y_train,
    epochs=100,
    learning_rate=0.1,
)

# TODO: run the test data through and print out your predictions
for x, y in zip(x_test, y_test):
    output = process(network, x)
    print(f"{np.argmax(output)} excepted {np.argmax(y)}")
