import numpy as np

np.arange(3).reshape((1, 3)).shape
np.arange(3).shape
np.arange(3).reshape((1, 3)) + np.arange(3)

l = np.array([1, 2, 3, 4, 5, 6, 7, 7, 8])
mask = (l < 3) | (l > 6)
l[mask]

m = np.arange(6).reshape(2, 3)
x = abs(m - 3) < 2
m[abs(m - 3) < 2]


X = np.random.random((100, 3))
diff = X.reshape((100, 1, 3)) - X
D = (diff ** 2).sum(axis=2)
i = np.arange(100)
D[i, i] = np.inf
r = D.argmin(axis=0)
