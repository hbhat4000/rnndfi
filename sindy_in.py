import numpy as np
from scipy.integrate import odeint
from sklearn.model_selection import train_test_split

from sparsereg.model import SINDy

x = np.loadtxt("data.txt")
n_rows = x.shape[0]/2
n_cols = x.shape[1]
n_controls = 4

x_train, x_test = x[:int(n_rows/2)], x[int(n_rows/2):]

kw = dict(fit_intercept=True, normalize=False)
model = SINDy(dt=1/60, degree=1, threshold=0.1, alpha=1, n_jobs=-1, kw=kw)

y_train = x_train[:,n_controls:n_cols] # create separate y so x contains "control"
y_test = x_test[:,n_controls:n_cols]
y_train = model.derivative.transform(y_train)
y_test = model.derivative.transform(y_test)

model = model.fit(x_train,y=y_train)

print("Score on test data ", model.score(x_test))

for i, eq in enumerate(model.equations()):
    print("dx_{} / dt = ".format(i), eq)


