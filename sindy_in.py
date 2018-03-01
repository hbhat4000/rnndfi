import numpy as np
from scipy.integrate import odeint
from sklearn.model_selection import train_test_split

from sparsereg.model import SINDy

x = np.loadtxt("data.txt")
n_rows = x.shape[0]/2

#xd = x[:,0:3]
#print(xd.shape)

x_train, x_test = x[:int(n_rows/2)], x[int(n_rows/2):]

kw = dict(fit_intercept=True, normalize=False)
model = SINDy(dt=1, degree=1, threshold=0.01, alpha=1.0, n_jobs=-1, kw=kw).fit(x_train)
# myops = {'sin': np.sin, 'cos': np.cos}
# model = SINDy(dt=0.1, degree=3, threshold=0.15, operators=myops,
#               alpha=1.0, n_jobs=-1, kw=kw).fit(x_train)

print("Score on test data ", model.score(x_test))

for i, eq in enumerate(model.equations()):
    print("dx_{} / dt = ".format(i), eq)


