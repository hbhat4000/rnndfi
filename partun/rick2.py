import numpy as np
from scipy.integrate import odeint
from sklearn.model_selection import train_test_split

from sparsereg.model import SINDy

x = np.loadtxt("no_input.txt")
print(x.shape)

#xd = x[:,0:3]
#print(xd.shape)

x_train, x_test = x[:3000], x[3000:]

kw = dict(fit_intercept=True, normalize=False)
model = SINDy(dt=0.1, degree=1, threshold=0.175, alpha=1.0, n_jobs=-1, kw=kw).fit(x_train)
#myops = {'sin': np.sin, 'cos': np.cos}
#model = SINDy(dt=1, degree=2, threshold=0.1, operators=myops,
#              alpha=1.0, n_jobs=-1, kw=kw).fit(x_train)

print("Score on test data ", model.score(x_test))

for i, eq in enumerate(model.equations()):
    print("dx_{} / dt = ".format(i), eq)

