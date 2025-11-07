import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

E = pd.read_csv("energia.dat").squeeze()
print(E)
plt.plot(np.arange(len(E)),E)
plt.show()
