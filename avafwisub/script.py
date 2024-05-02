import avafwisub_static_lib as avafwi
import numpy as np
import pandas as pd
# import array
# arr = array.array
df = pd.read_csv('avafwisub/wavelet-exp1.dat')

na = 128
nx = 10

# Variable initialization
dtin = 0.004
x1 = 0.0
dx = 0.1
na1 = 225
na2 = 400
nangout1 = 1
nangout2 = 7
freqmax = 32.0
famp = 1.0
napulso = 1 #?
nitex = 15
h = 20.0 #?
ermin = 0.15
lambda_ = 0.12
chsis = 0
chout = 0
evp = 1.0
evs = 1.0
erho = 1.0

# Array initializations 
# Fortran subroutine expects a single precision floating-point number 
# (4 bytes per element) for the arrays marked with intent(inout), while 
# the arrays provided from Python (using NumPy) are in double precision 
# (8 bytes per element)
realsis = np.ones((na, nx), dtype=np.float32) #Fortr
# realsis=np.fromfile('bin_files/anglegather-exp1.bin', dtype=np.float32)
vp=np.fromfile('bin_files/trendmodvp-exp1.bin', dtype=np.float32)
# with open('bin_files/trendmodvp-exp1.bin', 'rb') as file1:
    # vp = file1.read()
# with open('bin_files/trendmodvs-exp1.bin', 'rb') as file2:
    # vs = file2.read()
vs=np.fromfile('bin_files/trendmodvs-exp1.bin', dtype=np.float32)
# with open('bin_files/modrho-exp1.bin', 'rb') as file3:
    # rho = file3.read()
rho=np.fromfile('bin_files/modrho-exp1.bin', dtype=np.float32)
# vp = np.full((na,), 2.0, dtype=np.float32)
# vs = np.full((na,), 1.0, dtype=np.float32)
# rho = np.full((na,), 2.0, dtype=np.float32)
wavelet=df.to_numpy(dtype=np.float32)
# wavelet = np.ones(128, dtype=np.float32)
# wavelet[64] = 0.0  # Adjust for zero-based index

# Call the subroutine
# Call the subroutine, ensuring all arrays are properly sized
try:
    sisout = avafwi.avafwisub(dtin,dx,na1,na2,x1,nangout1,nangout2,realsis,vp,vs,rho,wavelet,freqmax,famp,napulso,h,nitex,ermin,lambda_,chsis,chout,evp,evs,erho)
except Exception as e:
    print("Error calling Fortran subroutine:", e)
    exit(1)

# Print outputs for verification
print('Output VP:', vp)
print('Output VS:', vs)
print('Output RHO:', rho)
print('Output Seismic Data:', sisout[:, 0])  # Print first trace
# import json

# # Path to the JSON file
# file_path = 'parameters.json'

# # Open the file and load the data
# with open(file_path, 'r') as file:
#     data = json.load(file)

# # Print the data to verify it's loaded correctly
# print(data)
