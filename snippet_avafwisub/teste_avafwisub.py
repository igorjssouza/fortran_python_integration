import numpy as np
import avafwisub_static_lib  # Assuming the subroutine is compiled and imported via f2py

# Constants
na = 10
nx = 10

# Variable initialization
dtin = 1
x1 = 0.0
dx = 0.1
na1 = 1
na2 = 5
nangout1 = 1
nangout2 = 3
freqmax = 50.0
famp = 1.0
napulso = 1
nitex = 10
h = 0.1
ermin = 0.01
lambda_ = 0.005
chsis = 1
chout = 2
evp = 1.0
evs = 0.5
erho = 2.3

# Array initializations 
# Fortran subroutine expects a single precision floating-point number 
# (4 bytes per element) for the arrays marked with intent(inout), while 
# the arrays provided from Python (using NumPy) are in double precision 
# (8 bytes per element)
realsis = np.ones((na, nx), dtype=np.float32) #Fortr
vp = np.full((na,), 2.0, dtype=np.float32)
vs = np.full((na,), 1.0, dtype=np.float32)
rho = np.full((na,), 2.0, dtype=np.float32)
wavelet = np.zeros(128, dtype=np.float32)
wavelet[63] = 1.0  # Adjust for zero-based index

# Call the subroutine
# Call the subroutine, ensuring all arrays are properly sized
try:
    sisout = avafwisub_static_lib.avafwisub(dtin,dx,na1,na2,x1,nangout1,nangout2,realsis,vp,vs,rho,wavelet,freqmax,famp,napulso,h,nitex,ermin,lambda_,chsis,chout,evp,evs,erho)
except Exception as e:
    print("Error calling Fortran subroutine:", e)
    exit(1)

# Print outputs for verification
print('Output VP:', vp)
print('Output VS:', vs)
print('Output RHO:', rho)
print('Output Seismic Data:', sisout[:, 0])  # Print first trace
