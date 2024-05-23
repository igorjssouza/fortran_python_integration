import os
import json
import avafwisub_static_lib as avafwi
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

def load_data(file_path, loader_func, *args, **kwargs):
    if os.path.exists(file_path):
        return loader_func(file_path, *args, **kwargs)
    else:
        raise FileNotFoundError(f"File not found: {file_path}")

def load_constants(json_path):
    if os.path.exists(json_path):
        with open(json_path, 'r') as file:
            return json.load(file)
    else:
        raise FileNotFoundError(f"JSON file not found: {json_path}")

# Load constants from JSON file
constants = load_constants('constants.json')

# Load data from files
wavelet = load_data('bin_files/wavelet-exp1.dat', pd.read_csv).to_numpy(dtype=np.float32) # pulso sismico <real(128)>
realsis_array = load_data('bin_files/anglegather-exp1.bin', np.fromfile, dtype=np.float32) # conjunto de angulos de entrada <real(na,nx)>
vp = load_data('bin_files/trendmodvp-exp1.bin', np.fromfile, dtype=np.float32) # modelo inicial de VP (m/s) <real(na)>
vs = load_data('bin_files/trendmodvs-exp1.bin', np.fromfile, dtype=np.float32) # modelo inicial de VS (m/s) <real(na)>
rho = load_data('bin_files/trendmodrho-exp1.bin', np.fromfile, dtype=np.float32) # modelo inicial de densidade (Kg/m3) <real(na)>

# Extract constants
na = constants["na"]             # num. de amostras do conjunto de entrada <integer>
nx = constants["nx"]             # num. de angulos do conjunto de entrada <integer>
dtin = constants["dtin"]         # intervalo de amostragem <real>
x1 = constants["x1"]             # primeiro angulo(graus) <real>
dx = constants["dx"]             # intervalo entre angulos (graus) <real>
na1 = constants["na1"]           # inicio janela temporal de inversao (Num. amostra) <integer> === verificar parametro ===
na2 = constants["na2"]           # final  janela temporal de invpersao (Num. amostra) <integer>=== verificar parametro ===
nangout1 = constants["nangout1"] # angulo inicial p/ inversao (Num. traco) <integer> === verificar parametro ===
nangout2 = constants["nangout2"] # angulo final p/ inversao   (Num. traco) <integer> === verificar parametro ===
h = constants["h"]               # espessura das camadas elementares para inversao (m) <real> === verificar parametro ===
nitex = constants["nitex"]       # numero maximo de iteracoes  <integer>
ermin = constants["ermin"]       # erro normalizado minimo <real>
lambda_ = constants["lambda_"]   # fator de regularizacao  <real>     === verificar parametro ===
evp = constants["evp"]           # energia de vp relativo 
evs = constants["evs"]           # energia de vs relativo 
erho = constants["erho"]         # energia de den relativo 
freqmax = constants["freqmax"]   # freq. maxima do pulso sismico (Hz) <real>
famp = constants["famp"]         # fator de amplitude do pulso sismico <real> === verificar parametro ===
napulso = constants["napulso"]   # duracao efetiva do pulso (Num. amostras) <integer> forcido pelo avafwiprog-v6-3
chsis = constants["chsis"]       # (0) disis=modelado (1) dsis=diferenca <integer>
chout = constants["chout"]       # (0) vp, vs e den relativos (1) vp, vs e den absolutos <integer>

# Reshape realsis_array into realsis matrix
# if realsis_array.size == na * nx:
realsis = realsis_array.reshape((nx, na)).T
sisout = np.zeros((nx, na), dtype=np.float32).T
# else:
    # raise ValueError("Size of realsis_array does not match na * nx")

def plot_and_save(data, title, filename):
    """Plots the data and saves the plot as a PDF."""
    plt.figure()
    plt.plot(data)
    plt.title(title)
    plt.savefig(filename, format='pdf')

plt.figure(figsize=(12, 8))
plt.imshow(realsis, cmap='gray', interpolation='nearest', aspect=0.2)
plt.colorbar()
plt.title('Matrix Visualization')
plt.xlabel('Column Index')
plt.ylabel('Row Index')
plt.savefig('realsis.pdf', format='pdf')

plot_and_save(vp, 'VP', 'vp_plot.pdf')
plot_and_save(vs, 'VS', 'vs_plot.pdf')
plot_and_save(rho, 'RHO', 'rho_plot.pdf')

# Call the Fortran subroutine
try:
    vp, vs, rho, sisout=avafwi.avafwisub(dtin, dx, na1, na2, x1, nangout1, nangout2, realsis, vp, vs, rho, wavelet, freqmax, famp, napulso, h, nitex, ermin, lambda_, chsis, sisout, chout, evp, evs, erho,[na,nx])
    # sismograma de diferença ou modelado <real(na,nx)>
except Exception as e:
    print("Error calling Fortran subroutine:", e)
    exit(1)

# Print outputs for verification
print('Output VP:', vp)
print('Output VS:', vs)
print('Output RHO:', rho)
print('Output Seismic Data:', sisout[:, 0])  # Print first trace


# import avafwisub_static_lib as avafwi
# import numpy as np
# import pandas as pd
# import matplotlib.pyplot as plt
# """
# Caro Geovane, estou passando um dado sintético para testar o AVA-FWI.

# O dado de entrada consiste de um conjunto de ângulo com 128 amostras e dez traços variando de 5 a 50 graus com intervalo regular de cinco graus (dt=4ms). descrição dos arquivos:
# anglegather-exp1.bin : dado de entrada
# wavelet-exp1.dat: pulso sísmico 
# trendmodvp-exp1.bin: modelo inicial de vp
# trendmodvs-exp1.bin: modelo inicial de vs
# trendmodrho-exp1.bin: modelo inicial de densidade
# modvp-exp1.bin: modelo alvo de vp
# modvs-exp1.bin: modelo alvo de vs 
# modrho-exp1.bin: modelo alvo de densidade
# """
# # import array
# # arr = array.array
# df = pd.read_csv('bin_files/wavelet-exp1.dat')
# # ximage < binaryfile n1=(samples in Y axis) =(distance between samples in Y)
# na = 128
# nx = 10 # num. de angulos do conjunto de entrada <integer> ----- OK
# # realsis = np.ones((na, nx), dtype=np.float32) #Fortr
# realsis_array =np.fromfile('bin_files/anglegather-exp1.bin', dtype=np.float32)
# if realsis_array.size == na*nx:
#     realsis = np.zeros((na,nx), dtype=np.float32)

#     for col in range(nx):
#         for row in range(na):
#             realsis[row,col]=realsis_array[col*na+row]
# # realsis = np.rot90(realsis, -1)
# # realsis=realsis_array.reshape((na,nx))
# # Variable initialization
# dtin = 0.004 # intervalo de amostragem <real>  ----- OK
# x1 = 5.0 # primeiro angulo(graus) <real> ----- OK
# dx = 5.0 # intervalo entre angulos (graus) <real> ----- OK
# na1 = 30 # inicio janela temporal de inversao (Num. amostra) <integer> ----- OK
# na2 = 80 # final  janela temporal de invpersao (Num. amostra) <integer> ----- OK
# nangout1 = 5 # angulo inicial p/ inversao (Num. traco) <integer> ----- OK
# nangout2 = 50 # angulo final p/ inversao   (Num. traco) <integer> ----- OK
# wavelet=df.to_numpy(dtype=np.float32)
# vp=np.fromfile('bin_files/trendmodvp-exp1.bin', dtype=np.float32)
# vs=np.fromfile('bin_files/trendmodvs-exp1.bin', dtype=np.float32)
# rho=np.fromfile('bin_files/trendmodrho-exp1.bin', dtype=np.float32)
# h = 4.0 # espessura das camadas elementares para inversao (m) <real> "verificar se está certo"
# nitex = 15 # numero maximo de iteracoes  <integer>
# ermin = 0.1 # espessura das camadas elementares para inversao (m) <real>
# lambda_ = 0.12 # fator de regularizacao  <real> 
# evp = 1.0 # energia de saida de vp relativo
# evs = 1.0 # energia de vs relativo
# erho = 1.0 # energia de den relativo
# freqmax = 249.999958  # ?
# famp = 1.0 # fator de amplitude do pulso sismico <real>
# napulso = 11 # duracao efetiva do pulso (Num. amostras) <integer>
# chsis = 1
# chout = 1

# plt.figure(figsize=(12, 8))
# plt.imshow(realsis, cmap='gray',interpolation='nearest', aspect=0.2)  # 'cmap' specifies the color map
# plt.colorbar()  # adds a color bar to indicate the scale
# plt.title('Matrix Visualization')
# plt.xlabel('Column Index')
# plt.ylabel('Row Index')
# plt.savefig('realsis.pdf', format='pdf')

# plt.figure()
# plt.plot(vp)
# plt.title('VP')
# plt.savefig('vp_plot.pdf', format='pdf')

# plt.figure()
# plt.plot(vs)
# plt.title('VS')
# plt.savefig('vs_plot.pdf', format='pdf')

# plt.figure()
# plt.plot(rho)
# plt.title('rho')
# plt.savefig('rho_plot.pdf', format='pdf')



# # Array initializations 
# # Fortran subroutine expects a single precision floating-point number 
# # (4 bytes per element) for the arrays marked with intent(inout), while 
# # the arrays provided from Python (using NumPy) are in double precision 
# # (8 bytes per element)


# # with open('bin_files/trendmodvp-exp1.bin', 'rb') as file1:
#     # vp = file1.read()
# # with open('bin_files/trendmodvs-exp1.bin', 'rb') as file2:
#     # vs = file2.read()

# # with open('bin_files/modrho-exp1.bin', 'rb') as file3:
#     # rho = file3.read()

# # vp = np.full((na,), 2.0, dtype=np.float32)
# # vs = np.full((na,), 1.0, dtype=np.float32)
# # rho = np.full((na,), 2.0, dtype=np.float32)
# # wavelet=df.to_numpy(dtype=np.float32)
# # wavelet = np.ones(128, dtype=np.float32)
# # wavelet[64] = 0.0  # Adjust for zero-based index

# # Call the subroutine
# # Call the subroutine, ensuring all arrays are properly sized
# try:
#     sisout = avafwi.avafwisub(dtin,dx,na1,na2,x1,nangout1,nangout2,realsis,vp,vs,rho,wavelet,freqmax,famp,napulso,h,nitex,ermin,lambda_,chsis,chout,evp,evs,erho)
# except Exception as e:
#     print("Error calling Fortran subroutine:", e)
#     exit(1)

# # Print outputs for verification
# print('Output VP:', vp)
# print('Output VS:', vs)
# print('Output RHO:', rho)
# print('Output Seismic Data:', sisout[:, 0])  # Print first trace
# # import json

# # # Path to the JSON file
# # file_path = 'parameters.json'

# # # Open the file and load the data
# # with open(file_path, 'r') as file:
# #     data = json.load(file)

# # # Print the data to verify it's loaded correctly
# # print(data)
