import numpy as np
import matplotlib.pyplot as plt

def read_complex_data(filename):
    """
    Reads complex data from a file where each line contains the real and imaginary parts.
    """
    data = np.loadtxt(filename, dtype=np.complex128)
    return data

# Read data from files
box_function_data = read_complex_data('test_fft/box_function.dat')
fft_data = read_complex_data('test_fft/sync_function.dat')

# Prepare the time axis for plotting
n = len(box_function_data)
time = np.arange(n)
# time = np.arange(-64, 64)

# Plot the box function
plt.figure(figsize=(12, 6))
plt.subplot(2, 1, 1)
plt.plot(time, box_function_data.real, label='Real part')
# plt.plot(time, box_function_data.imag, label='Imaginary part', linestyle='--')
plt.title('Box Function')
plt.xlabel('Index')
plt.ylabel('Amplitude')
plt.legend()
plt.grid(True)

# Plot the FFT result
plt.subplot(2, 1, 2)
plt.plot(time, fft_data.real, label='Real part')
# plt.plot(time, fft_data.imag, label='Imaginary part', linestyle='--')
plt.title('FFT Result')
plt.xlabel('Index')
plt.ylabel('Amplitude')
plt.legend()
plt.grid(True)

# Adjust layout and show the plots
plt.tight_layout()
plt.show()
