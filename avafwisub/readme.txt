Instrunções Parametrização AVAFWI

Campos que devem ser preenchidos:

arquivo do dado de entrada                   : nome do arquivo dos conjuntos de angulos
num. sequecial do primeiro conjunto p/inv.   : numero do primeiro conjunto de ângulo que será invertido
num. sequecial do ultimo conjunto p/inv.     : numero do último conjunto de ângulo que será invertido
num. de amostras do sismograma de entrada    : numero de amostras dos conjuntos de ângulo
arquivo modelo inicial de VP (m/s)           : nome do arquivo do modelo de baixa de Vp
arquivo modelo inicial de VS (m/s)           : nome do arquivo do modelo de baixa de Vs
arquivo modelo inicial de  densidade (Kg/m3) : nome do arquivo do modelo de baixa de Rho
nome arquivo vp inversao                     : nome do arquivo de Vp relativo
nome arquivo vs inversao                     : nome do arquivo de Vs relativo
nome arquivo rho inversao                    : nome do arquivo de Rho relativo

Obs:

 - O arquivo do dado de entrada, assim com os do modelos iniciais devem estar no formato binário.
 - Todos os conjuntos de angulos do arquivo de entrada devem possuir o mesmo número de angulos e o mesmo numero de amostras.
 - Para inverter todos os conjuntos de ângulo do dado de entrada o num. sequecial do primeiro conjunto p/inv. deve ser 1 e 
   num. sequecial do ultimo conjunto p/inv. deve corresponder ao num. total de conjuntos de ângulo. 
 - OS arquivos de saida também são gerados no formato binário.
 - Neste exemplo é assumido que os conjuntos de ângulo estão de cinco em cinco graus e possuem nove traços, mas apenas os
   traços de 1 a 7 (2.5 a 31.5 graus)  serão intertidos.
   
   Para rodar o programa é só digitar na linha de comando avafwiv6-3  dar enter e digitar o nome do arquivo de parâmetros.
   durante a execução é mostrado o statuss da inversão de cada conjunto.
 
 Como converter dado SEGY para o formato binário usando o SU:
 
  1) converter o arquivo do formato SEGY para o formato SU usando o comanto SEGYREAD:
  
                     Exemplo; converter o arquimo sis1.segy para o arquivo sis1.su:   segyread >sis1.su tape=sis1.segy
                     
  2) converter o arquivo SU para o formato binário: sustrip <sis1.su >sis1.bin                           
   
