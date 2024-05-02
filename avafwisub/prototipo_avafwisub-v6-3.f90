subroutine avafwisub(na,dtin,nx,dx,na1,na2,x1,nangout1,nangout2,     &
                     realsis,vp,vs,rho,wavelet,freqmax,famp,napulso, &
                     h,nitex,ermin,lambda,chsis,sisout,chout,evp,    &
                     evs,erho)
! USE fftsubs
! USE linpacksubs
! USE splinesubs
!avafwi_interface.f90
! Subrotina para determinar os parametros elasticos de 
! uma camada ( Vp, Vs e Den.) usando a tecnica de Inversao de
! forma de onda de AVA (AVA-WFI). A inversao usa o metodo de Levenberg-Marquadt
! (loop externo) para minimizar o erro quadratico entre o dado observado
! e o calculado. Para solucao do sistema linear de cada iteracao do 
! metodo LM e utilizado o metodo Conjugate Gradient Least Square (loop interno).

! Parametros de entrada:
!
!       realsis    : conjunto de angulos de entrada <real(na,nx)>
!       na         : num. de amostras do conjunto de entrada <integer>
!       dtin       : intervalo de amostragem <real>
!       nx         : num. de angulos do conjunto de entrada <integer>
!       x1         : primeiro angulo(graus) <real>
!       dx         : intervalo entre angulos (graus) <real>
!       na1        : inicio janela temporal de inversao (Num. amostra) <integer>
!       na2        : final  janela temporal de invpersao (Num. amostra) <integer>
!       nangout1   : angulo inicial p/ inversao (Num. traco) <integer>
!       nangout2   : angulo final p/ inversao   (Num. traco) <integer>
!       wavelet    : pulso sismico <real(128)>
!       freqmax    : freq. maxima do pulso sismico (Hz) <real>
!       famp       : fator de amplitude do pulso sismico <real>
!       napulso    : duracao efetiva do pulso (Num. amostras) <integer>
!       vp         : modelo inicial de VP (m/s) <real(na)>
!       vs         : modelo inicial de VS (m/s) <real(na)>
!       rho        : modelo inicial de densidade (Kg/m3) <real(na)>
!       h          : espessura das camadas elementares para inversao (m) <real>
!       nitex      : numero maximo de iteracoes  <integer>
!       ermin      : erro normalizado minimo <real>
!       lambda     : fator de regularizacao  <real>       
!       chsis      : (0) disis=modelado (1) dsis=diferenca <integer>
!       chout      : (0) vp, vs e den relativos (1) vp, vs e den absolutos <integer>
!       evp        : energia de vp relativo 
!       evs        : energia de vs relativo
!       erho       : energia de den relativo
!
! Parametros de saida:    
!
!       vp         : vp  <real(na)>
!       vs         : vs  <real(na)>
!       rho        : densidade  <real(na)>
!       sisout     : sismograma de diferença ou modelado <real(na,nx)>
!
! Observacoes:
!
!       O vetor wavelet deve conter 128 amostras com o tempo zero correspondendo a
!       posicao 65.
!
!
! Sergio Adriano Moura Oliveira,
!
! - Versao primaria;  Novembro 2014
!
! - Atualizacao para incluir parametro razdl; Setembro 2015
!
! - Aprimoramento da rotina de conversao de sismogramas tau-p para conjunto de 
!   angulo; Janeiro 2016 
!
! - Aprimoramento para incluir controle da variação do fator de estabilizacao durante 
!   as iteracoes; Maio 2016
!
! - Modificacao do intervalo de frequencias usadas para calculo dos conjuntos de angulos.
!   Modifiacao do valor de razdl (razdl=1.585). Regularizacao da energia dos tracos dos 
!   parametros de saida; Outubro 2016
!
! - Otimizacao da subrotina para calculo dos sismogragramas diferenciais; Abril 2018
!
IMPLICIT NONE
! declaracao parametros de entrada e saida
  INTEGER, INTENT(IN) :: na,nx,na1,na2,nangout1,nangout2
  INTEGER, INTENT(IN) :: napulso,nitex,chsis,chout
  REAL, INTENT(IN) :: dtin,x1,dx,freqmax,famp,h,ermin,lambda
  REAL, INTENT(IN) :: evp,evs,erho
  REAL, INTENT(IN) :: realsis(na,nx),wavelet(128)
  REAL, INTENT(INOUT) :: vp(na),vs(na),rho(na)
  REAL, DIMENSION(:,:), INTENT(OUT) :: sisout
! fim declaracao parametros de entrada
  INTEGER nc,nz,nitin,nausis,jansuav
  INTEGER i,j,k,l,m,n,o,cont,nl,np,nainv,natsup
  INTEGER nang2,namst,nataper,nrayp,nam2! mod-v6
  INTEGER ncaux,natpaux,tbaux
  INTEGER numfreq,najan,ncam,condconv ! mod-v6
  INTEGER, DIMENSION(:), ALLOCATABLE :: tp,ta,tb,ta2,tb2,pil,pfl
  REAL dz,hpc,vpc,vsc,denc,vpu,vsu,denu
  REAL deltavp,deltavs,deltaden,maxang
  REAL fref,er0,ner,ner2,nermin,b,f,df,a,res,razdl ! mod-v6
  REAL evprelat,evsrelat,erhorelat
  REAL v2,h2,pmax,dp ! mod-v6
  REAL soma,soma1,soma2,soma3,rho1,rho2,pi,rayp,vlambda
  REAL fqp,fqs,suvel(10),energias1,energias2,energraz
  REAL vpi(na),vsi(na),rhoi(na)
  real velpaux(na),velsaux(na),denaux(na)
  REAL, DIMENSION(:), ALLOCATABLE :: angulos,velp,vels,den
  REAL, DIMENSION(:), ALLOCATABLE :: velp2,vels2,den2
  REAL, DIMENSION(:), ALLOCATABLE :: velpmin,velsmin,denmin
  REAL, DIMENSION(:), ALLOCATABLE ::vptemp,vstemp,rhotemp
  REAL, DIMENSION(:), ALLOCATABLE ::vprelat,vsrelat,rhorelat
  REAL, DIMENSION(:), ALLOCATABLE :: velp0,vels0,den0
  REAL, DIMENSION(:), ALLOCATABLE :: vpx,vsx,rhox
  REAL, DIMENSION(:),ALLOCATABLE ::  x,p,p1,p2,p3,s2,r1,r2
  REAL, DIMENSION(:), ALLOCATABLE :: amp,phase,angrd  ! mod-v6
  REAL, DIMENSION(:), ALLOCATABLE :: raypvec,intvelsuav,rmsvel  ! mod-v6
  REAL, DIMENSION(:), ALLOCATABLE :: intvelsuav2,rmsvel2  ! mod-v6
  REAL, DIMENSION(:), ALLOCATABLE :: vtau,vtau2   ! mod-v6
  REAL, DIMENSION(:,:), ALLOCATABLE :: calcsis
  REAL, DIMENSION(:,:), ALLOCATABLE :: sisdifvp,sisdifvs,sisdifrho
  REAL, DIMENSION(:,:), ALLOCATABLE :: dsis,dsis2,sisprep,s,q
  REAL, DIMENSION(:,:), ALLOCATABLE :: MS,MG
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: G1,G2,G3
  COMPLEX vpc1,vpc2,vsc1,vsc2,kr,ui ! mod-v6
  COMPLEX Rd(2,2),Ru(2,2),Td(2,2),Tu(2,2)
  COMPLEX, DIMENSION(:), ALLOCATABLE :: wlet,fasesup ! mod-v6
  CHARACTER string*80
  CHARACTER*40  nome,nomevp,nomevs,nomerho,nomedz,nomesis
  CHARACTER*40  nomesisdif,nomepulso
endsubroutine avafwisub

