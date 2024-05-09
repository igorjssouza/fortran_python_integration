program avafwi

!
! Programa para determinacao dos parametros elasticos de 
! uma camada ( Vp, Vs e Den.) usando a tecnica de Inversao de
! forma de onda de AVA (AVA-WFI). Este programa chama a subrotina 
! avafwisub-v6-2.
!
! Sergio Adriano Moura Oliveira, Marco 2019
!
IMPLICIT NONE

  INTERFACE
     subroutine avafwisub(na,dtin,nx,dx,na1,na2,x1,nangout1,nangout2,     &
                          realsis,vp,vs,rho,wavelet,freqmax,famp,napulso, &
                          h,nitex,ermin,lambda,chsis,sisout,chout,evp,    &
                          evs,erho)
     INTEGER, INTENT(IN) :: na,nx,na1,na2,nangout1,nangout2
     INTEGER, INTENT(IN) :: napulso,nitex,chsis,chout
     REAL, INTENT(IN) :: dtin,x1,dx,freqmax,famp,h,ermin,lambda
     REAL, INTENT(IN) :: evp,evs,erho
     REAL, INTENT(IN) :: realsis(na,nx),wavelet(128)
     REAL, INTENT(INOUT) :: vp(na),vs(na),rho(na)
     REAL, DIMENSION(:,:), INTENT(OUT) :: sisout
     END SUBROUTINE avafwisub
  END INTERFACE

  INTEGER nsis1,nsis2,nam,nang,nang1,nang2,nampulso,nitr 
  INTEGER nam1,nam2,najan,tiposda,recsis
  REAL dt,ang1,dang,fm,ampfactor,dz,miner,alfa,sispulso(128)
  REAL rmsvp,rmsvs,rmsrho,ep1,ep2,df,amp(64)
  COMPLEX wlet(128)
  CHARACTER string*80
  CHARACTER*40  nome,nomevp,nomevs,nomerho,nomesis
  CHARACTER*40  nomesisdif,nomepulso
  CHARACTER*40  nomevpinv,nomevsinv,nomerhoinv
  LOGICAL est
  INTEGER i,j,n,o,tiposis,nanginv,idsis,idsda
  REAL, DIMENSION(:), ALLOCATABLE :: vpn,vsn,rhon
  REAL, DIMENSION(:,:), ALLOCATABLE :: sis,dsis
!
! entrada de parametros
!
      write( *, '(A)', ADVANCE='NO') 'Entre o nome &
           do arquivo de parametros: '
      read*,nome
      print*

      inquire(file=nome,exist=est)
	  if(est.eqv..true.) then
             open(11,file=nome,status='old')
 10          read (11,'(a80)',end=20) string
             write (*,'(a80)') string
             goto 10
 20          rewind 11
             read (11,'(t47,(a))') nomesis
	     read (11,'(t47,i5)') nsis1
	     read (11,'(t47,i5)') nsis2
	     read (11,'(t47,i4)') nam
             read (11,'(t47,f7.2)') dt
	     read (11,'(t47,i4)') nang
             read (11,'(t47,f7.2)') ang1
             read (11,'(t47,f7.2)') dang
             read (11,'(t47,i4)') nam1
             read (11,'(t47,i4)') nam2
             read (11,'(t47,i4)') nang1
             read (11,'(t47,i4)') nang2
             read (11,'(t47,(a))') nomepulso
             read (11,'(t47,(a))') nomevp
	     read (11,'(t47,(a))') nomevs
	     read (11,'(t47,(a))') nomerho
             read (11,'(t47,f7.2)') dz
	     read (11,'(t47,i4)') nitr
             read (11,'(t47,f7.2)') miner        
             read (11,'(t47,f7.2)') alfa
             read (11,'(t47,f7.2)') rmsvp
             read (11,'(t47,f7.2)') rmsvs
             read (11,'(t47,f7.2)') rmsrho
             read (11,'(t47,i4)') tiposda
             read (11,'(t47,i4)') tiposis
             read (11,'(t47,i4)') recsis
	     read (11,'(t47,(a))') nomesisdif
	     read (11,'(t47,(a))') nomevpinv
	     read (11,'(t47,(a))') nomevsinv
	     read (11,'(t47,(a))') nomerhoinv

             close(unit=11)
	  else
	      print*
	      print*, 'arquivo', nome,' nao encontrado !  '   
      endif

      nanginv=nang2-nang1+1
      ampfactor=0.3
!
! alocando espaco para vetores e matrizes
!
     allocate( vpn(nam),vsn(nam),rhon(nam) )
     allocate( sis(nam,nang),dsis(nam,nanginv) )
!
! lendo pulso sismico
!
     open(14,file=nomepulso,status='old')
     do j=1,128
        read(14,*) sispulso(j)
     enddo
     close(unit=14)

!
! Determinacao da duracao efetiva do pulso
!
        ep1=0.
        do i=1,128
           ep1=ep1+sispulso(i)**2
        enddo
        ep2=sispulso(65)**2
        i=0
        do while (ep2/ep1 .LT. 0.9999)
           i=i+1
           ep2=ep2+sispulso(65+i)**2+sispulso(65-i)**2
        enddo
        nampulso=2*i+1
        print*,' '
        print*,' '
        print*, 'duração do pulso sismico (num amostras): ', nampulso
        print*, 'duracao do pulso sismico (mseg.)       : ',dt*nampulso*1000.   
!
! obtendo espectro de amplitude do pulso sismico
!
        do j=1,64
           wlet(j)=cmplx(sispulso(64+j))
           wlet(64+j)=cmplx(sispulso(j)) 
        enddo
        call fork(128,wlet,-1.) ! aplicando fft    
        ep1=0.
        do j=1,64 
           amp(j)=sqrt(real(wlet(j))**2+aimag(wlet(j))**2)
           ep1=ep1+amp(j)**2
        enddo
        ep2=amp(1)**2
        j=1
        do while (ep2/ep1 .LT. 0.999999)
           j=j+1
           ep2=ep2+amp(j)**2
        enddo
        fm=j*1/(128*dt)        
        print*, 'Frequencia maxima do pulso sismico (Hz): ',fm
        print*, ' '
!
! abrindo arquivos de saida
!

!    open(17,file=nomesisdif,status='unknown',form='unformatted',  &
!         access='direct',recl=nam)
 
!    open(18,file='vpinv.bin',status='unknown',form='unformatted',  &
!         access='direct',recl=nam) 

!    open(19,file='vsinv.bin',status='unknown',form='unformatted',  &
!         access='direct',recl=nam)

!    open(20,file='rhoinv.bin',status='unknown',form='unformatted',  &
!         access='direct',recl=nam)

    if (recsis .EQ. 1) then
       open(17,file=nomesisdif,status='unknown',form='unformatted',  &
            access='direct',recl=4*nam)
    endif
 
 
    open(18,file=nomevpinv,status='unknown',form='unformatted',  &
         access='direct',recl=4*nam) 

    open(19,file=nomevsinv,status='unknown',form='unformatted',  &
         access='direct',recl=4*nam)

    open(20,file=nomerhoinv,status='unknown',form='unformatted',  &
         access='direct',recl=4*nam)

!
! Inicio inversao dos conjuntos
!
!
    do idsis=nsis1,nsis2

       idsda=idsis-nsis1+1
!
! lendo sismograma real
!
!      open(16,file=nomesis,status='old',form='unformatted',  &
!           access='direct',recl=nam)
      open(16,file=nomesis,status='old',form='unformatted',  &
           access='direct',recl=4*nam)
      do n=1,nang
         read(16,rec=(idsis-1)*nang+n) (sis(o,n), o=1,nam)
      enddo
      close(unit=16)
!
! lendo arquivos de modelos iniciais
!
!      open(11,file=nomevp,status='old',form='unformatted',   &
!           access='direct',recl=nam)
      open(11,file=nomevp,status='old',form='unformatted',   &
           access='direct',recl=4*nam)
      read(11,rec=idsis) (vpn(j), j=1,nam)
      close(unit=11)


!      open(12,file=nomevs,status='old',form='unformatted',   &
!           access='direct',recl=nam)
      open(12,file=nomevs,status='old',form='unformatted',   &
           access='direct',recl=4*nam)
      read(12,rec=idsis) (vsn(j), j=1,nam)
      close(unit=12)

!      open(13,file=nomerho,status='old',form='unformatted',   &
!           access='direct',recl=nam)
      open(13,file=nomerho,status='old',form='unformatted',   &
           access='direct',recl=4*nam)
      read(13,rec=idsis) (rhon(j), j=1,nam)
      close(unit=13)
!
! inversao do conjunto 
!
      print*,'----------------------------------'
      print*,'invertendo conjunto numero:',idsis


      call avafwisub(nam,dt,nang,dang,nam1,nam2,ang1,nang1,nang2,     &
                     sis,vpn,vsn,rhon,sispulso,fm,ampfactor,nampulso,   &
                     dz,nitr,miner,alfa,tiposis,dsis,tiposda,rmsvp,   &
                     rmsvs,rmsrho)


!      print*,'fim inversao do conjunto numero:',idsis

!
! gravando sismogramas residuais
!

      if (recsis .EQ. 1) then
         do n=1,nanginv
            write(17,rec=(idsda-1)*nanginv+n) (dsis(o,n), o=1,nam)
         enddo
      endif	
!
! gravando vetores solucoes no dado de saida
!
      write(18,rec=idsda) ( vpn(j), j=1,nam )
      write(19,rec=idsda) ( vsn(j), j=1,nam )
      write(20,rec=idsda) ( rhon(j), j=1,nam )


    enddo
!
!
! Fim da inversao dos conjuntos
!
!

    if (recsis .EQ. 1) close(unit=17)   ! fechando arquivos de saida
    close(unit=18)
    close(unit=19)
    close(unit=20)



END program avafwi

