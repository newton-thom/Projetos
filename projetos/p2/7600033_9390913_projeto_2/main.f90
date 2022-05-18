
PROGRAM SistemaSolar
               implicit none

               !sobre as dimensoes do array r(:,:,:)
               !(r de cada objeto,x,y)xy do plano(forcaRadial)
               !como assumiremos que o sol esta fixo
               ! na origem(hipotese)
               !o unico objeto sera o que orbita
               !mas mesmo assim vou criar essa dimensao
               !no array para fazer alguns testes com
               !mais de uma orbita, para efeito de                               
               !calculos

  REAL(4),PARAMETER:: dt =0.05
  REAL(8):: tau
  INTEGER N,i  
  REAL(4),DIMENSION(:,:),ALLOCATABLE:: r,v
  REAL(4),DIMENSION(:), ALLOCATABLE:: t,modR,modV
  REAL(4):: ri(1:2),anoS
  PARAMETER(anoS = 3.2*(10**7))
  REAL(4),PARAMETER:: yr = 3.2 
  REAL(4),PARAMETER:: G = 1 !
  REAL(4),PARAMETER::PI = 3.14159265
  N=3000

  OPEN(FILE='modulos.dat',UNIT=2,ACTION='write',status='replace')
  OPEN(FILE='espaco.dat',UNIT=1,ACTION='write',status='replace')
  OPEN(file='valores.txt',unit=3)
  allocate(r(1:2,0:N+1))!r(1,i),r(2,i), posicao de (x,y), no tempo i
  allocate(v(1:2,0:N+1))
  allocate(t(0:N+1))
  allocate(modr(0:N+1))
  allocate(modv(0:N+1))


  call layout
  call receba
  call info
  i=0

  modr(0) = modular(r(1,0),r(2,0))
  modv(0) = modular(v(1,0),v(2,0))

  DO WHILE(i.le.N)
    call eulerCrome  
    call calculomod
    i=i+1       
    WRITE(1,*)r(1,i),r(2,i),v(1,i),v(2,i),t(i)
    WRITE(2,*)modR(i),modV(i)           
  ENDDO


  CLOSE(1)
  CLOSE(2)
  CLOSE(3)
  PRINT*,'finish'

  CONTAINS
  ! a funcao 'modular' retorna o modulo de duas componentes
  !a principio para o modulo de R, mas também para 
  !verificar se mod(v) continua constante
  
    FUNCTION modular(x,y)
      real(4):: modular
      real(4),intent(in)::x,y
      modular = SQRT(x**2+y**2)
    END function modular
    
    SUBROUTINE calculomod
    
    modR(i+1) = modular(r(1,i+1),r(2,i+1)) 
    modV(i+1) = modular(v(1,i+1),v(2,i+1))
    
    END SUBROUTINE
    
    SUBROUTINE eulerCrome
      v(1,i+1) = v(1,i) - ((4*(PI**2)*r(1,i))/(modr(i)**3))*dt
      v(2,i+1) = v(2,i) - ((4*(PI**2)*r(2,i))/(modr(i)**3))*dt 
      r(1,i+1) = r(1,i) + v(1,i+1)*dt
      r(2,i+1) = r(2,i) + v(2,i+1)*dt
      t(i+1)= t(i)+dt 
    END SUBROUTINE eulerCrome

    SUBROUTINE layout
      call barra
      PRINT*, "Bem vindo a simulação gravitacional de nossa Estrela"
      call barra
      PRINT*, 'Vamos simular a orbita da Terra e outros & 
              &Planetas em torno do Sol' 
      call barra
      PRINT*, "Digite a quantidades de anos a serem simulados"
      !READ*, n                  
    ENDSUBROUTINE layout        

    SUBROUTINE info
      tau = N*dt
      modr(0) = modular(r(1,0),r(2,0))
      
      print*, 'O periodo de simulacao será :',tau
      call barra
      print*, 'Com ',N,'passos'

    
    ENDSUBROUTINE 

    
    SUBROUTINE receba
      PRINT*,"---  Digite as condicoes iniciais do &
              &objeto"
      call barra
      PRINT*,"DIGITE EM ORDEM SEPARADO POR VIRGULA &
      &OU ENTER AS SEGUINTES CONDICOES"
      PRINT*,"x(t0),y(t0),Vx(t0),Vy(t0)"

      READ*,r(1,0),r(2,0),v(1,0),v(2,0)
      t(0) = dt
      call barra
      PRINT*, 'REGISTRANDO AS SEGUINTES CONDIÇÕES INICIAIS'
      call barra
      WRITE(1,*) r(1,0),r(2,0),v(1,0),v(2,0),t(0)
!:      WRITE(*,*) r(1,0),r(2,0),v(1,0),v(2,0),t(0),'0'
           
    END SUBROUTINE receba
  
    SUBROUTINE barra
      print*,'-------------------------------------------------------'
    end subroutine barra

END PROGRAM SistemaSolar
