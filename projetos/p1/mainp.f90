        !este programa quando executado, pergunta ao usuário:
        !as codiçõe iniciais da bola x(t=0),y(t=0)

        !sejam x(t),y(t), as funções que representam a posição da bola
        !no plano cartesiano XY
        !assim como suas devidas derivadas em relação ao tempo 
        


      PROGRAM bilhar
        IMPLICIT NONE

        REAL(4),DIMENSION(:,:),ALLOCATABLE:: pos,vel !vetor rn(x,y)(t)
        REAL(4),DIMENSION(:),ALLOCATABLE:: tempo
        INTEGER,DIMENSION(1:4,1:2):: nBorda!vetor normal da borda      
        
        REAL(4)::x,y,xp,yp,tam !tam referece ao modulo do vetor velo
        REAL(4)::dBorda(1:2) !distancia até a borda mais proxima
                        
        INTEGER:: i, nMesa, nPassos, L
        LOGICAL:: limite,borda

        REAL(4)::dt,periodo
        PARAMETER (L =200)!200 cm, 2 metros         
        
        call iniNormalBorda !incicializa os vetores normais as bordas
               
        call recebeEntradas ! N e dt
       
        ALLOCATE(pos(0:nPassos,1:2))
        ALLOCATE(vel(0:nPassos,1:2))
        ALLOCATE(tempo(0:nPassos))
        OPEN(FILE='dados.txt',UNIT=1,STATUS='REPLACE',ACTION='WRITE')
        OPEN(FILE='mesaQ.txt',UNIT=2,STATUS='REPLACE',ACTION='WRITE')
        
        OPEN(FILE ='mesaE.txt',UNIT=4,STATUS='REPLACE',ACTION='WRITE')


        periodo =nPassos*dt! tempo do sistema

        call layout !introduz, coleta, e escreve nos arquivos abertos
        i=0      
        DO WHILE(i.le.nPassos)
                
                pos(i+1,1)= pos(i,1)+vel(i,1)*dt
                pos(i+1,2)= pos(i,2)+vel(i,2)*dt
                x= pos(i+1,1)
                y= pos(i+1,2)

                !verifica se o proximo passa estaria fora da mesa 

                if((x.ge.(L/2).or.x.le.-L/2).or.&
                        &((y.ge.L/2).or.y.le.-L/2))then
            
            !           borda = check(x,y,N,)
            
                        x =pos(i+1,1)
                        y =pos(i+1,2)
                        dBorda(1) = L/2-x
                        dBorda(2) = L/2-y

                        if(x.ge.L/2.or.x.le.-L/2)then
                        xp = vel(i,1)*(-1)
                                vel(i+1,1)= xp
                                vel(i+1,2)=vel(i,2)
                                tempo(i+1)= tempo(i)+dt
                                !else !case 2:saiu pelo esquerdo
                        endif           !fariamos os mesmos 2 passos para y

                        if(y.ge.L/2.or.y.le.-L/2)then
                                yp = vel(i,2)*(-1)
                                vel(i+1,2) = yp
                                vel(i+1,1) = vel(i,1)
                                tempo(i+1) = tempo(i)+dt        
                        endif                       
                else 
                        vel(i+1,1)= vel(i,1)    !velocidade permanece constante
                        vel(i+1,2)= vel(i,2)    !a menos que...:
                        tempo(i+1) =tempo(i)+dt
                endif

                !WRITE(1,*)pos(i+1,1),pos(i+1,2),vel(i+1),
                i=i+1


                WRITE(1,*)pos(i,1),pos(i,2),vel(i,1),vel(i,2),tempo(i)
        ENDDO
        


        PRINT*, 'finish'
 
        CLOSE(1)

        CONTAINS
                SUBROUTINE recebeEntradas
                WRITE(*,*)'Digite o numero de iterações do problema'
                READ*, nPassos
                WRITE(*,*)'E o intervalo de tempo dt; entre elas'
                READ*,dt
                END SUBROUTINE recebeEntradas     
                               
                SUBROUTINE layout               
                         
                PRINT*, '----------------------------------------------'
                PRINT*,' o perido da simulação será:',& 
                & int(periodo),'segundos'

                PRINT*,'As unidades de espaço serão em cm'
                PRINT*, 'A mesa quadrada tem lado de L= 200 = 2 metros'
                PRINT*, '--------------------------------------------'
                PRINT*, 'Digite a posição inicial da bola, na ordem',&
                & '  de modo:  "x(t=0),y(t=0)" '
                PRINT*, '(x e y devem ser >-L/2  E <L/2)'
                READ*,pos(0,1),pos(0,2)
                WRITE(*,*)'Da imesma forma para Vx(0),Vx(0)'
                READ*,vel(0,1),vel(0,2)
                tempo(0)= 0d0
                WRITE(1,*)'#as colunas representam na ordem:'
                WRITE(1,*)'# x(t)             y(t)             vx(t)'&
                &,'           vy(t)             t'
                WRITE(1,*)pos(0,1),pos(0,2),vel(0,1),vel(0,2),tempo(0)!escreve as
                        !condiçoes iniciais no arquivo de saida txt
                        !na ordem:
                        !x(t),y(t),Vx(t),Vy(t),t(i)
       



                END SUBROUTINE layout

                SUBROUTINE iniNormalBorda
                       nborda(1,:) = (/-1,0/) !borda leste
                       nborda(2,:) = (/0,-1/) !norte
                       nborda(3,:) = (/1,0/)!oeste
                       nborda(4,:) = (/0,1/)!sul              
                END SUBROUTINE iniNormalBorda
                
               
                                    
                        
              END PROGRAM bilhar
