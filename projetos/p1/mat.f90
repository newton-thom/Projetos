        PROGRAM alo
              INTEGER,DIMENSION(1:4,1:2):: nborda
               integer n
                 nborda(1,:)= (/-1,0/)
                nborda(2,:)=(/0,-1/)
                nborda(3,:)=(/1,0/)
                nborda(4,:)=(/0,1/)
               
               do n=1,4
                 PRINT*,nborda(n,:)
                 
                enddo


                END PROGRAM alo
