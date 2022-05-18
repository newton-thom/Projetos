PROGRAM desenhaMesaCircular
        REAL(8):: x,y,dy, L
        integer:: N
        L=200
        call circular
CONTAINS

SUBROUTINE circular
OPEN(FILE='mesaC.txt',UNIT=3,ACTION='WRITE')
n=1000!pontos
dy = L/(N)
y=L/2
print*, dy
DO WHILE(y.ge.-L/2)
        x =  SQRT((l/2)**2-y**2)
        WRITE(3,*)x,y
        y=y-dy
ENDDO
DO WHILE(y.le.L/2)
x = - SQRT((l/2)**2-y**2)
WRITE(3,*)x,y
y=y+dy
ENDDO
CLOSE(3)
END SUBROUTINE circular
END PROGRAM desenhaMesaCircular
