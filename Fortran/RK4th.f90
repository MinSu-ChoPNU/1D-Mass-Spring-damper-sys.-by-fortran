program RK4th
    implicit none
    REAL :: m,k,c,x1,x2,Ft,Ts,Tstart,Tend,k1,k2,k3,k4!x1은 초기위치, x2는 초기 속도, Ft는 초기 힘,Ts는 간격,Tstrat는 시작 시간, Tend는 종료 시간
    REAL :: matX1(600,1)
    REAL :: matX2(600,1)
    Integer :: i,l

    !matX 초기화
    DO i=1,600,1
        matX1(i,1)=0.00
        matX2(i,1)=0.00
    END Do
    

    x1=0 !초기 위치
    x2=0 !초기 속도
    matX1(1,1)=x1 !초기 위치 입력
    matX2(1,1)=x2 !초기 속도 입력

    !matX초기화 체크
    !DO i=1,50,1
        !write(*,*) matX1(i,1),matX2(i,1)
    !END DO

    m=20
    k=2
    c=4
    Ft=5
    Ts=0.1
    Tstart=0
    Tend=60


    DO l=1,599,1
        
        !x1에 대해서
        k1=matX2(l,1)
        k2=matX2(l,1)+0.5*k1*Ts
        k3=matX2(l,1)+0.5*k2*Ts
        k4=matX2(l,1)+k3*Ts
        matX1(l+1,1)=matX1(l,1)+(1.0/6.0)*(k1+2*k2+2*k3+k4)*Ts

        !x2에 대해서
        k1=-k/m*(matX1(l,1))-c/m*(matX2(l,1))+Ft/m
        k2=-k/m*(matX1(l,1)+0.5*k1*Ts)-c/m*(matX2(l,1)+0.5*k1*Ts)+Ft/m
        k3=-k/m*(matX1(l,1)+0.5*k2*Ts)-c/m*(matX2(l,1)+0.5*k2*Ts)+Ft/m
        k4=-k/m*(matX1(l,1)+k3*Ts)-c/m*(matX2(l,1)+k3*Ts)+Ft/m
        matX2(l+1,1)=matX2(l,1)+(1.0/6.0)*(k1+2*k2+2*k3+k4)*Ts

    END DO
    open(1, file='RK4th_x1_output.txt') ! 이렇게 치면 output.txt란 파일이생성됨
    open(2, file='RK4th_x2_output.txt') ! 이렇게 치면 output.txt란 파일이생성됨
    DO i=1,599,1
        write(1,*) (matX1(i+1,1))
    END DO
    open(2, file='RK4th_x2_output.txt') ! 이렇게 치면 output.txt란 파일이생성됨
    DO i=1,599,1
        write(2,*) (matX2(i+1,1))
    END DO

    DO i=1,599,1
        write(*,*) (matX1(i+1,1)),(matX2(i+1,1))
    END DO

END program RK4th