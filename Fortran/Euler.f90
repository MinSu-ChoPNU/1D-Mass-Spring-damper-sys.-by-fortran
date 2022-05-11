program Euler
    implicit none
    REAL :: m,k,c,x1,x2,Ft,Ts,Tstart,Tend!x1은 초기위치, x2는 초기 속도, Ft는 초기 힘,Ts는 간격,Tstrat는 시작 시간, Tend는 종료 시간
    REAL :: matA(2,2)
    REAL :: matB(2,1)
    REAL :: matC(2,1)
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

    matA=reshape((/1.00, -(Ts*k)/m, Ts, 1-(Ts*c)/m/),(/2,2/))
    matB=reshape((/0.00,Ts/m/),(/2,1/))
    matC=reshape((/x1,x2/),(/2,1/))
    !Euler 법 계산
    DO l=1,599,1
        matX1(l+1,1)=matA(1,1)*matX1(l,1)+matA(1,2)*matX2(l,1)+matB(1,1)*Ft
        matX2(l+1,1)=matA(2,1)*matX1(l,1)+matA(2,2)*matX2(l,1)+matB(2,1)*Ft
    END DO
    DO i=1,599,1
        write(*,*) (matX1(i+1,1)),(matX2(i+1,1))
    END DO

END program Euler