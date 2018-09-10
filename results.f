	
C --------------------------------------------------------------
	SUBROUTINE results(P,CN,Q,Area,tc,xIa,jstype,D,pL,Y,qp,tp,qdepth,
     &  ieroty)
C --------------------------------------------------------------
C	Output summary of hydrology results nicely
C --------------------------------------------------------------
C      version 3.0.1, Last ModIFied: See ModIFications below
C      WRITTEN FOR: ASAE'99 Toronto paper, March 8, 2002 
C      Written by: R. Munoz-Carpena (rmc)   &   J. E. Parsons, BAE (jep)
C                  University of Florida        BAE, NC State University
C                  Gainesville, FL 32611        Raleigh, NC 27695-7625(USA)
C                  e-mail: carpena@ufl.edu      
C --------------------------------------------------------------
	IMPLICIT DOUBLE PRECISION (a-h, o-z)
	CHARACTER*4 stype(5)
	DATA stype/'I','IA','II','III','user'/
C --------------------------------------------------------------
	WRITE(2,*)' '
	WRITE(2,*)' HYDROGRAPH CALCULATION FOR WATERSHED-SCS METHOD'
	WRITE(2,*)' '
	WRITE(2,*)'INPUTS'
	WRITE(2,*)'------'
	WRITE(2,100)P
	WRITE(2,200)stype(jstype)
	WRITE(2,250)D
	WRITE(2,300)CN
	WRITE(2,400)Area
	WRITE(2,500)pL
	WRITE(2,600)Y*100.d0
	WRITE(2,610)ieroty
	WRITE(2,*)' '
	WRITE(2,*)'OUTPUTS'
	WRITE(2,*)'-------'
	WRITE(2,700)Q, Q*Area*10.d0
	WRITE(2,800)xIa
	WRITE(2,900)tc,tc*60.d0
	WRITE(2,*)' '
C --------------------------------------------------------------
100	FORMAT('Storm Rainfall=',f8.2,' mm')
200	FORMAT('SCS storm type= ',a4)
250	FORMAT('Storm duration=',f6.1,' h')
300	FORMAT('SCS Curve number=',f6.1)
400	FORMAT('Watershed area=',f8.2,' ha')
500	FORMAT('Maximum flow path length=',f8.2,' m')
600	FORMAT('Average slope of flow path=',f8.2,' %')
610   FORMAT('MUSLE type=',i3,' where:',/,
     C    2x,'1=Williams, 2=GLEAMS, 3=Foster  (See Manual)')
700	FORMAT('Runoff volume=',f8.2,' mm=',f8.2,' m3')
800	FORMAT('Initial Abstraction=',f8.2,' mm')
900	FORMAT('Concentration time=',f8.2,' h= ',f8.2,' min')

	RETURN
	END SUBROUTINE results
