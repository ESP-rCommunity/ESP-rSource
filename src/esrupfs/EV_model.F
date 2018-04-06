C This file holds data relating to electric vehicles and related code. The vehicle model
C can be used in conjunction with ESP-r's power flow modelling capability. The routines 
C are as follows. 
C POWOC_EV_model     - electric vehicle-to-home model dealing with the presence of the 
C                      vehicle, the state of charge and the charging current/power
C
C EV_state           - calculates the "state" of the electric vehicle _idle_, charge or
C                      absent (logical).
C
C EV_init            - initialised the component's data.
C
C EV_charge_ctl        routine that handles the different charging options for the vehicle.
C
C > EV_fix_charge    - fixed charge rate for the vehicle battery, charge rate set by the
C                      user - typically used during a fast (e.g. 2hr) or trickle (e.g. 8 hr)
C                      charge.
C 
C > EV_opp_charge    -  vehicle only charges when the building demand is low enough (set by user)
C                       or alternatively there is power available from any local renewables. 
C
C > EV_V2H_charge    - the vehicle charges at periods of low power or renewable availability or
C                      alternatively can discharge in order to limit the peak building demand.
C
C EV_trip()          - calculates feasible trips for the electric vehicle.
C 
C EV_SOC()           - calculates the new SOC of the battery based on the trip or charging situation.
C 
C EV_save()          - saves the EV data to the library and transfers boundary conditions, also writes the
C                      component trace if required   
C    
C New routines to be added here <<>>.
C
C
C *POWOC_EVehicle***********************************************************************
C This is the main controlling routine for the EV model. This calculates the status of the
C vehicle and the state of charge of the battery at each simulation timestep
C
C
      SUBROUTINE POWOC_EVEHICLE(IPWC)

#include "building.h"
#include "plant.h"
#include "power.h"

C Trace Commons
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Subroutine POWOC_EVEHICLE'
        WRITE(ITU,*)'Component',IPWC
      ENDIF

      CALL EV_STATE(IPWC)
      CALL EV_TRIP(IPWC)
      CALL EV_SOC(IPWC)
      CALL EV_CHARGE(IPWC)

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Exiting POWOC_EVEHICLE'
      ENDIF

      RETURN 
      END
 
C *EV_STATE***************************************************************************
C Calculates the current status of the EV and sets this to one of three states. 
C present_charging, present_idle or absent. The routine also tracks how long a particular
C state has persisted.

      SUBROUTINE EV_STATE(IPWC)

#include "building.h"
#include "plant.h"
#include "power.h"

C Trace Commons
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

C The current state of the EV.
      COMMON/EVSTATE1/EV_STATUSF(MPOWCOM),EV_STATUSP(MPOWCOM)

C Lockout for presence and absence - minimum stay and minimum trip.
      COMMON/EVDATA1/STAY_LOCKOUT(MPOWCOM),ABSENT_LOCKOUT(MPOWCOM)

      COMMON/EVDATA2/ANNUAL_DIST(MPOWCOM),AVE_TRIP(MPOWCOM),
     &DISTEQ_A0(MPOWCOM),DISTEQ_A1(MPOWCOM),DISTEQ_A2(MPOWCOM),
     &DISTEQ_A3(MPOWCOM),MAX_SPEED(MPOWCOM)

C Trip probabilites for up to different time periods of the day.
      COMMON/EVDATA3/TRIP_PROBWK(MPOWCOM,6),
     &TRIP_PROBSAT(MPOWCOM,6),TRIP_PROBSUN(MPOWCOM,6)

C Probability modifier if car is charging (less likely to be driven)
      COMMON/EVSTATE3/PROB_CHARGE_MOD(MPOWCOM) 

C Period definition - up to six periods in each day type may be defined and 3
C holiday periods when the car will be abssent.
      COMMON/EVDATA4/NWKPER(MPOWCOM),WKPERS(MPOWCOM,6),NSATPER(MPOWCOM),
     &SATPERS(MPOWCOM,6),NSUNPER(MPOWCOM),SUNPERS(MPOWCOM,6)

      COMMON/EVDATA5/NOHOLPEREV(MPOWCOM),EVHOLPERS(MPOWCOM,3),
     &EVHOLPERE(MPOWCOM,3)

C Charge, idle and trip duration counters.
      COMMON/EVSTATE4/CHARGE_DUR(MPOWCOM),TRIP_DUR(MPOWCOM),
     &IDLE_DUR(MPOWCOM)

C Data recording for the vehicle. 
c      COMMON/EVSTATE5/NO_TRIP(MPOWCOM),TOT_TRIP_DURATION(MPOWCOM)

C Initialization status of power-only components
      COMMON/ENETPOWCOMP/BCOMP_INITIALIZED(MPOWCOM)

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      REAL CHARGE_DUR,
     &TRIP_DUR,IDLE_DUR,PROB_CHARGE_MOD,WKPERS,SATPERS,SUNPERS,
     &TRIP_PROBWK,TRIP_PROBSAT,TRIP_PROBSUN,STAY_LOCKOUT,ABSENT_LOCKOUT,
     &NOM_TRIP,MAX_SPEED

      INTEGER NOHOLPEREV,NWKPER,NSATPER,NSUNPER,EVHOLPERS,
     &EVHOLPERE

      LOGICAL BCOMP_INITIALIZED,SATURDAY,SUNDAY,WEEKDAY,HOLIDAY
      
      CHARACTER*6 EV_STATUSF,EV_STATUSP ! CHARGE,  ABSENT OR _IDLE_

C Check for initialisation of the EV model.
      IF(.NOT.BCOMP_INITIALIZED(IPWC))THEN 

C Initialise the EV data.
        CALL EV_INIT(IPWC)

C Temporary trace header.
        WRITE(88,*) 
     &'  time  |status|typ|  SOC   |   P    |    Q   |  trip  '

        BCOMP_INITIALIZED(IPWC)=.TRUE.   
      ENDIF

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Subroutine EV_State'
      ENDIF

C Store the previous state
      EV_STATUSP(IPWC)=EV_STATUSF(IPWC)      

C Generate a test value between 0. and 1.
  
      TEST_VAL=RAND_NO()    

C Check for day type.
      IF(IDWF.EQ.6)THEN
        SATURDAY=.TRUE.
        SUNDAY=.FALSE.
        WEEKDAY=.FALSE.
      ELSEIF(IDWF.EQ.7)THEN
        SATURDAY=.FALSE.
        SUNDAY=.TRUE.
        WEEKDAY=.FALSE.
      ELSE
        SATURDAY=.FALSE.
        SUNDAY=.FALSE.
        WEEKDAY=.TRUE.
      ENDIF

C Check for holiday period <<should really move into the mainstream of ESP-r>>
C Holiday period implies a period when the car is away.
      HOLIDAY=.FALSE.
      DO 5 IHOL=1,NOHOLPEREV(IPWC)
        IF(IDYF.GE.EVHOLPERS(IPWC,IHOL).AND.IDYF.LT.
     &EVHOLPERE(IPWC,IHOL))THEN
          HOLIDAY=.TRUE.
          SATURDAY=.FALSE.
          SUNDAY=.FALSE.
          WEEKDAY=.FALSE.
        ENDIF
  5   CONTINUE


C Set the Nominal number of trips (2-leg) per day.
      NOM_TRIP=ANNUAL_DIST(IPWC)/(365.*AVE_TRIP(IPWC))       
    
C Work out the current period
      IF(WEEKDAY)THEN
        DO 7 IPER=1,NWKPER(IPWC)-1
          IF(ENETIMEF.GE.WKPERS(IPWC,IPER).AND.
     &    ENETIMEF.LT.WKPERS(IPWC,IPER+1))THEN
            INPER=IPER
          ENDIF
  7     CONTINUE

C Check if in final trip period
        IF(ENETIMEF.GE.WKPERS(IPWC,NWKPER(IPWC)))THEN
          INPER=NWKPER(IPWC)
        ENDIF

C Set going or leaving  probabilities.
         PROB_THRESH=TRIP_PROBWK(IPWC,INPER)*NOM_TRIP*ENETSTP/3600.

      ELSEIF(SATURDAY)THEN
        DO 8 IPER=1,NSATPER(IPWC)-1
          IF(ENETIMEF.GE.SATPERS(IPWC,IPER).AND.
     &    ENETIMEF.LT.SATPERS(IPWC,IPER+1))THEN
            INPER=IPER
          ENDIF
  8     CONTINUE

C Check if in final trip period
        IF(ENETIMEF.GE.SATPERS(IPWC,NSATPER(IPWC)))THEN
          INPER=NSATPER(IPWC)
        ENDIF

C Set going or leaving probabilities.
        PROB_THRESH=TRIP_PROBSAT(IPWC,INPER)*NOM_TRIP*ENETSTP/3600.

      ELSEIF(SUNDAY)THEN
        DO 9 IPER=1,NSUNPER(IPWC)-1
          IF(ENETIMEF.GE.SUNPERS(IPWC,IPER).AND.
     &    ENETIMEF.LT.SUNPERS(IPWC,IPER+1))THEN
            INPER=IPER 
          ENDIF          
  9     CONTINUE

C Check if in final draw period
        IF(ENETIMEF.GE.SUNPERS(IPWC,NSUNPER(IPWC)))THEN
          INPER=NSUNPER(IPWC)
        ENDIF

C Set going or leaving  probabilities.
c        PROB_THRESH=TRIP_PROBSUN(IPWC,INPER)*2.*NOM_TRIP
         PROB_THRESH=TRIP_PROBSUN(IPWC,INPER)*NOM_TRIP*ENETSTP/3600.
      ELSEIF(HOLIDAY)THEN

C In a holiday period - car away.
        PROB_THRESH=-1.0E-6
        EV_STATUSP(IPWC)='ABSENT' 
      ENDIF


      IF(EV_STATUSP(IPWC).EQ.'CHARGE')THEN
C Car is present and charging, test for car leaving. 
         CHARGE_DUR(IPWC)=CHARGE_DUR(IPWC)+ENETSTP

C Apply charging modifier to the probability of the car leaving. 
         PROB_THRESH=PROB_THRESH*PROB_CHARGE_MOD(IPWC)

C Check if in stay lockout.
         IF(CHARGE_DUR(IPWC).LT.STAY_LOCKOUT(IPWC)) PROB_THRESH=-1.0E-6 !needs revisiting
         IF(TEST_VAL.LE.PROB_THRESH)THEN 
           EV_STATUSF(IPWC)='ABSENT' 

         ELSE
           EV_STATUSF(IPWC)='CHARGE' 
         ENDIF   

      ELSEIF(EV_STATUSP(IPWC).EQ.'_IDLE_')THEN
        IDLE_DUR(IPWC)=IDLE_DUR(IPWC)+ENETSTP

C Check if in stay lockout.
         IF(IDLE_DUR(IPWC).LT.STAY_LOCKOUT(IPWC)) PROB_THRESH=-1.0E-6

C Car is present, test for car leaving.
         IF(TEST_VAL.LE.PROB_THRESH)THEN 
           EV_STATUSF(IPWC)='ABSENT'
         ELSE
           EV_STATUSF(IPWC)='_IDLE_'          
         ENDIF   
      ELSE !car absent

C Check if in trip lockout.
        IF(TRIP_DUR(IPWC).LT.ABSENT_LOCKOUT(IPWC)) PROB_THRESH=-1.0E-6

C Car is away determine if the car will return, if car returns, charge state is later determined by 
C the controller.
        IF(TEST_VAL.LE.PROB_THRESH)THEN
          EV_STATUSF(IPWC)='_IDLE_'
        ELSE
          EV_STATUSF(IPWC)='ABSENT'
        ENDIF
      ENDIF

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Variables:  '
        WRITE(ITU,*)'Status F    ',EV_STATUSF(IPWC)
        WRITE(ITU,*)'Status P    ',EV_STATUSP(IPWC)
        IF(SATURDAY) WRITE(ITU,*)'Day         ',"Saturday"
        IF(SUNDAY) WRITE(ITU,*)'Day         ',"Sunday"
        IF(WEEKDAY) WRITE(ITU,*)'Day         ',"Weekday"
        IF(HOLIDAY) WRITE(ITU,*)'Day         ',"Holiday"
        WRITE(ITU,*)'Exiting EV_State'
      ENDIF 

      RETURN 
      END

C *EV_INIT****************************************************************************
C This routine initialised the simulation data for each EV

      SUBROUTINE EV_INIT(IPWC)
#include "building.h"
#include "plant.h"
#include "power.h"

C Trace Commons
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/EVSTATE1/EV_STATUSF(MPOWCOM),EV_STATUSP(MPOWCOM)

C Lockout for presence and absence - minimum stay and minimum trip.
      COMMON/EVSTATE2/STAY_LOCKOUT(MPOWCOM),ABSENT_LOCKOUT(MPOWCOM)

C Trip probabilites for up to different time periods of the day.
      COMMON/EVDATA3/TRIP_PROBWK(MPOWCOM,6),
     &TRIP_PROBSAT(MPOWCOM,6),TRIP_PROBSUN(MPOWCOM,6)


C Period definition - up to six periods in each day type may be defined and 3
C holiday periods when the car will be abssent.
      COMMON/EVDATA4/NWKPER(MPOWCOM),WKPERS(MPOWCOM,6),
     &NSATPER(MPOWCOM),SATPERS(MPOWCOM,6),NSUNPER(MPOWCOM),
     &SUNPERS(MPOWCOM,6)

      COMMON/EVDATA5/NOHOLPEREV(MPOWCOM),EVHOLPERS(MPOWCOM,3),
     &EVHOLPERE(MPOWCOM,3)


C Charge, idle and trip duration counters.
      COMMON/EVSTATE4/CHARGE_DUR(MPOWCOM),TRIP_DUR(MPOWCOM),
     &IDLE_DUR(MPOWCOM)

C Data recording for the vehicle. 
      COMMON/EVSTATE5/NO_TRIP(MPOWCOM),TOT_TRIP_DURATION(MPOWCOM)

C EV SOC
      COMMON/EVSTATE6/EV_SOCF(MPOWCOM),EV_SOCP(MPOWCOM)

      COMMON/EVDATA6/BATTERY_CAP(MPOWCOM),KWH_KM(MPOWCOM),
     &KWH_HR(MPOWCOM),CHARGE_EFF(MPOWCOM),MIN_SOC(MPOWCOM),
     &PROB_CHARGE_MOD(MPOWCOM)

      COMMON/EVDATA2/ANNUAL_DIST(MPOWCOM),AVE_TRIP(MPOWCOM),
     &DISTEQ_A0(MPOWCOM),DISTEQ_A1(MPOWCOM),DISTEQ_A2(MPOWCOM),
     &DISTEQ_A3(MPOWCOM),MAX_SPEED(MPOWCOM)


      COMMON/EVRECORD1/TOT_TRIP_DURP(MPOWCOM),TOT_TRIP_DURF(MPOWCOM)
      COMMON/EVRECORD2/TOT_TRIP_DISTP(MPOWCOM),TOT_TRIP_DISTF(MPOWCOM)

      COMMON/EVDATA8/NCHARGEDATA(MPOWCOM),CHARGE_CTL_DATA(MPOWCOM,20)   

      CHARACTER*6 EV_STATUSP, EV_STATUSF

      INTEGER EVHOLPERS,EVHOLPERE

      REAL MIN_SOC,PROB_CHARGE_MOD,MAX_SPEED,KWH_KM,KWH_HR,CHARGE_DUR,
     &IDLE_DUR,TRIP_DUR,EV_SOCP,EV_SOCF

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Subroutine EV_init'
      ENDIF

      EV_STATUSF(IPWC)="_IDLE_"
      EV_STATUSP(IPWC)="_IDLE_"
      TOT_TRIP_DURP(IPWC)=0.
      TOT_TRIP_DURF(IPWC)=0.
      TOT_TRIP_DISTP(IPWC)=0.
      TOT_TRIP_DISTF(IPWC)=0.
      IDATC=1 !initialise the data counter (component has variable number of data items)

C EV Battery data. 
      BATTERY_CAP(IPWC)=POWCDAT(IPWC,IDATC) !Battery capacity (kWh)
      IDATC=IDATC+1
      KWH_KM(IPWC)=POWCDAT(IPWC,IDATC) !Battery discharge kWh per km of travel (inc. discharge eff.)
      IDATC=IDATC+1
      KWH_HR(IPWC)=POWCDAT(IPWC,IDATC) !Loss due to battery parasitic losses  kWh/h.
      IDATC=IDATC+1
      CHARGE_EFF(IPWC)=POWCDAT(IPWC,IDATC) !Used to calculate the charge power and current. 
      IDATC=IDATC+1
      EV_SOCF(IPWC)=POWCDAT(IPWC,IDATC) !Initial state of charge.
      EV_SOCP(IPWC)=EV_SOCF(IPWC)
      IDATC=IDATC+1
      MIN_SOC(IPWC)=POWCDAT(IPWC,IDATC) !used to calculate a feasible trip.
      IDATC=IDATC+1
      PROB_CHARGE_MOD(IPWC)=POWCDAT(IPWC,IDATC)
      IDATC=IDATC+1

C Trip distance calculation.
      ANNUAL_DIST(IPWC)=POWCDAT(IPWC,IDATC) !Annual car use (km)
      IDATC=IDATC+1
      AVE_TRIP(IPWC)=POWCDAT(IPWC,IDATC)    !A trip is an outward journey and return (2-legs)
      IDATC=IDATC+1
      DISTEQ_A0(IPWC)=POWCDAT(IPWC,IDATC) !Distance for a trip calculated based on exponential function 
      IDATC=IDATC+1                  !D=A3*P^3+A2*P^2+A1*P, where P is a  <=0 random number <=1
      DISTEQ_A1(IPWC)=POWCDAT(IPWC,IDATC) !The distance for a leg is therefore D/2
      IDATC=IDATC+1
      DISTEQ_A2(IPWC)=POWCDAT(IPWC,IDATC)
      IDATC=IDATC+1
      DISTEQ_A3(IPWC)=POWCDAT(IPWC,IDATC)
      IDATC=IDATC+1

C Other trip data
      STAY_LOCKOUT(IPWC)=POWCDAT(IPWC,IDATC)
      IDATC=IDATC+1
      ABSENT_LOCKOUT(IPWC)=POWCDAT(IPWC,IDATC) 
      IDATC=IDATC+1
      MAX_SPEED(IPWC)=POWCDAT(IPWC,IDATC) 
      IDATC=IDATC+1

C Holiday periods
      NOHOLPEREV(IPWC)=NINT(POWCDAT(IPWC,IDATC))
      IDATC=IDATC+1

      DO 100 IHOLP=1,NOHOLPEREV(IPWC)
        EVHOLPERS(IPWC,IHOLP)=NINT(POWCDAT(IPWC,IDATC))
        IDATC=IDATC+1
        EVHOLPERE(IPWC,IHOLP)=NINT(POWCDAT(IPWC,IDATC))
        IDATC=IDATC+1
  100 CONTINUE

C Time periods and trip probability     
      NWKPER(IPWC)=NINT(POWCDAT(IPWC,IDATC))
      IDATC=IDATC+1

      DO 101 IWKP=1,NWKPER(IPWC)
        WKPERS(IPWC,IWKP)=POWCDAT(IPWC,IDATC)
        IDATC=IDATC+1
        TRIP_PROBWK(IPWC,IWKP)=POWCDAT(IPWC,IDATC)
        IDATC=IDATC+1
101   CONTINUE

      NSATPER(IPWC)=NINT(POWCDAT(IPWC,IDATC))
      IDATC=IDATC+1

      DO 102 ISTP=1,NSATPER(IPWC)
        SATPERS(IPWC,ISTP)=POWCDAT(IPWC,IDATC)
        IDATC=IDATC+1
        TRIP_PROBSAT(IPWC,ISTP)=POWCDAT(IPWC,IDATC)
        IDATC=IDATC+1
102   CONTINUE

      NSUNPER(IPWC)=NINT(POWCDAT(IPWC,IDATC))
      IDATC=IDATC+1

      DO 103 ISNP=1,NSUNPER(IPWC)
        SUNPERS(IPWC,ISNP)=POWCDAT(IPWC,IDATC)
        IDATC=IDATC+1
        TRIP_PROBSUN(IPWC,ISNP)=POWCDAT(IPWC,IDATC) !last data item - do not increment counter
        IDATC=IDATC+1 
103   CONTINUE  

      NCHARGEDATA(IPWC)=NINT(POWCDAT(IPWC,IDATC))
      IDATC=IDATC+1 
      DO 104 ICTD=1,NCHARGEDATA(IPWC)
        CHARGE_CTL_DATA(IPWC,ICTD)=POWCDAT(IPWC,IDATC) 
        IDATC=IDATC+1   
104   CONTINUE   

C Read and initialise the random seed
      ISEED=NINT(POWCDAT(IPWC,IDATC))
      CALL SRAND(ISEED)  

      IF(ITRACE(2).GT.0)THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Variables:  '
        WRITE(ITU,*)'Status F    ',EV_STATUSF(IPWC)
        WRITE(ITU,*)'Status P    ',EV_STATUSP(IPWC)
        WRITE(ITU,*)'Duration P  ',TOT_TRIP_DURP(IPWC)
        WRITE(ITU,*)'Duration F  ',TOT_TRIP_DURF(IPWC)
        WRITE(ITU,*)'Distance P  ',TOT_TRIP_DISTP(IPWC)
        WRITE(ITU,*)'Distance F  ',TOT_TRIP_DISTF(IPWC)
        WRITE(ITU,*)'Capacity    ',BATTERY_CAP(IPWC)   
        WRITE(ITU,*)'kWh/km      ',KWH_KM(IPWC)
        WRITE(ITU,*)'kWh/hr      ',KWH_HR(IPWC)
        WRITE(ITU,*)'Charge eff  ',CHARGE_EFF(IPWC)
        WRITE(ITU,*)'SOC init    ',EV_SOCF(IPWC)
        WRITE(ITU,*)'SOC min     ',MIN_SOC(IPWC)
        WRITE(ITU,*)'Charge prob ',PROB_CHARGE_MOD(IPWC)
        WRITE(ITU,*)'Yearly use  ',ANNUAL_DIST(IPWC)
        WRITE(ITU,*)'Ave trip    ',AVE_TRIP(IPWC)  
        WRITE(ITU,*)'Dist eqn a0 ',DISTEQ_A0(IPWC)
        WRITE(ITU,*)'Dist eqn a1 ',DISTEQ_A1(IPWC)
        WRITE(ITU,*)'Dist eqn a2 ',DISTEQ_A2(IPWC)
        WRITE(ITU,*)'Dist eqn a3 ',DISTEQ_A3(IPWC)
        WRITE(ITU,*)'Max speed   ',MAX_SPEED(IPWC)
        WRITE(ITU,*)'Lockout sty ',STAY_LOCKOUT(IPWC)
        WRITE(ITU,*)'Lockupt abs ',ABSENT_LOCKOUT(IPWC)

        WRITE(ITU,*)'No hol per ',NOHOLPEREV(IPWC)
        DO 200 IHOLP=1,NOHOLPEREV(IPWC)
          WRITE(ITU,*)'Hol per   ',IHOLP
          WRITE(ITU,*)' -> start ',EVHOLPERS(IPWC,IHOLP)
          WRITE(ITU,*)' -> end   ',EVHOLPERE(IPWC,IHOLP)
  200   CONTINUE

        WRITE(ITU,*)'No wk  per ',NWKPER(IPWC)
        DO 201 IWKP=1,NWKPER(IPWC)
          WRITE(ITU,*)'Wk per    ',IWKP
          WRITE(ITU,*)' -> start ',WKPERS(IPWC,IWKP)
          WRITE(ITU,*)' -> prob. ',TRIP_PROBWK(IPWC,IWKP)
201     CONTINUE

        WRITE(ITU,*)'No sat per ',NSATPER(IPWC)
        DO 202 ISTP=1,NSATPER(IPWC)
          WRITE(ITU,*)'Wk per    ',ISTP
          WRITE(ITU,*)' -> start ',SATPERS(IPWC,ISTP)
          WRITE(ITU,*)' -> prob. ',TRIP_PROBSAT(IPWC,ISTP)
202     CONTINUE

        WRITE(ITU,*)'No sun per ',NSUNPER(IPWC)
        DO 203 ISNP=1,NSUNPER(IPWC)
          WRITE(ITU,*)'Wk per    ',ISNP
          WRITE(ITU,*)' -> start ',SUNPERS(IPWC,ISNP)
          WRITE(ITU,*)' -> prob. ',TRIP_PROBSUN(IPWC,ISNP)
203     CONTINUE

        WRITE(ITU,*)'No ctl data ',NCHARGEDATA(IPWC)
        DO 204 ICTD=1,NCHARGEDATA(IPWC)
          WRITE(ITU,*)' -> Ctl data ',CHARGE_CTL_DATA(IPWC,ICTD)
204     CONTINUE   
        WRITE(ITU,*)'Exiting EV_init'
      ENDIF   
        
      RETURN
      END
        

C *EV_TRIP****************************************************************************
C This routine calculates the trip distance for the EV and also checks trip feasibility.
C Statistical data to calibrate this model for the UK is available from the DfT

      SUBROUTINE EV_TRIP(IPWC)

#include "building.h"
#include "plant.h"
#include "power.h"

C Trace Commons
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/EVSTATE1/EV_STATUSF(MPOWCOM),EV_STATUSP(MPOWCOM)

      COMMON/EVDATA2/ANNUAL_DIST(MPOWCOM),AVE_TRIP(MPOWCOM),
     &DISTEQ_A0(MPOWCOM),DISTEQ_A1(MPOWCOM),DISTEQ_A2(MPOWCOM),
     &DISTEQ_A3(MPOWCOM),MAX_SPEED(MPOWCOM)

C Charge, idle and trip duration counters.
      COMMON/EVSTATE4/CHARGE_DUR(MPOWCOM),TRIP_DUR(MPOWCOM),
     &IDLE_DUR(MPOWCOM)

C Data recording for the vehicle. 
      COMMON/EVSTATE5/NO_TRIP(MPOWCOM),TOT_TRIP_DURATION(MPOWCOM)

C Data recording for the vehicle. 
      COMMON/EVSTATE7/TOT_TRIP_DURP(MPOWCOM),TOT_TRIP_DURF(MPOWCOM)
      COMMON/EVSTATE8/TOT_TRIP_DISTP(MPOWCOM),TOT_TRIP_DISTF(MPOWCOM),
     &TRIP_DIST(MPOWCOM,999)

      REAL TOT_TRIP_DURP,TOT_TRIP_DURF,TOT_TRIP_DISTP,TOT_TRIP_DISTF,
     &ANNUAL_DIST,AVE_TRIP,DISTEQ_A0,DISTEQ_A1,DISTEQ_A2,DISTEQ_A3,DIST,
     &MAX_SPEED,CHARGE_DUR,IDLE_DUR,TRIP_DUR

      CHARACTER*6 EV_STATUSF,EV_STATUSP

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Subroutine EV_trip'
      ENDIF

      IF(EV_STATUSF(IPWC).EQ.'_IDLE_'.AND.EV_STATUSP(IPWC).EQ.
     &'ABSENT')THEN


        TRIP_DUR(IPWC)=TRIP_DUR(IPWC)+ENETSTP !seconds.

C All car trips are assumed to have an out/in, both being of equal distance.
        DIST=1000.

C Calculate a *feasibile* trip. The trip distance cannot be greater
C than the maximum speed of the vehicle x the trip duration. Trip distance
C also checked by SOC calculation and updated if necessary. 

        ICOU=1
        DO 10 WHILE(((MAX_SPEED(IPWC)*(TRIP_DUR(IPWC)/3600.)).LT.DIST)
     &.AND.(ICOU.LE.10))
          TEST_VAL=RAND_NO()
          DIST=DISTEQ_A3(IPWC)*TEST_VAL**3.+DISTEQ_A2(IPWC)*TEST_VAL**2.
     &+DISTEQ_A1(IPWC)*TEST_VAL+DISTEQ_A0(IPWC)   
          ICOU=ICOU+1      
  10    CONTINUE

C If dist is still infeasible, set to feasible value.
        IF((MAX_SPEED(IPWC)*(TRIP_DUR(IPWC)/3600.)).LT.DIST) 
     &DIST=MAX_SPEED(IPWC)*(TRIP_DUR(IPWC)/3600.)
        
C Reset the trip duration counter and update simulation trip data.
        NO_TRIP(IPWC)=NO_TRIP(IPWC)+1
        TRIP_DIST(IPWC,NO_TRIP(IPWC))=DIST
        TOT_TRIP_DURP(IPWC)=TOT_TRIP_DURF(IPWC)
        TOT_TRIP_DURF(IPWC)=TOT_TRIP_DURP(IPWC)+TRIP_DUR(IPWC)
        TOT_TRIP_DISTP(IPWC)=TOT_TRIP_DISTF(IPWC)
        TOT_TRIP_DISTF(IPWC)=TOT_TRIP_DISTP(IPWC)+DIST
        TRIP_DUR(IPWC)=0.

      ELSEIF(EV_STATUSF(IPWC).EQ.'ABSENT')THEN !check
        TRIP_DUR(IPWC)=TRIP_DUR(IPWC)+ENETSTP
      ENDIF 

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)'STATUS          ',EV_STATUSF(IPWC)
        WRITE(ITU,*)'TEST_VAL        ',TEST_VAL
        WRITE(ITU,*)'DIST            ',DIST
        WRITE(ITU,*)'NO_TRIP         ',NO_TRIP(IPWC)
        WRITE(ITU,*)'TOT_TRIP_DURF   ',TOT_TRIP_DURF(IPWC)
        WRITE(ITU,*)'TOT_TRIP_DURP   ',TOT_TRIP_DURP(IPWC)
        WRITE(ITU,*)'TOT_TRIP_DISTF  ',TOT_TRIP_DISTF(IPWC)
        WRITE(ITU,*)'TOT_TRIP_DISTP  ',TOT_TRIP_DISTP(IPWC)
        WRITE(ITU,*)'Exiting EV_trip'
      ENDIF

      RETURN 
      END

C *EV_CHARGE**************************************************************************
C 
C This routine calculates the charge delivered to the battery depending upon the 
C Charging mode.
C 1 - 'dumb' charge whenever the car returns.
C 2 - 'dumb' charge whenever the car returns below a threshold SOC.
C 3 - timed charge (w/ start time)
C 4 - opportune charge when load falls below a user defined limit*
C 5 - co-operative charge when the battery discharged to keep load below a limit*

      SUBROUTINE EV_CHARGE(IPWC)

#include "building.h"
#include "plant.h"
#include "power.h"

C Trace Commons
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/EVSTATE1/EV_STATUSF(MPOWCOM),EV_STATUSP(MPOWCOM)

      COMMON/EVDATA6/BATTERY_CAP(MPOWCOM),KWH_KM(MPOWCOM),
     &KWH_HR(MPOWCOM),CHARGE_EFF(MPOWCOM),MIN_SOC(MPOWCOM),
     &PROB_CHARGE_MOD(MPOWCOM)

C Charging control data.
      COMMON/EVDATA7/CHARGE_TYPE(MPOWCOM),CHARGE_KWH(MPOWCOM)
      COMMON/EVDATA8/NCHARGEDATA(MPOWCOM),CHARGE_CTL_DATA(MPOWCOM,20)   

C EV SOC
      COMMON/EVSTATE6/EV_SOCF(MPOWCOM),EV_SOCP(MPOWCOM)

      COMMON/EVSTATE8/TOT_TRIP_DISTP(MPOWCOM),TOT_TRIP_DISTF(MPOWCOM),
     &TRIP_DIST(MPOWCOM,999)

      REAL CHARGE_CTL_DATA,CHARGE_SENS,CHARGE_ACT,CHARGE_V_NOM,
     &CHARGE_I_MAX,CHARGE_KWH,EV_SOCF,EV_SOCP,
     &MIN_SOC,PROB_CHARGE_MOD,MAX_SPEED,KWH_KM,KWH_HR,TOT_TRIP_DISTP,
     &TOT_TRIP_DISTF,TRIP_DIST

      CHARACTER*6 EV_STATUSF,EV_STATUSP

      INTEGER CHARGE_TYPE,NCHARGEDATA

      SMALL=1.0E-6

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Subroutine EV_charge'
      ENDIF

C If the car is not present then exit.  
      PPOWOC(IPWC)=0.
      QPOWOC(IPWC)=0. 
      CHARGE_KWH(IPWC)=0.
   
      IF(EV_STATUSF(IPWC).EQ.'ABSENT') THEN 

C Trace temporary.
      WRITE(88,*) 
     &ENETIMEF,EV_STATUSF(IPWC),CHARGE_TYPE(IPWC),EV_SOCF(IPWC),
     &PPOWOC(IPWC),QPOWOC(IPWC),TOT_TRIP_DISTF(IPWC)

C Do nothing when car is absent.
        RETURN      
      ELSE

      CHARGE_TYPE(IPWC)=NINT(CHARGE_CTL_DATA(IPWC,1))

C Slow of fast 'fixed' charge, charge when car returns or whenever a SOC threshold  OR
C A timed charge.
    
      CHARGE_I_MAX=CHARGE_CTL_DATA(IPWC,2)
      CHARGE_V_NOM=CHARGE_CTL_DATA(IPWC,3)

      IF((CHARGE_TYPE(IPWC).GT.0).AND.(CHARGE_TYPE(IPWC).LE.3))THEN

        CHARGE_V=CHARGE_V_NOM !user can set a nominal charge voltage or set this to -1.0 to pick up nodal voltage

C Use the connected nodal voltage.
        IF(CHARGE_V_NOM.LT.0.0)THEN

C <<pick up nodal voltage here.>>
        IF(CHARGE_V.LT.SMALL) CHARGE_V=CHARGE_V_NOM
        ENDIF

        IF(CHARGE_TYPE(IPWC).EQ.1)THEN

C Charging when car is present.
          IF(EV_SOCF(IPWC).LT.100.)THEN
            EV_STATUSF(IPWC)='CHARGE'
          ELSE
            CHARGE_KWH(IPWC)=0.0
            EV_STATUSF(IPWC)='_IDLE_'
          ENDIF
          CHARGE_KWH(IPWC)=((CHARGE_I_MAX*CHARGE_V)
     &-(1000.*KWH_HR(IPWC)))*(ENETSTP/(3600.*1000.))         
          SOCF=100.*(CHARGE_KWH(IPWC)/BATTERY_CAP(IPWC))+EV_SOCF(IPWC)
          IF(SOCF.GE.100.)THEN
            CHARGE_KWH(IPWC)=
     &BATTERY_CAP(IPWC)*(1.-(EV_SOCF(IPWC)/100.))
          ENDIF
          
        ELSEIF(CHARGE_TYPE(IPWC).EQ.2)THEN

C Charging once battery is depleted below a threshold.
          SOC_THRESH=CHARGE_CTL_DATA(IPWC,3)
          IF(EV_SOCF(IPWC).LT.SOC_THRESH)THEN
            EV_STATUSF(IPWC)='CHARGE'
            CHARGE_KWH(IPWC)=((CHARGE_I_MAX*CHARGE_V)
     &-(1000.*KWH_HR(IPWC)))*(ENETSTP/(3600.*1000.))         
            SOCF=100.*(CHARGE_KWH(IPWC)/BATTERY_CAP(IPWC))+EV_SOCF(IPWC)
            IF(SOCF.GE.100.)THEN
              CHARGE_KWH(IPWC)=
     &BATTERY_CAP(IPWC)*(1.-(EV_SOCF(IPWC)/100.))
              EV_STATUSF(IPWC)='_IDLE_'
            ENDIF
          ELSE
            EV_STATUSF(IPWC)='_IDLE_'
            CHARGE_KWH(IPWC)=0.0
          ENDIF
        ELSE !charge type 3

C Time-based charging after a particular period.
          TIME_THRESH=CHARGE_CTL_DATA(IPWC,3)
          IF(ENETIMEF.GE.TIME_THRESH)THEN
            EV_STATUSF(IPWC)='CHARGE'
            CHARGE_KWH(IPWC)=((CHARGE_I_MAX*CHARGE_V)
     &-(1000.*KWH_HR(IPWC)))*(ENETSTP/1000.)         
            SOCF=100.*(CHARGE_KWH(IPWC)/BATTERY_CAP(IPWC))+EV_SOCF(IPWC)
            IF(SOCF.GE.100.)THEN
              CHARGE_KWH(IPWC)=
     &BATTERY_CAP(IPWC)*(1.-(EV_SOCF(IPWC)/100.))
              EV_STATUSF(IPWC)='_IDLE_'
            ENDIF
          ELSE
            EV_STATUSF(IPWC)='_IDLE_'
            CHARGE_KWH(IPWC)=0.0
          ENDIF
        ENDIF ! end charge type 3
      ELSE

c      CHARGE_KWH(IPWC)=0.0

      ENDIF 

C Assign the power flows to the electrical network.
      IF(CHARGE_EFF(IPWC).GT.1.0E-06)THEN
C Account for charge/discharge inefficiency
        PPOWOC(IPWC)=(-1000.*CHARGE_KWH(IPWC)/(ENETSTP/3600.))/
     &(CHARGE_EFF(IPWC)/100.)
      ELSE
        PPOWOC(IPWC)=-1000.*CHARGE_KWH(IPWC)/(ENETSTP/3600.)
      ENDIF
      QPOWOC(IPWC)=0.

      ENDIF !check for absence if statement

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)'Status         ',EV_STATUSF(IPWC)
        WRITE(ITU,*)'Charge type    ',CHARGE_TYPE(IPWC)

        WRITE(ITU,*)'No ctl data    ',NCHARGEDATA(IPWC)
        DO 204 ICTD=1,NCHARGEDATA(IPWC)
          WRITE(ITU,*)' -> Ctl data ',CHARGE_CTL_DATA(IPWC,ICTD)
204     CONTINUE   
        WRITE(ITU,*)'SOCf           ',EV_SOCF(IPWC)
        WRITE(ITU,*)'Charge kWh     ',CHARGE_KWH(IPWC)
        WRITE(ITU,*)'P Charge W     ',PPOWOC(IPWC)
        WRITE(ITU,*)'Q Charge VAR   ',QPOWOC(IPWC)
        WRITE(ITU,*)'Exiting EV_charge'
      ENDIF

C Trace temporary.
      WRITE(88,*) 
     &ENETIMEF,EV_STATUSF(IPWC),CHARGE_TYPE(IPWC),EV_SOCF(IPWC),
     &PPOWOC(IPWC),QPOWOC(IPWC),TOT_TRIP_DISTF(IPWC)

      RETURN 
      END


C *EV_SOC*****************************************************************************
C
C This routine updates the state of charge for the EV battery. 
C
      SUBROUTINE EV_SOC(IPWC)

#include "building.h"
#include "plant.h"
#include "power.h"

C Trace Commons
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

C The current state of the EV.
      COMMON/EVSTATE1/EV_STATUSF(MPOWCOM),EV_STATUSP(MPOWCOM)

      COMMON/EVDATA6/BATTERY_CAP(MPOWCOM),KWH_KM(MPOWCOM),
     &KWH_HR(MPOWCOM),CHARGE_EFF(MPOWCOM),MIN_SOC(MPOWCOM),
     &PROB_CHARGE_MOD(MPOWCOM)

C Charging control data.
      COMMON/EVDATA7/CHARGE_TYPE(MPOWCOM),CHARGE_KWH(MPOWCOM)

C EV SOC
      COMMON/EVSTATE6/EV_SOCF(MPOWCOM),EV_SOCP(MPOWCOM)

      COMMON/EVSTATE8/TOT_TRIP_DISTP(MPOWCOM),TOT_TRIP_DISTF(MPOWCOM),
     &TRIP_DIST(MPOWCOM,999)



      REAL SOCF,EV_SOCP, EV_SOCF,CHARGE_KWH,TOT_TRIP_DISTF,
     &TOT_TRIP_DISTP,TRIP_DIST,MIN_SOC,KWH_HR,KWH_KM

      INTEGER CHARGE_TYPE

      CHARACTER*6 EV_STATUSF,EV_STATUSP

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Subroutine EV_SOC'
      ENDIF

C Store the future value of SOC prior to updating
      EV_SOCP(IPWC)=EV_SOCF(IPWC)
      SOCF=EV_SOCF(IPWC)
      IF(EV_STATUSF(IPWC).EQ.'CHARGE')THEN
        SOCF=100.*(CHARGE_KWH(IPWC)/BATTERY_CAP(IPWC))+EV_SOCF(IPWC)
        IF(SOCF.GT.100.) SOCF=100.

C Calculate the discharge according to the distance travelled.
      ELSEIF(EV_STATUSF(IPWC).EQ.'_IDLE_')THEN
        IF(EV_STATUSP(IPWC).EQ.'ABSENT')THEN
          DIST=TOT_TRIP_DISTF(IPWC)-TOT_TRIP_DISTP(IPWC) 
          DISCHARGE=DIST*KWH_KM(IPWC)
          SOCF=EV_SOCF(IPWC)-100.*(DISCHARGE/BATTERY_CAP(IPWC))

C Check that the distance is feasible according to the MIN_SOC. If not then adjust the distance.
          IF(SOCF.LE.MIN_SOC(IPWC))THEN
            SOCF=MIN_SOC(IPWC)
            DISTADJ=(((EV_SOCF(IPWC)-MIN_SOC(IPWC))/100.)
     &*BATTERY_CAP(IPWC))/KWH_KM(IPWC)
            TOT_TRIP_DISTF(IPWC)=TOT_TRIP_DISTP(IPWC)+DISTADJ
          ENDIF

        ELSE

C Account for any parasitic losses.
          SOCF=EV_SOCP(IPWC)-100*((KWH_HR(IPWC)*ENETSTP/3600.)/
     &BATTERY_CAP(IPWC))
        ENDIF
      ENDIF

C Update the SOC to the new value.
      EV_SOCF(IPWC)=SOCF 

      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)'SOCf          ',EV_SOCF(IPWC)
        WRITE(ITU,*)'SOCp          ',EV_SOCP(IPWC)
        WRITE(ITU,*)'Exiting EV_SOC'
      ENDIF

      RETURN
      END

      
      
