MODULE REACH_SED
  ! USE DATAWRITE, ONLY : log_process
  IMPLICIT NONE
CONTAINS
  !#################################################################################################################
  SUBROUTINE rch_resuspended(ir,               &
                             it,               &
                             rchclayw,         &
                             rchsiltw,         &
                             rchclaym,         &
                             rchsiltm,         &
                             parpeak,          &
                             parscoef,         &
                             parsexp,          &
                             parrchk,          &
                             parrchc,          &
                             vol,              &
                             riv,              &
                             rivflux,          &
                             sedstate,         &
                             TAU,              &
                             sed_resus,        &
                             sed_settle        &
                             )

    USE MODELTYPE,        ONLY    :    RIVERTYPE,      &
                                       RIVFLUXTYPE
    USE MODELTYPE_SED,    ONLY    :    SEDSTATETYPE
    USE MODELVAR_SED,     ONLY    :    sedresp
    INTEGER,INTENT(IN)                ::  it 
    INTEGER,INTENT(IN)                ::  ir
    TYPE(RIVERTYPE)   , INTENT(IN)    ::  riv
    TYPE(RIVFLUXTYPE) , INTENT(IN)    ::  rivflux
    TYPE(SEDSTATETYPE), INTENT(INOUT) ::  sedstate
    REAL(KIND=8),       INTENT(IN)    ::  parpeak     
    REAL(KIND=8),       INTENT(IN)    ::  parscoef
    REAL(KIND=8),       INTENT(IN)    ::  parsexp  
    REAL(KIND=8),       INTENT(IN)    ::  parrchk
    REAL(KIND=8),       INTENT(IN)    ::  parrchc 
    REAL(KIND=8),       INTENT(IN)    ::  vol      
    REAL(KIND=8),       INTENT(IN)    ::  rchclayw     !<m/d, settling velocity for cohesive sediment fraction 
    REAL(KIND=8),       INTENT(IN)    ::  rchsiltw     !<m/d
    REAL(KIND=8),       INTENT(IN)    ::  rchclaym     !<erodibility coefficient (kg/m2/d)
    REAL(KIND=8),       INTENT(IN)    ::  rchsiltm     !<erodibility coefficient (kg/m2/d)
    REAL(KIND=8),       INTENT(OUT)   ::  TAU          !shear stress (kg/m2)
    REAL(KIND=8),       INTENT(OUT)   ::  sed_resus(4) !<kg, amount of resuspended sediment
    REAL(KIND=8),       INTENT(OUT)   ::  sed_settle(4)!<kg, amount of sediment settling to bed sediment
    
    !>local
    REAL(KIND=8) :: avedepm !average depth of water in RCHRES (m)
    REAL(KIND=8) :: depconc_c,depconc_l !concentration of suspended sediment lost to 
                                        !deposition during a simulation interval
    REAL(KIND=8) :: scrconc_c,scrconc_l
    REAL(KIND=8) :: susp,susp_c,susp_l    !suspended storage of sediment fraction (mg.ft3/l or mg.m3/l)
    ! REAL(KIND=8) :: vol !volume of water in RCHRES (ft3 or m3)
    REAL(KIND=8) :: susp_c_min,susp_l_min
    REAL(KIND=8) :: bed,bed_c,bed_l !storage of sediment fraction in bed (mg.ft3/l or mg.m3/l)
    REAL(KIND=8) :: depsed,depsed_c,depsed_l
    REAL(KIND=8) :: resed,resed_c,resed_l
    REAL(KIND=8) :: gam  ! unit weight, or density, of water (1000 kg/m3)
    REAL(KIND=8) :: grav ! acceleration due to gravity (9.8l m/s2)
    REAL(KIND=8) :: usatr  ! shear velocity (m/s)
    REAL(KIND=8) :: hrad  ! hydraulic radius (m)
    REAL(KIND=8) :: vel !liusu m/s
    REAL(KIND=8) :: depsed_s
    REAL(KIND=8) :: resed_s

    depsed_c=0.;depsed_l=0.
    resed_c=0.;resed_l=0.

    sed_resus = 0.
    sed_settle= 0.

    !------------------
    grav  = 9.81 !m/s2
    gam   = 1000 !kg/m3

    susp_c = sedstate%claypool !kg
    susp_l = sedstate%siltpool

    susp_c_min=sedstate%claypool-0.05* rivflux%VOL(1)/1000
    susp_l_min=sedstate%siltpool-0.1* rivflux%VOL(1)/1000
    
    bed_c  = sedstate%claybed
    bed_l  = sedstate%siltbed
    

    avedepm = vol/(riv%width*riv%length)
    hrad = (avedepm * riv%width)/(2*avedepm+riv%width) !m
    usatr = SQRT(grav*riv%slope*hrad)
    TAU =riv%slope*gam*hrad
 
    !---------caly----------------------
    !TAU<taucd depositon------------------
    IF(TAU .LT. sedresp(ir)%claycd) THEN 
      IF(sedstate%clayconc.gt.0.05)then
        depconc_c = sedstate%clayconc* (1.0 - exp((-rchclayw/avedepm)*(1.0-TAU/sedresp(ir)%claycd)))
        depsed_c = depconc_c *vol *1.e-3 !mg/L * m3 *1.e-3= kg
        IF(depsed_c .GE. susp_c_min) THEN
          depsed_c = susp_c_min 
          susp_c = 0.05* rivflux%VOL(1)/1000 
          bed_c  = bed_c  + depsed_c
        ELSE
          susp_c = susp_c - depsed_c
          bed_c  = bed_c  + depsed_c
        ENDIF
      ENDIF
    !TAU>taucs suspended------------------
    ELSEIF(TAU .GT. sedresp(ir)%claycs) THEN 
      scrconc_c = sedresp(ir)%mclay/avedepm*1000*(TAU/sedresp(ir)%claycs - 1.0) !clay
      resed_c = scrconc_c*vol *1.e-3 !mg/L * m3 *1.e-3= kg
      IF(resed_c .GE. bed_c) THEN
        resed_c = bed_c        
        bed_c  = 0.     
        susp_c = susp_c + resed_c
      ELSE
        bed_c  = bed_c  - resed_c
        susp_c = susp_c + resed_c
      ENDIF
    END IF
  
    !----------silt-----------
    IF(TAU .LT. sedresp(ir)%siltcd) THEN
      IF(sedstate%siltconc.GT.0.1)THEN
        depconc_l = sedstate%siltconc* (1.0 - exp((-rchsiltw/avedepm)*(1.0-TAU/sedresp(ir)%siltcd)))
        depsed_l = depconc_l *vol *1.e-3
        IF(depsed_l .GE. susp_l_min)THEN
          ! depsed_l = susp_l
          ! susp_l=0.
          depsed_l = susp_l_min
          susp_l = 0.1* rivflux%VOL(1)/1000
          bed_l  = bed_l  + depsed_l
        ELSE
          susp_l = susp_l - depsed_l
          bed_l  = bed_l  + depsed_l
        END IF
      ENDIF
    ELSEIF(TAU .GT. sedresp(ir)%siltcs) THEN 
      scrconc_l = sedresp(ir)%msilt/avedepm*1000*(TAU/sedresp(ir)%siltcs - 1.0)
      resed_l = scrconc_l*vol *1.e-3 !mg/L * m3 *1.e-3= kg
      IF(resed_l .GE. bed_l) THEN  
        resed_l = bed_l
        bed_l  = 0.
        susp_l = susp_l + resed_l
      ELSE
        bed_l  = bed_l  - resed_l
        susp_l = susp_l + resed_l
      END IF 
    END IF
    
    !sand------------------
    CALL rch_sand_resp(ir,          &
                       it,          &
                       parpeak,     &
                       parscoef,    &
                       parsexp,     &
                       parrchk,     &
                       parrchc,     &
                       vol,         &
                       rivflux,     &
                       sedstate,    &
                       resed_s,     &
                       depsed_s     &
                       )

    !>amount of resus/settle-----
    sed_resus(1) = resed_c; sed_resus(2) = resed_l; sed_resus(3) = resed_s
    sed_resus(4) = resed_c + resed_l + resed_s

    sed_settle(1)= depsed_c; sed_settle(2)=depsed_l; sed_settle(3)=depsed_s
    sed_settle(4)= depsed_c + depsed_l + depsed_s

    sedstate%claypool = susp_c
    sedstate%siltpool = susp_l
    sedstate%claybed = bed_c
    sedstate%siltbed = bed_l
    sedstate%sedpool = sedstate%claypool+sedstate%siltpool+sedstate%sandpool
    sedstate%sedbed = sedstate%claybed+sedstate%siltbed+sedstate%sandbed

    IF(rivflux%VOL(1).gt.0.)THEN
      sedstate%clayconc =sedstate%claypool*1000./rivflux%VOL(1) !<mg/L
      ! if(sedstate%clayconc.LT.0.05)then
      !  sedstate%clayconc=0.05
       sedstate%claypool=sedstate%clayconc * rivflux%VOL(1)/1000
      ! ENDIF
  
      sedstate%siltconc =sedstate%siltpool*1000./rivflux%VOL(1) 
      ! IF(sedstate%siltconc.LT.0.1)THEN
      !  sedstate%siltconc=0.1
       sedstate%siltpool = sedstate%siltconc * rivflux%VOL(1)/1000
      ! ENDIF
  
      sedstate%sandconc =sedstate%sandpool*1000./rivflux%VOL(1)  
      ! IF(sedstate%sandconc.LT.0.05)THEN
      !  sedstate%sandconc=0.05 
       sedstate%sandpool = sedstate%sandconc * rivflux%VOL(1)/1000
      ! ENDIF
    ELSE
       sedstate%claypool=0.
       sedstate%siltpool=0.
       sedstate%sandpool=0.
       sedstate%sedpool=0.
    ENDIF
    
    !>Update concentration
    sedstate%sedconc = sedstate%clayconc+sedstate%siltconc+sedstate%sandconc

  END SUBROUTINE rch_resuspended
  !#################################################################################################################
  SUBROUTINE rch_sand_resp(ir,           &
                           it,           &
                           parpeak,      &
                           parscoef,     &
                           parsexp,      &
                           parrchk,      &
                           parrchc,      &
                           rchvol,       &
                           rivflux,      &
                           sedstate,     &
                           resus,        &
                           settle        &
                           )
    USE MODELTYPE,      ONLY            :      RIVFLUXTYPE                                         
    USE MODELTYPE_SED,  ONLY            :      SEDSTATETYPE                                                                       
    USE MODELVAR,       ONLY            :      simcfg,         &
                                               num_ant_type,  &
                                               realzero
    USE MODELVAR_SED,   ONLY            :      g_acc
    USE MODELPAR
    INTEGER,INTENT(IN)                  ::    ir 
    INTEGER,INTENT(IN)                  ::    it 
    REAL(KIND=8),       INTENT(IN)      ::    parpeak     
    REAL(KIND=8),       INTENT(IN)      ::    parscoef
    REAL(KIND=8),       INTENT(IN)      ::    parsexp  
    REAL(KIND=8),       INTENT(IN)      ::    parrchk
    REAL(KIND=8),       INTENT(IN)      ::    parrchc       
    REAL(KIND=8),       INTENT(IN)      ::    rchvol                     
    TYPE(RIVFLUXTYPE),  INTENT(IN)      ::    rivflux
    TYPE(SEDSTATETYPE), INTENT(INOUT)   ::    sedstate
    REAL(KIND=8),       INTENT(OUT)     ::    resus
    REAL(KIND=8),       INTENT(OUT)     ::    settle

    REAL(KIND=8)  ::  vel, pvel, sedconc_max, ppconc_max
    REAL(KIND=8)  ::  sedbed(1), sedpool(1),sedpool_min(1)
    REAL(KIND=8)  ::  pp_sed_resp(1), pp_sedbed(1), pppool(1), pp_resp(1)
    REAL(KIND=8)  ::  remove_parant(num_ant_type)
    REAL(KIND=8)  ::  bedpool(num_ant_type),parpool(num_ant_type)
    REAL(KIND=8)  ::  resp(1),sed_resp(1)

    resus=0.
    settle=0.
    sed_resp = 0.
    sedbed  = sedstate%sandbed
    sedpool = sedstate%sandpool
    sedpool_min=sedstate%sandpool-0.05*rivflux%VOL(1)/1000
    vel  = rivflux%vel
    pvel = parpeak * vel !<peak velocity, m/s
    sedconc_max = parscoef*pvel**parsexp*1.E6  !<original kg/L -> mg/L

    IF(sedstate%sandconc.GT.0.05)THEN
      IF(sedstate%sandconc.GT.sedconc_max) THEN !<deposition
        sed_resp = (sedstate%sandconc-sedconc_max)*MAX(rchvol, realzero)/1000. !<mg/l*m3/1000=kg
      ELSEIF(sedstate%sandconc.LT.sedconc_max) THEN !<resuspend
        sed_resp = -1.*(sedconc_max-sedstate%sandconc)*MAX(rchvol, realzero)*parrchk*parrchc/1000. 
      END IF


      IF(sed_resp(1).GT.0) THEN   !<deposition

        IF(sed_resp(1).GT.sedpool_min(1))sed_resp(1)=sedpool_min(1)

        CALL add_pool(1, sedbed(1), sed_resp(1))
        CALL remove_pool(1, sedpool(1), sed_resp(1))
        settle =  sed_resp(1)
      ELSEIF(sed_resp(1).LT.0) THEN 
        resp(1)=-sed_resp(1)
        IF(-sed_resp(1).GT.sedbed(1)) THEN !resuspanded
          sedbed = 0.
          CALL add_pool(1, sedpool(1), resp(1))
        ELSE 
          CALL remove_pool(1, sedbed(1), resp(1))
          CALL add_pool(1, sedpool(1), resp(1))
        END IF
        resus=resp(1)
      END IF

    ENDIF
    sedstate%sandbed =sedbed(1)
    sedstate%sandpool=sedpool(1)

  END SUBROUTINE rch_sand_resp
  !######################################################################################################  
  !####################################################################################################################
  SUBROUTINE advect_sed(ir,            &
                        it,            & 
                        q_up,          &
                        precvol,       &
                        sed_up_c,      &
                        sed_up_l,      &
                        sed_up_s,      &
                        sub_to_riv,    &
                        rivstate,      &
                        rivflux,       &
                        sedstate       &
                        )

    USE MODELTYPE
    USE MODELTYPE_SED,    ONLY    :    SEDSTATETYPE

    INTEGER, INTENT(IN) :: ir
    INTEGER, INTENT(IN) :: it
    ! REAL(KIND=8), INTENT(IN) :: ks
    REAL(KIND=8), INTENT(IN) :: q_up
    REAL(KIND=8), INTENT(IN) :: precvol
    REAL(KIND=8), INTENT(IN) :: sed_up_c
    REAL(KIND=8), INTENT(IN) :: sed_up_l
    REAL(KIND=8), INTENT(IN) :: sed_up_s
    

    TYPE(SUB2RIVTYPE),  INTENT(IN)     ::   sub_to_riv
    TYPE(RIVSTATETYPE), INTENT(INOUT)  ::   rivstate
    TYPE(RIVFLUXTYPE),  INTENT(INOUT)  ::   rivflux
    TYPE(SEDSTATETYPE), INTENT(INOUT)  ::   sedstate

    REAL(KIND=8) :: sedload,ks
    REAL(KIND=8) :: SROVOL, EROVOL,sed_conc
    ks=0.
    SROVOL = 0.
    EROVOL = (1.-ks)*rivstate%Q(2)*3600.*24.  !<m3

    !clay--------------
    sed_conc = (sed_up_c + sedstate%clayconc/1000*(rivflux%VOL(0)-SROVOL))/(rivflux%VOL(1)+EROVOL)  !kg/m3
    ! IF(sed_conc .LT.0.05) sed_conc=0.05
    sedload = SROVOL*sedstate%clayconc/1000. + EROVOL*sed_conc  !<kg   
    sedstate%sedload(1) = sedload  !!kg                                                                                                     
    sedstate%clayconc = sed_conc * 1000.  !!kg/m3*1000 = g/m3
 
    !silt--------------
    sed_conc = (sed_up_l + sedstate%siltconc/1000*(rivflux%VOL(0)-SROVOL))/(rivflux%VOL(1)+EROVOL)
    ! IF(sed_conc .LT.0.1) sed_conc=0.1
    sedload = SROVOL*sedstate%siltconc/1000. + EROVOL*sed_conc  !<kg
    sedstate%sedload(2) = sedload
    sedstate%siltconc = sed_conc* 1000

    !sand--------------    
    sed_conc = (sed_up_s + sedstate%sandconc/1000*(rivflux%VOL(0)-SROVOL))/(rivflux%VOL(1)+EROVOL)
    ! IF(sed_conc .LT.0.1) sed_conc=0.05
    sedload = SROVOL*sedstate%sandconc/1000. + EROVOL*sed_conc  !<kg
    sedstate%sedload(3)=sedload
    sedstate%sandconc=sed_conc*1000

    !sum--------------     
    sedstate%sedload(4) = sedstate%sedload(3) + sedstate%sedload(2) + sedstate%sedload(1)
    sedstate%sedconc = sedstate%clayconc + sedstate%siltconc + sedstate%sandconc
    

    ! IF(sedstate1%clayconc.LT.0.05)sedstate1%clayconc=0.05
    ! IF(sedstate1%siltconc.LT.0.1) sedstate1%siltconc=0.1
    ! IF(sedstate1%sandconc.LT.0.05)sedstate1%sandconc=0.05 
    ! IF(sedstate1%sedconc .LT.0.2) sedstate1%sedconc=0.2

    !!----update sed pool----------------
    IF(rivflux%VOL(1).gt.0.)THEN
      sedstate%claypool = sedstate%clayconc * rivflux%VOL(1)/1000
      sedstate%siltpool = sedstate%siltconc * rivflux%VOL(1)/1000
      sedstate%sandpool = sedstate%sandconc * rivflux%VOL(1)/1000
      sedstate%sedpool  = sedstate%claypool + sedstate%siltpool + sedstate%sandpool
    ELSE
      sedstate%claypool=0.
      sedstate%siltpool=0.
      sedstate%sandpool=0.
      sedstate%sedpool=0.
    ENDIF

    END SUBROUTINE advect_sed
!####################################################################################################################
    SUBROUTINE remove_pool(n,pool,removal)
    
      USE MODELVAR,    ONLY    :     realzero
      !INOUT
      INTEGER, INTENT(IN) :: n                   !<number of soillayers
      REAL(KIND=8), INTENT(INOUT) :: pool(n)     !<soil pool array
      REAL(KIND=8), INTENT(INOUT) :: removal(n)  !<amount to be removed
      
      !Local variables
      REAL(KIND=8) a(n)
  
      a = pool - removal
      WHERE(a .GE. realzero)
        pool = a
      ELSEWHERE
        removal = pool
        pool = 0.
      ENDWHERE
  
    END SUBROUTINE remove_pool
    !################################################################################################################## 
    
     !##################################################################################################################
    SUBROUTINE add_pool(n, pool, source)
  
      INTEGER, INTENT(IN) :: n                          !<number of soillayers
      REAL(KIND=8), INTENT(INOUT) :: pool(n)            !<soil pool array
      REAL(KIND=8), INTENT(IN)    :: source(n)          !<amount to be added
  
      pool(:) = pool(:) + source(:)
  
    END SUBROUTINE add_pool
    !##################################################################################################################
    

END MODULE REACH_SED