MODULE INITIALIZE_ANT
  IMPLICIT NONE
CONTAINS 
!######################################################################################################
   SUBROUTINE initialize_ant_var
    USE MODELVAR, ONLY: num_ant_type,           &
                        num_cell,                &
                        sim_length,              &
                        num_sub,                 &
                        bulkdensity,             &
                        dp,riverflux,river,      &
                        max_soillayer,num_riv,num_landuse
    USE MODELVAR_SED, ONLY : sedstate
    USE MODELVAR_ANT, ONLY: antusage,                &
                            antbalan,                &
                            peosource,           &
                            animsource,          &
                            peo,                 &
                            sulo_sub,            &
                            solo_sub,            &
                            gwlo_sub,            &
                            palo_sub,            &
                            antsub2riv,          &
                            antistate,           &
                            airdrydep,           &
                            id_dc,               &
                            id_otc,              &
                            id_tc,               &
                            id_ctc,              &
                            antpool,&
                            solrunoff_sub,source_sub,leach_sub,surfer_sub,gw_sub,eroed_sub,init_sol,&
                            end_sol,&
                            decay_sol,&
                            wetdep,deg_sub,init_sub,end_sub,&
                            rchdeg,rchburl,rchmass,rchload,rchout,rchstart,rchend,rch1,&
                            path_sub,celllo,cellloyr

    INTEGER iant,isub
    ! REAL(KIND=8) :: sol_hlife   
    ! REAL(KIND=8) :: par_hlife
    ! REAL(KIND=8) :: aqu_hlife
    REAL(KIND=8) :: fer_hlife,ly0_hlife

    IF(.NOT.ALLOCATED(wetdep)) ALLOCATE(wetdep(num_ant_type))
    IF(.NOT.ALLOCATED(airdrydep)) ALLOCATE(airdrydep(num_ant_type))
      airdrydep(id_tc) = 280.*12./365   !!< mg/km2/d  ng/g*g/m2/yr/365  ng/m2=mg/km2
      airdrydep(id_otc)= 820.*12./365   !< mg/km2/d
      airdrydep(id_ctc)= 970.*12./365   !< mg/km2/d
      airdrydep(id_dc) = 690.*12./365   !< mg/km2/d


      IF(.NOT.ALLOCATED(path_sub)) ALLOCATE(path_sub(num_ant_type,num_sub,sim_length,4,num_landuse),source=0._dp)

      IF(.NOT.ALLOCATED(source_sub)) ALLOCATE(source_sub(num_ant_type,num_sub,sim_length,3),source=0._dp)
      !<num_ant_type,num_sub,sim_length,type  1<animal 2<people 3<air
      IF(.NOT.ALLOCATED(solrunoff_sub)) ALLOCATE(solrunoff_sub(num_ant_type,num_sub,sim_length,max_soillayer))
      IF(.NOT.ALLOCATED(leach_sub)) ALLOCATE(leach_sub(num_ant_type,num_sub,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(deg_sub)) ALLOCATE(deg_sub(num_ant_type,num_sub,sim_length,2),source=0._dp)
      IF(.NOT.ALLOCATED(end_sub)) ALLOCATE(end_sub(num_ant_type,num_sub,sim_length,2),source=0._dp)
      IF(.NOT.ALLOCATED(init_sub)) ALLOCATE(init_sub(num_ant_type,num_sub,sim_length,2),source=0._dp)
      IF(.NOT.ALLOCATED(rchdeg)) ALLOCATE(rchdeg(num_riv,num_ant_type,sim_length,2),source=0._dp)
      IF(.NOT.ALLOCATED(rchburl)) ALLOCATE(rchburl(num_riv,num_ant_type,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(rchmass)) ALLOCATE(rchmass(num_riv,num_ant_type,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(rchload)) ALLOCATE(rchload(num_riv,num_ant_type,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(rchout)) ALLOCATE(rchout(num_riv,num_ant_type,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(rchstart)) ALLOCATE(rchstart(num_riv,num_ant_type,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(rchend)) ALLOCATE(rchend(num_riv,num_ant_type,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(rch1)) ALLOCATE(rch1(num_riv,num_ant_type),source=0._dp)
      IF(.NOT.ALLOCATED(celllo)) ALLOCATE(celllo(num_ant_type,num_cell,sim_length,5),source=0._dp)
      IF(.NOT.ALLOCATED(cellloyr)) ALLOCATE(cellloyr(num_ant_type,num_cell,5),source=0._dp)

      IF(.NOT.ALLOCATED(surfer_sub)) ALLOCATE(surfer_sub(num_ant_type,num_sub,sim_length))
      IF(.NOT.ALLOCATED(gw_sub)) ALLOCATE(gw_sub(num_ant_type,num_sub,sim_length))
      IF(.NOT.ALLOCATED(eroed_sub)) ALLOCATE(eroed_sub(num_ant_type,num_sub,sim_length))
      IF(.NOT.ALLOCATED(init_sol)) ALLOCATE(init_sol(num_ant_type,num_cell,0:max_soillayer),source=0._dp)
      IF(.NOT.ALLOCATED(end_sol)) ALLOCATE(end_sol(num_ant_type,num_cell,0:max_soillayer),source=0._dp)
      IF(.NOT.ALLOCATED(decay_sol)) ALLOCATE(decay_sol(num_ant_type,sim_length,num_cell,0:max_soillayer),source=0._dp)
      



    IF(.NOT.ALLOCATED(antbalan)) ALLOCATE(antbalan(num_ant_type))
    IF(.NOT.ALLOCATED(peosource)) ALLOCATE(peosource(num_cell,num_ant_type))
    IF(.NOT.ALLOCATED(animsource)) ALLOCATE(animsource(num_cell,num_ant_type))
    IF(.NOT.ALLOCATED(peo)) ALLOCATE(peo(num_cell,num_ant_type))

    IF(.NOT.ALLOCATED(sulo_sub)) ALLOCATE(sulo_sub(num_ant_type,num_sub,sim_length),source=0._dp) 
    IF(.NOT.ALLOCATED(solo_sub)) ALLOCATE(solo_sub(num_ant_type,num_sub,sim_length),source=0._dp) 
    IF(.NOT.ALLOCATED(gwlo_sub)) ALLOCATE(gwlo_sub(num_ant_type,num_sub,sim_length),source=0._dp) 
    IF(.NOT.ALLOCATED(palo_sub)) ALLOCATE(palo_sub(num_ant_type,num_sub,sim_length),source=0._dp) 

    IF(.NOT.ALLOCATED(antsub2riv)) ALLOCATE(antsub2riv(num_sub))
    IF(.NOT.ALLOCATED(antistate)) ALLOCATE(antistate(num_sub))
    DO isub = 1, num_sub
      IF(.NOT.ALLOCATED(antsub2riv(isub)%suload)) ALLOCATE(antsub2riv(isub)%suload(num_ant_type,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(antsub2riv(isub)%soload)) ALLOCATE(antsub2riv(isub)%soload(num_ant_type,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(antsub2riv(isub)%gwload)) ALLOCATE(antsub2riv(isub)%gwload(num_ant_type,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(antsub2riv(isub)%load))   ALLOCATE(antsub2riv(isub)%load(num_ant_type,sim_length),source=0._dp)
      IF(.NOT.ALLOCATED(antsub2riv(isub)%paload)) ALLOCATE(antsub2riv(isub)%paload(num_ant_type,sim_length),source=0._dp)
    END DO

    DO isub = 1, num_sub
      IF(.NOT.ALLOCATED(antistate(isub)%srbconc)) ALLOCATE(antistate(isub)%srbconc(num_ant_type,4))
      IF(.NOT.ALLOCATED(antistate(isub)%conc)) ALLOCATE(antistate(isub)%conc(num_ant_type))
      IF(.NOT.ALLOCATED(antistate(isub)%parconc)) ALLOCATE(antistate(isub)%parconc(num_ant_type))

         antistate(isub)%srbconc(:,1)  =4* 1.E-3 !�?<ng/g *1.e-3 = mg/kg
         antistate(isub)%srbconc(:,2)  =3* 1.E-3
         antistate(isub)%srbconc(:,3)  =1* 1.E-3
         antistate(isub)%srbconc(:,4)  =8* 1.E-3
!test  初值从8->4
        antistate(isub)%conc(id_tc)  = 9.2*1.e-6  !<TC      !< ng/L *1,e-6 = mg/L
        antistate(isub)%conc(id_otc) = 4.2*1.e-6  !<CTC  
        antistate(isub)%conc(id_ctc) = 8.5*1.e-6  !<OTC 
        antistate(isub)%conc(id_dc)  = 5.7*1.e-6  !<DC 

        antistate(isub)%parconc(id_tc )  = 2 *1.e-3  !<TC      !<ng/g *1.e-3 = mg/kg
        antistate(isub)%parconc(id_otc)  = 3 *1.e-3  !<CTC  
        antistate(isub)%parconc(id_ctc)  = 2 *1.e-3  !<OTC 
        antistate(isub)%parconc(id_dc )  = 4 *1.e-3  !<DC 
        
        IF(.NOT.ALLOCATED(antistate(isub)%bedconc)) ALLOCATE(antistate(isub)%bedconc(num_ant_type))
        ! antistate(isub)%bedconc(id_tc )  = 2 * 1.E-3  !<TC !!ng/g->mg/kg
        ! antistate(isub)%bedconc(id_ctc)  = 3 * 1.E-3 !<CTC 
        ! antistate(isub)%bedconc(id_otc)  = 2 * 1.E-3 !<OTC 
        ! antistate(isub)%bedconc(id_dc )  = 4 * 1.E-3 !<DC


        !----test 提高bedconc的浓�?--------------
        antistate(isub)%bedconc(id_tc )  = 100 * 1.E-3  !<TC !!ng/g->mg/kg
        antistate(isub)%bedconc(id_ctc)  = 100 * 1.E-3 !<CTC 
        antistate(isub)%bedconc(id_otc)  = 100 * 1.E-3 !<OTC 
        antistate(isub)%bedconc(id_dc )  = 100 * 1.E-3 !<DC
        !-------------------------------------------------------------------------------------------------
        IF(.NOT.ALLOCATED(antistate(isub)%outant))   ALLOCATE(antistate(isub)%outant(num_ant_type,sim_length),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%outant_p)) ALLOCATE(antistate(isub)%outant_p(num_ant_type),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%outant_d)) ALLOCATE(antistate(isub)%outant_d(num_ant_type),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%spsantmass)) ALLOCATE(antistate(isub)%spsantmass(num_ant_type,4),source = 0._dp)!!mg
        IF(.NOT.ALLOCATED(antistate(isub)%disantmass)) ALLOCATE(antistate(isub)%disantmass(num_ant_type),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%chantmass)) ALLOCATE(antistate(isub)%chantmass(num_ant_type))!!mg
        IF(.NOT.ALLOCATED(antistate(isub)%sedantmass)) ALLOCATE(antistate(isub)%sedantmass(num_ant_type))!!mg
        IF(.NOT.ALLOCATED(antistate(isub)%deg)) ALLOCATE(antistate(isub)%deg(num_ant_type,sim_length,2),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%bural)) ALLOCATE(antistate(isub)%bural(num_ant_type,sim_length),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%mass)) ALLOCATE(antistate(isub)%mass(num_ant_type,sim_length,2),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%outant_p1)) ALLOCATE(antistate(isub)%outant_p1(num_ant_type,sim_length),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%outant_d1)) ALLOCATE(antistate(isub)%outant_d1(num_ant_type,sim_length),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%load)) ALLOCATE(antistate(isub)%load(num_ant_type,sim_length),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%antconc)) ALLOCATE(antistate(isub)%antconc(num_ant_type,sim_length),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%antex)) ALLOCATE(antistate(isub)%antex(num_ant_type,sim_length,2,4),source = 0._dp)
        IF(.NOT.ALLOCATED(antistate(isub)%antdif)) ALLOCATE(antistate(isub)%antdif(num_ant_type,sim_length),source = 0._dp)

        antistate(isub)%spsantmass(:,1)=antistate(isub)%srbconc(:,1)*sedstate(isub)%claypool
        antistate(isub)%spsantmass(:,2)=antistate(isub)%srbconc(:,2)*sedstate(isub)%siltpool
        antistate(isub)%spsantmass(:,3)=antistate(isub)%srbconc(:,3)*sedstate(isub)%sandpool
        antistate(isub)%spsantmass(:,4)=antistate(isub)%srbconc(:,4)*sedstate(isub)%sedpool

        DO iant=1,num_ant_type
          antistate(isub)%chantmass(iant)= antistate(isub)%conc(iant)*river(isub)%vol*1.E3+&
                                           antistate(isub)%parconc(iant)*15.*river(isub)%vol *1.E-3 !<mg, mg/kg*mg/L * m3
          antistate(isub)%sedantmass(iant)=antistate(isub)%bedconc(iant)*&
                                           bulkdensity*0.2*river(isub)%width*river(isub)%length !<mg
          antistate(isub)%disantmass(iant)= antistate(isub)%conc(iant)*river(isub)%vol*1.E3
                                            !<mg, mg/kg*mg/L * m3  
                              
        END DO
      END DO

    ! IF(.NOT.ALLOCATED(antpool)) ALLOCATE(antpool(max_soillayer))
    ! DO ily = 1, max_soillayer
    IF(.NOT.ALLOCATED(antpool%m_dissol)) ALLOCATE(antpool%m_dissol(num_ant_type,0:max_soillayer))
    IF(.NOT.ALLOCATED(antpool%m_sorbed)) ALLOCATE(antpool%m_sorbed(num_ant_type,0:max_soillayer))
    IF(.NOT.ALLOCATED(antpool%im_dissol))ALLOCATE(antpool%im_dissol(num_ant_type,0:max_soillayer))
    IF(.NOT.ALLOCATED(antpool%im_sorbed))ALLOCATE(antpool%im_sorbed(num_ant_type,0:max_soillayer))
    ! END DO








    DO iant=1,num_ant_type
      ! sol_hlife = antusage(iant)
      ! par_hlife = antusage(iant)%Hlife_par
      ! aqu_hlife = antusage(iant)%Hlife_aqu 
      fer_hlife = antusage(iant)%DTmanure
      ! ly0_hlife = antusage(iant)%Hlife_sol

      IF (fer_hlife > 0.) THEN
        antbalan(iant)%decay_f = Exp(-.693 / fer_hlife)
      ELSE  
        antbalan(iant)%decay_f = 0.
      ENDIF
    ENDDO

    CALL get_peo_antinput(peosource)

    CALL get_animal_input(animsource)

  END SUBROUTINE initializE_ant_var
  !##########################################################################################################

  !##########################################################################################################
  SUBROUTINE gridsearch_peo(c,searchRadius,gridSize,period,peoinput)

    USE MODELVAR, ONLY : nrow_grid, &
                         ncol_grid, &
                         num_cell,  &
                         num_ant_type,  &
                         outpeo,    &
                         landuse

    USE MODELVAR_ANT,ONLY :    people,    &
                               antusage,     &
                               antcropdata,  &
                               antbalan
                         
                         
    INTEGER,INTENT(IN) :: c 
    REAL,  INTENT(IN) :: searchRadius
    REAL,  INTENT(IN) :: gridSize
    INTEGER,  INTENT(IN) :: period
    REAL(KIND = 8),  INTENT(INOUT) :: peoinput(num_cell,num_ant_type)!!mg/km2/d
  
    

    !>local 
    REAL(KIND=8) :: Uhuman !!g/peo/yr
    REAL(KIND=8) :: Ehuman 
    REAL(KIND=8) :: subarea
    REAL(KIND=8) :: decay_f
    REAL(KIND=8):: emh(num_ant_type)!dim->crop*2;dan:autumn,shuang:winter
    REAL(KIND=8):: dist
    INTEGER:: count
    INTEGER mday1,mday2     !>cultivation dates
    INTEGER        :: popula,iant,ic,k,ik,iland
    INTEGER        :: days,days1,days2
    INTEGER        :: centerRow, centerCol
    INTEGER        :: i, j, rowMin, rowMax, colMin, colMax
    INTEGER, ALLOCATABLE :: incircle(:)
    ! INTEGER, PARAMETER :: nrow_grid=248,ncol_grid=258
    subarea = 0.
    ik  = 0
    emh = 0.
    count = 0
    mday1=0;mday2=0
    !---------------
    IF(MOD(c,ncol_grid) .NE. 0) THEN
      centerRow = INT(c/ncol_grid) + 1
      centerCol = MOD(c,ncol_grid)
    ELSE
      centerRow = INT(c/ncol_grid)
      centerCol = ncol_grid
    ENDIF
    ! �?定可能在圆内的栅格的范围
    rowMin = MAX(1, centerRow - CEILING(searchRadius/gridSize))
    rowMax = MIN(nrow_grid, centerRow + CEILING(searchRadius/gridSize))
    colMin = MAX(1, centerCol - CEILING(searchRadius/gridSize))
    colMax = MIN(ncol_grid, centerCol + CEILING(searchRadius/gridSize))
    ! ! 预估圆内�?能的最大栅格数
    IF(.NOT.ALLOCATED(incircle)) ALLOCATE(incircle((rowMax-rowMin+1)*(colMax-colMin+1)))
    incircle = 0
    
    !-----------------
    DO i = rowMin, rowMax
      DO j = colMin, colMax
        dist = SQRT(REAL((i - centerRow)**2 + (j - centerCol)**2)) * gridSize
        IF (dist <= searchRadius) THEN
          ic = 258*(i-1)+j
          iland = landuse(ic)
          if(iland .EQ. 2 .OR. iland .EQ. 4) then 
            subarea = subarea + 0.0025
            count = count + 1
            incircle(count) = ic
          END if
        ENDIF
      ENDDO
    ENDDO
    !---------------
    ! subarea = 0.0025 *count
    popula = people(c)
    ! write(6,*)people(c)
    DO k = 1,3
      
       !>rice (4-paddyfield)and wheat(2-dryland)
      IF(iland.EQ.4) THEN  !
        IF(k.NE.1) CYCLE 
      ELSEIF(iland.EQ.2) THEN 
        IF(k.NE.2) CYCLE 
      ELSE
        CYCLE
      END IF

      mday1 = antcropdata(k)%manday1        !autumn ploughing
      mday2 = antcropdata(k)%manday2        !winter crops sowing date
      days1 = MOD(mday1,period)
      days2 = MOD(mday2,period)
      ! write(6,*)iland,days1,days2

      DO iant=1, num_ant_type
        Uhuman  = antusage(iant)%Uhuman
        Ehuman  = antusage(iant)%Ehuman
        decay_f = antbalan(iant)%decay_f
        IF(days1 .NE. 0.) THEN
          DO i = 1, days1
          emh(iant)=(emh(iant)+popula*(Uhuman/365)*Ehuman*1000/subarea)*decay_f!<g->mg/km2/d rural
          END DO
        ELSEIF(days2 .NE. 0.) THEN
          DO i = 1, days2
          emh(iant)=(emh(iant)+popula*(Uhuman/365)*Ehuman*1000/subarea)*decay_f!<g->mg/km2/d rural
          END DO
        ENDIF
      END DO
    END DO

    !---------------
      DO i = 1,count
        ic = incircle(i)
        IF(ic .GT. 0)THEN
        ! incircle(count) = 258*(i-1)+j
          DO iant = 1,num_ant_type
              IF(peoinput(ic,iant) .EQ. 0.) THEN
                peoinput(ic,iant) = emh(iant)
                
              ELSE
                peoinput(ic,iant) = max(peoinput(ic,iant),emh(iant))
              ENDIF
          END DO
          
        ENDIF
      END DO
      ! END DO
    ! END DO

  END SUBROUTINE gridsearch_peo
!##########################################################################################################
!##########################################################################################################
  SUBROUTINE search1(c,searchRadius,gridSize,incircle,subarea,count)

    USE MODELVAR, ONLY : nrow_grid, &
                         ncol_grid, &
                         num_cell,  &
                         num_ant_type,&
                         landuse

    USE MODELVAR_ANT,ONLY: people,    &
                           antusage,     &
                           antcropdata,  &
                           antbalan
                         
                         
    INTEGER,INTENT(IN) :: c 
    REAL(KIND=8),  INTENT(IN)    :: searchRadius
    REAL(KIND=8),  INTENT(IN)    :: gridSize
    REAL(KIND=8),  INTENT(OUT)   :: subarea!!km2
    INTEGER,  INTENT(OUT)   :: count
    !>local 
    REAL(KIND=8):: dist
    
    INTEGER        :: centerRow, centerCol
    INTEGER        :: i, j, rowMin, rowMax, colMin, colMax,ic,iland
    INTEGER, ALLOCATABLE,  INTENT(OUT)  :: incircle(:)
    ! INTEGER, PARAMETER :: nrow_grid=248,ncol_grid=258
    subarea = 0.
    count = 0
    
    !---------------
    IF(MOD(c,ncol_grid) .NE. 0) THEN
      centerRow = INT(c/ncol_grid) + 1
      centerCol = MOD(c,ncol_grid)
    ELSE
      centerRow = INT(c/ncol_grid)
      centerCol = ncol_grid
    ENDIF
    ! �?定可能在圆内的栅格的范围
    rowMin = MAX(1, centerRow - CEILING(searchRadius/gridSize))
    rowMax = MIN(nrow_grid, centerRow + CEILING(searchRadius/gridSize))
    colMin = MAX(1, centerCol - CEILING(searchRadius/gridSize))
    colMax = MIN(ncol_grid, centerCol + CEILING(searchRadius/gridSize))
    ! ! 预估圆内�?能的最大栅格数
    IF(.NOT.ALLOCATED(incircle)) ALLOCATE(incircle((rowMax-rowMin+1)*(colMax-colMin+1)))
    incircle = 0
    ! count = (rowMax-rowMin+1)*(colMax-colMin+1)
    !-----------------
    DO i = rowMin, rowMax
      DO j = colMin, colMax
        dist = SQRT(REAL((i - centerRow)**2 + (j - centerCol)**2)) * gridSize
        IF (dist <= searchRadius) THEN
          ic = 258*(i-1)+j
          iland = landuse(ic)
          if(iland.EQ.1 .OR. iland.EQ.2 .OR. iland.EQ.4) then  !规模>林地，农�?
            subarea = subarea + 0.0025
            count = count + 1
            incircle(count) = ic
          END if
        ENDIF
      ENDDO
    ENDDO
    !---------------

  END SUBROUTINE search1
  !##########################################################################################################
!##########################################################################################################
  SUBROUTINE search2(c,searchRadius,gridSize,incircle,subarea,count)

    USE MODELVAR, ONLY : nrow_grid, &
                         ncol_grid, &
                         num_cell,  &
                         num_ant_type,&
                         landuse

    USE MODELVAR_ANT,ONLY: people,    &
                           antusage,     &
                           antcropdata,  &
                           antbalan
                         
                         
    INTEGER,INTENT(IN) :: c 
    REAL(KIND=8),  INTENT(IN)    :: searchRadius
    REAL(KIND=8),  INTENT(IN)    :: gridSize
    REAL(KIND=8),  INTENT(OUT)   :: subarea!!km2
    INTEGER,  INTENT(OUT)   :: count
    !>local 
    REAL(KIND=8):: dist
    
    INTEGER        :: centerRow, centerCol
    INTEGER        :: i, j, rowMin, rowMax, colMin, colMax,ic,iland
    INTEGER, ALLOCATABLE,  INTENT(OUT)  :: incircle(:)
    ! INTEGER, PARAMETER :: nrow_grid=248,ncol_grid=258
    subarea = 0.
    count = 0
    
    !---------------
    IF(MOD(c,ncol_grid) .NE. 0) THEN
      centerRow = INT(c/ncol_grid) + 1
      centerCol = MOD(c,ncol_grid)
    ELSE
      centerRow = INT(c/ncol_grid)
      centerCol = ncol_grid
    ENDIF
    ! �?定可能在圆内的栅格的范围
    rowMin = MAX(1, centerRow - CEILING(searchRadius/gridSize))
    rowMax = MIN(nrow_grid, centerRow + CEILING(searchRadius/gridSize))
    colMin = MAX(1, centerCol - CEILING(searchRadius/gridSize))
    colMax = MIN(ncol_grid, centerCol + CEILING(searchRadius/gridSize))
    ! ! 预估圆内�?能的最大栅格数
    IF(.NOT.ALLOCATED(incircle)) ALLOCATE(incircle((rowMax-rowMin+1)*(colMax-colMin+1)))
    incircle = 0
    ! count = (rowMax-rowMin+1)*(colMax-colMin+1)
    !-----------------
    DO i = rowMin, rowMax
      DO j = colMin, colMax
        dist = SQRT(REAL((i - centerRow)**2 + (j - centerCol)**2)) * gridSize
        IF (dist <= searchRadius) THEN
          ic = 258*(i-1)+j
          iland = landuse(ic)
          if(iland.EQ.1) then  !散养>林地
            subarea = subarea + 0.0025
            count = count + 1
            incircle(count) = ic
          END if
        ENDIF
      ENDDO
    ENDDO
    !---------------

  END SUBROUTINE search2
  !##########################################################################################################



  !##########################################################################################################
  SUBROUTINE get_peo_antinput(peosource)
    
    USE MODELVAR, ONLY: num_cell,num_ant_type
    USE MODELVAR_ANT, ONLY: people

    REAL(KIND = 8),  INTENT(INOUT) :: peosource(num_cell,num_ant_type)
    INTEGER ip , ik
    peosource=0.
    DO ik = 1,num_cell
      ip=people(ik)
      IF(ip .NE. -9999 .AND. ip .NE. 0) THEN
        CALL gridsearch_peo(ik,800.,50.,180,peosource) !60->180
      END IF
    END DO

  END SUBROUTINE get_peo_antinput
  !##########################################################################################################

  !##########################################################################################################
  SUBROUTINE get_animal_input(animalsource)
    
    USE MODELVAR, ONLY: num_cell,         &
                        num_ant_type,    &
                        landuse
    USE MODELVAR_ANT, ONLY:   antusage,         &
                              farm,             &
                              farmdata
    REAL(KIND=8),  INTENT(INOUT) :: animalsource(num_cell,num_ant_type)
                                   !1>规模  2>散养
    INTEGER im,ik,iant,isp,i,ic
    INTEGER number !number of animals
    INTEGER cultuer_cycle(4)
    ! REAL(KIND=8) :: Upig ,Epig, Uchicken,Echicken
    REAL(KIND=8) :: usage,excrete
    REAL(KIND=8) :: searchR,gridsize
    REAL(KIND=8) :: ema(num_ant_type)!!mg/km2
    INTEGER, ALLOCATABLE  :: incircle_1(:),incircle_2(:)
    REAL(KIND=8) :: subarea_1,subarea_2
    INTEGER :: count,count1,count2

    ! cultuer_cycle(1)=55
    ! cultuer_cycle(2)=365
    ! cultuer_cycle(3)=55
    ! cultuer_cycle(4)=365
    animalsource=0.
    searchR=2000.
    gridsize=50.
    DO ik = 1,num_cell
      im=farm(ik)
      IF(im .NE. -9999 .AND. im .NE. 0) THEN
        IF(farmdata(im)%scale .EQ. 1)THEN
          CALL search1(ik,searchR,gridsize,incircle_1,subarea_1,count1) !规模
        ELSEIF(farmdata(im)%scale .EQ. 2)THEN
          CALL search2(ik,searchR,gridsize,incircle_2,subarea_2,count2) !散养
        ENDIF
        DO iant=1, num_ant_type
          isp = farmdata(im)%species
          IF(isp.EQ.1 .OR. isp.EQ.2 .OR. isp.EQ.3)THEN !�?
            usage    = antusage(iant)%Uchicken!!g/zhi/y
            excrete  = antusage(iant)%Echicken
            number   = farmdata(im)%number_chicken
          ELSEIF(isp.EQ.4)THEN !�?
            usage    = antusage(iant)%Upig!!g
            excrete  = antusage(iant)%Epig
            number   = farmdata(im)%number_pig
          ENDIF

          IF(farmdata(im)%scale .EQ. 1) THEN !1>规模 2>散养
            ema(iant)=number*(usage/365)*1000*excrete/subarea_1!<mg/km2/d  !0.2为大型养殖场施用到环境中的比�?
            count=count1
            ELSEIF(farmdata(im)%scale .EQ. 2)THEN
            ema(iant)=number*(usage/365)*1000*excrete/subarea_2!<mg/km2
            count=count2
          ENDIF
          ! if(ema(iant).gt.0)write(6,*)ema(iant),ik,im
        END DO

        
        DO i = 1,count
          IF(farmdata(im)%scale .EQ. 1) THEN
            ic = incircle_1(i)
          ELSEIF(farmdata(im)%scale .EQ. 2) THEN
            ic = incircle_2(i)
          ENDIF

          IF(ic .GT. 0)THEN
            ! incircle(count) = 258*(i-1)+j
              DO iant = 1,num_ant_type
                  IF(animalsource(ic,iant) .EQ. 0.) THEN
                    animalsource(ic,iant) = ema(iant)
                  ELSE
                    animalsource(ic,iant) = MAX(animalsource(ic,iant),ema(iant))
                  ENDIF
              ! if(animalsource(ic,iant).gt.0)write(6,*)animalsource(ic,iant)
            END DO
          ENDIF
        END DO
      END IF
      
    END DO
  END SUBROUTINE get_animal_input
  !##########################################################################################################
END MODULE INITIALIZE_ANT