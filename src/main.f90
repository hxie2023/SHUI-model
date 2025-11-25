PROGRAM main
  USE MODELTYPE
  USE MODELTYPE_SED
  USE MODELTYPE_ANT
  USE DATETIME
  USE MODELVAR
  USE MODELVAR_SED
  USE MODELVAR_ANT
  USE DATAWRITE
  USE DATAGET
  USE DATAGET_ANT
  USE GENERAL
  USE MODELPAR
  USE MODELPAR_SED
  USE MODELPAR_ANT
  USE INITIALIZE
  USE INITIALIZE_SED
  USE INITIALIZE_ANT
  USE ABOVEGROUND
  USE GROUNDWATER
  USE LANDWATER
  USE SURFACEWATER
  USE EROSION
  USE GW_ANT
  USE SOIL_ANT
  USE RUN
  USE REACH_ANT
  USE REACH_SED
  USE OMP_LIB
  ! USE NETCDF
  
  IMPLICIT NONE
  INTEGER i, it, k, n,j,ik,bd2,bd5,c,iland
  INTEGER il, is, ib, ip,idt
  REAL(kind=8) :: depclay(80),depsilt(80),depsand(80)
  REAL(kind=8) clro(24),slro(24),snro(24)
  REAL(KIND=8) sumpeo(4),sumanim(4)
  REAL(KIND=8) emh_s(4)
  clro(24)=0.;slro(24)=0.;snro(24)=0.
  
  CALL get_args
  CALL date_and_time(logdate, logtime, logzone, logdtvalue)
  WRITE(6, '(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') &
        ' Project started date: ',logdtvalue(1),'-',logdtvalue(2),'-',logdtvalue(3),    &
        '  time: ',logdtvalue(5),':',logdtvalue(6),':',logdtvalue(7)
  ! WRITE(logfilename, '(a)') 'out/'//'log/'//'SHUI_'//logdate(1:8)//logtime(1:4)//'.log'
  ! OPEN(UNIT=log_unit, FILE = TRIM(logfilename), STATUS = 'REPLACE', ACTION = 'WRITE')
  ! WRITE(log_unit, '(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)') &
  !       ' Project started date: ',logdtvalue(1),'-',logdtvalue(2),'-',logdtvalue(3),    &
  !       '  time: ',logdtvalue(5),':',logdtvalue(6),':',logdtvalue(7)
  ! CALL log_process('SHUI model')
  CALL load_forcing
  CALL load_antdata
  CALL load_parameter
  CALL load_sed_parameter
  CALL load_ant_parameter
  CALL prepare_print
  CALL initialize_model
  CALL initialize_sedvar
  CALL initialize_ant_var

  CALL omp_set_num_threads(num_threads) 
  CALL log_process('OMP threads number set.')

! WRITE(*, *) steplen

! OPEN(UNIT=test_unit, FILE='out/test.txt', STATUS='REPLACE', ACTION='WRITE')
! OPEN(UNIT=998, FILE='out/test2.txt', STATUS='REPLACE', ACTION='WRITE')
! OPEN(UNIT=997, FILE='out/test3.txt', STATUS='REPLACE', ACTION='WRITE')
! write(997,"(8A20)")"date","rchdeg-ch","rchdeg-bed","rchout","rchload","dif","resusu","settle"
!       ! write(997,"(25A20)")"date","rchdeg-TC","rchdeg-CTC","rchdeg-OTC","rchdeg-DXC",&
!       !                   "rchburl-TC","rchburl-CTC","rchburl-OTC","rchburl-DXC",&
!       !                   "load-TC","load-CTC","load-OTC","load-DXC",&
!       !                   "mass-TC","mass-CTC","mass-OTC","mass-DXC",&
!       !                   "out-TC","out-CTC","out-OTC","out-DXC",&
!       !                   "end-TC","end-CTC","end-OTC","end-DXC"
! OPEN(UNIT=996, FILE='out/test4.txt', STATUS='REPLACE', ACTION='WRITE')
     

! OPEN(UNIT=995, FILE='out/sedconc.txt', STATUS='REPLACE', ACTION='WRITE')
!       write(995,"(5A10)")"date","sedconc"
! OPEN(UNIT=994, FILE='out/antconc.txt', STATUS='REPLACE', ACTION='WRITE')
!       write(994,"(5A10)")"date","TC","CTC","OTC","DXC"

! OPEN(UNIT=993, FILE='out/antload.txt', STATUS='REPLACE', ACTION='WRITE')
!       write(993,"(45A15)")"date","SUF-TC","SUF-CTC","SUF-OTC","SUF-DXC","SOL-TC","SOL-CTC","SOL-OTC","SOL-DXC",&
!                         "PAR-TC","PAR-CTC","PAR-OTC","PAR-DXC","GW-TC","GW-CTC","GW-OTC","GW-DXC",&
!                         "DEG-TC","DEG-CTC","DEG-OTC","DEG-DXC",&
!                         "DEG-TC","DEG-CTC","DEG-OTC","DEG-DXC",&
!                         "INIT-P-TC","INIT-P-CTC","INIT-P-OTC","INIT-P-DXC",&
!                         "INIT-D-TC","INIT-D-CTC","INIT-D-OTC","INIT-D-DXC","END-P-TC","END-P-CTC","END-P-OTC","END-P-DXC",&
!                         "END-D-TC","END-D-CTC","END-D-OTC","END-D-DXC","LEACH-TC","LEACH-CTC","LEACH-OTC","LEACH-DXC"
! OPEN(UNIT=992, FILE='out/antsource.txt', STATUS='REPLACE', ACTION='WRITE')
!       write(992,"(9A10)")"date","ANI-TC","ANI-CTC","ANI-OTC","ANI-DXC","PEO-TC","PEO-CTC","PEO-OTC","PEO-DXC"


! OPEN(UNIT=888, FILE='out/conc_riv5.txt', STATUS='REPLACE', ACTION='WRITE')
!     write(888,"(5A10)")"date","TC","CTC","OTC","DXC"
! OPEN(UNIT=887, FILE='out/conc_riv9.txt', STATUS='REPLACE', ACTION='WRITE')
!     write(887,"(5A10)")"date","TC","CTC","OTC","DXC" 
! OPEN(UNIT=886, FILE='out/conc_riv15.txt', STATUS='REPLACE', ACTION='WRITE')
!     write(886,"(5A10)")"date","TC","CTC","OTC","DXC" 
! OPEN(UNIT=885, FILE='out/conc_riv17.txt', STATUS='REPLACE', ACTION='WRITE')
!     write(885,"(5A10)")"date","TC","CTC","OTC","DXC" 
! OPEN(UNIT=884, FILE='out/conc_riv20.txt', STATUS='REPLACE', ACTION='WRITE')
!     write(884,"(5A10)")"date","TC","CTC","OTC","DXC" 
! OPEN(UNIT=883, FILE='out/conc_riv23.txt', STATUS='REPLACE', ACTION='WRITE')
!     write(883,"(5A10)")"date","TC","CTC","OTC","DXC" 
! OPEN(UNIT=882, FILE='out/conc_riv7.txt', STATUS='REPLACE', ACTION='WRITE')
!     write(882,"(5A10)")"date","TC","CTC","OTC","DXC" 
!   !>Land simulation
! OPEN(UNIT=881, FILE='out/pathload.txt', STATUS='REPLACE', ACTION='WRITE')
!     write(881,"(13A20)")"date","ld-surflow","ld-interflow","ld-baseflow","ld-erosion" ,&
!                               "nt-surflow","nt-interflow","nt-baseflow","nt-erosion" ,&
!                               "qt-surflow","qt-interflow","qt-baseflow","qt-erosion" 
! OPEN(UNIT=880, FILE='out/pathload-24sub.txt', STATUS='REPLACE', ACTION='WRITE')
! OPEN(UNIT=777,FILE='out/pathload-riv.txt', STATUS='REPLACE', ACTION='WRITE')   

! OPEN(UNIT=666,FILE='out/cell-discharge.txt', STATUS='REPLACE', ACTION='WRITE')
! OPEN(UNIT=665,FILE='out/cell-deg.txt', STATUS='REPLACE', ACTION='WRITE')
! OPEN(UNIT=664,FILE='out/cell-leach.txt', STATUS='REPLACE', ACTION='WRITE')
! OPEN(UNIT=663,FILE='out/cell-mixre.txt', STATUS='REPLACE', ACTION='WRITE')
! OPEN(UNIT=662,FILE='out/cell-solre.txt', STATUS='REPLACE', ACTION='WRITE')

!######################################################################################################
  CALL log_process('Domin loops started...')  
  !$OMP PARALLEL DO PRIVATE(il,is,ib,k) &
  !$OMP& REDUCTION(+:suro_sub,soro_sub,gwro_sub,clro_sub,slro_sub,snro_sub) &
  !$OMP& REDUCTION(+:prec_b,thrf_b,pet_b,infilt_b,rechg_b,epc_b,eps_b,suro_b,soro1_b,soro2_b,gwro_b,surosat_b,suroexc_b) &
  !$OMP& REDUCTION(+:num_sim_cell) &
  !$OMP& REDUCTION(+:sulo_sub,solo_sub,palo_sub,gwlo_sub)&
  !$OMP& REDUCTION(+:solrunoff_sub,source_sub,leach_sub,surfer_sub,gw_sub,eroed_sub,init_sol,end_sol,deg_sub)&  
  !$OMP& REDUCTION(+:init_sub,end_sub,path_sub)    
  DO k= 1,num_cell
  ! DO k = 32975, 32975
  ! DO k = 51720, 51720  !<201,120 -> 200*258+120
  ! DO k = 7845, 7845  !<31,105 -> 30*258+105
    ! DO k = 51416,51416
    il=landuse(k);is=soiltype(k);ib=subbasin(k)
    ! write(6,*)il,is
    ! if(ib.ne.20)CYCLE
! IF(k == 51720) WRITE(6,*) il,is,ib
! IF(k == 51720) WRITE(6,*) coef_delay_perc(0,3),coef_delay_perc(1,3),coef_delay_perc(2,3)
! IF(k == 51720) WRITE(6,*) sw_wp(0,is),sw_fc(0,is),sw_pv(0,is)
! IF(k == 51720) WRITE(6,*) sw_wp(1,is),sw_fc(1,is),sw_pv(1,is)
! IF(k == 51720) WRITE(6,*) sw_wp(2,is),sw_fc(2,is),sw_pv(2,is)
    IF(il.EQ.-9999.OR.is.EQ.-9999.OR.ib.EQ.-9999) CYCLE
    num_sim_cell = num_sim_cell + 1
    CALL run_land_1cell(c          =     k,           &
                        tss        =     sim_length,  &
                        suro       =     suro_sub,    &
                        soro       =     soro_sub,    &
                        gwro       =     gwro_sub,    &
                        prec_b     =     prec_b,      &
                        thrf_b     =     thrf_b,      &
                        pet_b      =     pet_b,       &
                        infilt_b   =     infilt_b,    &
                        rechg_b    =     rechg_b,     &
                        epc_b      =     epc_b,       &
                        eps_b      =     eps_b,       &
                        suro_b     =     suro_b,      &
                        soro1_b    =     soro1_b,     &
                        soro2_b    =     soro2_b,     &
                        gwro_b     =     gwro_b,      &
                        surosat_b  =     surosat_b,   &
                        suroexc_b  =     suroexc_b,   &
                        clro       =     clro_sub,    &
                        slro       =     slro_sub,    &
                        snro       =     snro_sub,    &
                        sulo       =     sulo_sub,    &
                        solo       =     solo_sub,    &
                        palo       =     palo_sub,    &
                        gwlo       =     gwlo_sub,    &
                        leach      =     leach_sub,   &
                        deglo      =     deg_sub,     &
                        endant     =     end_sub,     &
                        iniant     =     init_sub,    &
                        pathlo     =     path_sub,    &
                        celllo1    =     celllo&
                       )            
  END DO
  !$OMP END PARALLEL DO
  ! ! !-----------------------------
  ! DO i = 1, sim_length
  !       WRITE(996, '(A11)', ADVANCE='NO') date_sim(i)
  !     ! DO j = 1, 24
  !       ! WRITE(996, '(F20.4)',ADVANCE='NO') source_sub(1,j,i,1)+source_sub(1,j,i,2)+source_sub(1,j,i,3) !!mg/km2/d
  !       WRITE(996, *) sum(source_sub(1,:,i,1)),sum(source_sub(1,:,i,2)),&
  !                                          sum(source_sub(1,:,i,3)) !!mg/km2/d
  !     ! END DO
  !       ! WRITE(996, *) 
  !   END DO



!-------------
  ! i=sedpar_general(genid_idriv)
  !   write(6,*)sedresp(i)%claycd,sedresp(i)%claycs,sedresp(i)%mclay,&
  !                                            sedresp(i)%siltcd,sedresp(i)%siltcs,sedresp(i)%msilt
  
  CALL log_process('Domin loops completed.') 

  ! WRITE(6,*) 'simulation length=', sim_length
  ! WRITE(6,*) 'num_sim_cell=', num_sim_cell
  ! WRITE(6,*) 'prec=',     prec_b/num_sim_cell
  ! WRITE(6,*) 'thrf=',     thrf_b/num_sim_cell
  ! WRITE(6,*) 'pet=',      pet_b/num_sim_cell
  ! WRITE(6,*) 'infilt=',   infilt_b/num_sim_cell
  ! WRITE(6,*) 'recharge=', rechg_b/num_sim_cell
  ! WRITE(6,*) 'evap_s=',   eps_b/num_sim_cell
  ! WRITE(6,*) 'evap_c=',   epc_b/num_sim_cell
  ! WRITE(6,*) 'suro=',     suro_b/num_sim_cell
  ! WRITE(6,*) 'soro1=',    soro1_b/num_sim_cell
  ! WRITE(6,*) 'soro2=',    soro2_b/num_sim_cell
  ! WRITE(6,*) 'gwro=',     gwro_b/num_sim_cell
  ! WRITE(6,*) 'surosat=',  surosat_b/num_sim_cell
  ! WRITE(6,*) 'suroexc=',  suroexc_b/num_sim_cell
CLOSE(999)

!######################################################################################################
  !>Land routing
  DO i = 1, num_sub
    CALL routing_land(isub        =    i,              &
                      tss         =    sim_length,     &
                      suro        =    suro_sub(i,:),  &
                      soro        =    soro_sub(i,:),  &
                      gwro        =    gwro_sub(i,:),  &
                      sub_to_riv  =    sub2riv(i)      &
                      )

    CALL routing_erosion(isub        =    i,                &
                         tss         =    sim_length,       &
                         clro        =    clro_sub(i,:,:),  &
                         slro        =    slro_sub(i,:,:),  &
                         snro        =    snro_sub(i,:,:),  &
                         sub_to_riv  =    sub2riv(i)        &
                         )
    CALL routing_ant(isub = i,&
                     tss  = sim_length,        &
                     sulo = sulo_sub(:,i,:),   &
                     solo = solo_sub(:,i,:),   &
                     palo = palo_sub(:,i,:),   &
                     gwlo = gwlo_sub(:,i,:),   &
                     asub_to_riv = antsub2riv(i))
  END DO

  ! call outload
  ! call output
  ! DO i = 1, 24
  ! DO it = 1, sim_length
    ! write(996,*)suro_sub(10,it),sulo_sub(1,10,it)
  ! write(998,*)solo_sub(1,15,it),sulo_sub(1,15,it),palo_sub(1,15,it),gwlo_sub(1,15,it)
  !   !  write(998,*)it,antsub2riv(18)%paload(1,it)*1.e6,antsub2riv(18)%load(1,it)*1.e6,&
  !                 ! antsub2riv(19)%paload(1,it)*1.e6,antsub2riv(19)%load(1,it)*1.e6
      ! write(997,*)it,antsub2riv(15)%suload(1,it)*1.e6,antsub2riv(15)%soload(1,it)*1.e6,&
      !             antsub2riv(15)%paload(1,it)*1.e6,antsub2riv(15)%gwload(1,it)*1.e6
  ! write(997,*)antsub2riv(15)%load(1,it)*1.e6,
  ! !     write(998,*)sub2riv(18)%suflow(it),sub2riv(18)%soflow(it),&
  ! !                 sub2riv(19)%suflow(it),sub2riv(19)%soflow(it)
  !     clro(i) = clro(i)+sub2riv(i)%erosion_c(it)
  !     slro(i) = slro(i)+sub2riv(i)%erosion_l(it)
  !     snro(i) = snro(i)+sub2riv(i)%erosion_s(it)
  ! END DO 
    ! write(998,*)clro_sub(20,idt,1),slro_sub(20,idt,1),snro_sub(20,idt,1)
    ! write(996,*) sub2riv(20)%sed(1,2,idt),sub2riv(20)%sed(2,2,idt),&
    !              sub2riv(20)%sed(3,2,idt)
                 
  ! DO i = 1, sim_length
  ! !    WRITE(996, '`, ADVANCE='NO') date_sim(i)
  !    DO j = 1, 24
  ! !     ! WRITE(996, '(F10.4)',ADVANCE='NO') sub2riv(j)%sed(1,1,i)
  !   ! WRITE(996,*) sub2riv(11)%gwflow(i)!sum(gwro_sub(:,i))
  !     suload(j)=sum(antsub2riv(j)%suload(:,i))
  !     soload(j)=sum(antsub2riv(j)%soload(:,i))
  !     gwload(j)=sum(antsub2riv(j)%gwload(:,i))
  !     paload(j)=sum(antsub2riv(j)%paload(:,i))
  !     WRITE(997,*) suload(j),soload(j),paload(j),gwload(j)!!kg/d
  !     ! WRITE(997,*) antsub2riv(11)%suload(1,i), antsub2riv(11)%soload(1,i),antsub2riv(11)%paload(1,i),antsub2riv(11)%gwload(1,i)!!kg/d 
  !     ! WRITE(996, '(F20.16)',ADVANCE='NO') antsub2riv(j)%load(1,i)+antsub2riv(j)%paload(1,i)+antsub2riv(j)%gwload(1,i)!!kg/d 
  !   END DO
  !     ! WRITE(996, *)
  ! END DO

  ! DO i = 1, 24
  !   DO it = 1, sim_length
  !     clro(i) = clro(i)+sub2riv(i)%erosion_c(it)
  !     slro(i) = slro(i)+sub2riv(i)%erosion_l(it)
  !     snro(i) = snro(i)+sub2riv(i)%erosion_s(it)
  ! !     ! write(998,*) it,sub2riv(i)%erosion_c(it),sub2riv(i)%erosion_l(it),sub2riv(i)%erosion_s(it)
  !   END DO
  ! ! !     write(996,*)antsub2riv(20)%paload(1,it),antsub2riv(20)%load(1,it)
  !   write(996,*)i,clro(i),slro(i),snro(i)
  ! END DO
  
  
  !--------------------------------
  ! CALL print_land_nc(time_len     =    comp_length,        &
  !                    var_record   =    out_land,           &
  !                    filename     =    land_nc_file)
! WRITE(6,*) 'land ended.'
!######################################################################################################
  !>Reach routing and substance transport and transformation

  CALL run_rch(tss              =    sim_length,           &
               sub_to_rivs      =    sub2riv,              &
               rivs             =    river,                &
               rivstates        =    riverstate,           &
               rivfluxes        =    riverflux,            &
               sedstates        =    sedstate,             &
               sedfluxes        =    sedflux               &
               )

  ! CALL print_rch_flow("out/flow.rch")
  ! CALL print_indiv_rch_flow("out/rch9.txt", 9)
  ! CALL print_rch_sedconc("out/sedconc.rch")
  CALL print_indiv_rch_sedconc("out/sedconc_17.rch", 17)
  ! CALL print_indiv_rch_sedload("out/sedload_22.rch", 22)

!######################################################################################################
  !>Only for preparing hotstart
  ! CALL record_state(var_record   =    out_hot,             &
  !                   filename     =    hotstart_file)

  CALL finish_print
  WRITE(6, *) "----------------------------------------------------------------------"
  WRITE(6, *) "Project completed."
  CALL date_and_time(values = logdtvalue)
  WRITE(6, '(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')                                  &
             ' Project ended date: ',logdtvalue(1),'-',logdtvalue(2),'-',logdtvalue(3), &
             '  time: ',logdtvalue(5),':',logdtvalue(6),':',logdtvalue(7)
  ! WRITE(log_unit, *) "----------------------------------------------------------------------"
  ! WRITE(log_unit, '(A,I4,A,I2.2,A,I2.2,A,I2.2,A,I2.2,A,I2.2)')                                  &
  !            ' Project ended date: ',logdtvalue(1),'-',logdtvalue(2),'-',logdtvalue(3), &
  !            '  time: ',logdtvalue(5),':',logdtvalue(6),':',logdtvalue(7)
  ! CLOSE(log_unit)
END PROGRAM main