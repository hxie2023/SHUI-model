MODULE INITIALIZE_SED
  IMPLICIT NONE
CONTAINS 
!######################################################################################################
!######################################################################################################
  SUBROUTINE initialize_sedvar
    USE MODELTYPE
    USE MODELPAR
    USE MODELVAR,        ONLY   :   num_cell,        &
                                    sim_length,      &
                                    num_sub,         &
                                    num_riv,         &
                                    dp,              &
                                    river,           &
                                    bulkdensity
    USE MODELVAR_SED,    ONLY   :   dep_pc,          &
                                    erod_g,          &
                                    erod_tc,         &
                                    vs_clay,         &
                                    vs_sand,         &
                                    vs_silt,         &
                                    sand_size,       &
                                    silt_size,       &
                                    clay_size,       &
                                    flow_density,    &
                                    g_acc,           &
                                    eta,             &
                                    paro_sub,        &
                                    clro_sub,        &
                                    slro_sub,        &
                                    snro_sub,        &
                                    sediment_density,&
                                    relsed,          &
                                    sedstate,        &
                                    sedflux

    INTEGER isub, ir

    vs_sand = sand_size*sand_size*(sediment_density*1000.-flow_density)*g_acc/eta/18.
    vs_silt = silt_size*silt_size*(sediment_density*1000.-flow_density)*g_acc/eta/18.
    vs_clay = clay_size*clay_size*(sediment_density*1000.-flow_density)*g_acc/eta/18.

    IF(.NOT.ALLOCATED(dep_pc)) ALLOCATE(dep_pc(3, num_cell,sim_length),source=0._dp) !<clay,silt,sand
    IF(.NOT.ALLOCATED(erod_g)) ALLOCATE(erod_g(3, num_cell,sim_length),source=0._dp) !<clay,silt,sand
    IF(.NOT.ALLOCATED(erod_tc)) ALLOCATE(erod_tc(3, num_cell,sim_length),source=0._dp) !<clay,silt,sand
    IF(.NOT.ALLOCATED(relsed)) ALLOCATE(relsed(3, num_cell,sim_length),source=0._dp)

    IF(.NOT.ALLOCATED(paro_sub)) ALLOCATE(paro_sub(num_sub,sim_length),source=0._dp)
    IF(.NOT.ALLOCATED(clro_sub)) ALLOCATE(clro_sub(num_sub,sim_length,2),source=0._dp)
    IF(.NOT.ALLOCATED(slro_sub)) ALLOCATE(slro_sub(num_sub,sim_length,2),source=0._dp)
    IF(.NOT.ALLOCATED(snro_sub)) ALLOCATE(snro_sub(num_sub,sim_length,2),source=0._dp)

    IF(.NOT.ALLOCATED(sedstate)) ALLOCATE(sedstate(num_riv))
    IF(.NOT.ALLOCATED(sedflux)) ALLOCATE(sedflux(num_riv))
   
    DO ir = 1, num_riv
      sedstate(ir)%sedconc       = 15._dp   !<mg/L, TODO
      sedstate(ir)%clayconc      = 5._dp
      sedstate(ir)%siltconc      = 8._dp
      sedstate(ir)%sandconc      = 2._dp

      sedstate(ir)%sedpool       = sedstate(ir)%sedconc*river(ir)%vol/1000.
      sedstate(ir)%claypool      = sedstate(ir)%clayconc*river(ir)%vol/1000.
      sedstate(ir)%siltpool      = sedstate(ir)%siltconc*river(ir)%vol/1000.
      sedstate(ir)%sandpool      = sedstate(ir)%sandconc*river(ir)%vol/1000.
      !
      sedstate(ir)%sedbed    = sediment_density*0.2*river(ir)%width*river(ir)%length *1.e3 !kg
      sedstate(ir)%claybed    = sedstate(ir)%sedbed * (4./11)
      sedstate(ir)%siltbed    = sedstate(ir)%sedbed * (4./11)
      sedstate(ir)%sandbed    = sedstate(ir)%sedbed * (3./11)
      IF(.NOT.ALLOCATED(sedstate(ir)%sedload)) ALLOCATE(sedstate(ir)%sedload(4),source=0._dp)
    END DO

    DO ir = 1, num_riv
      IF(.NOT.ALLOCATED(sedflux(ir)%sedload)) &
         ALLOCATE(sedflux(ir)%sedload(4,sim_length), source = 0.0_dp)    !<Daily riverine SPS flux
      IF(.NOT.ALLOCATED(sedflux(ir)%spsconc)) &
         ALLOCATE(sedflux(ir)%spsconc(4,sim_length), source = 0.0_dp)    !<Daily average SPS concentration
    END DO

      ! riverstate(isub)%ppsedbed   = bulkdensity*500.*0.1*river(isub)%area*1.E-6   !<PP in river bedload sediment; TODO
    ! !-------------------------------------------------------------------------------------------------
  
    ! CALL log_process('Model variables initialized.')

  END SUBROUTINE initialize_sedvar


END MODULE INITIALIZE_SED