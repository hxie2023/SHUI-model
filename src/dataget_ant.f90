MODULE DATAGET_ANT
  USE DATAWRITE, ONLY : log_process
  IMPLICIT NONE
  PUBLIC load_antdata
CONTAINS
  SUBROUTINE load_antdata
    USE MODELVAR_ANT, ONLY :ant_usage_file,     &
                            ant_usage_file_unit, &
                            antusage,           &
                            ant_popu_file,      &
                            ant_popu_file_unit, &
                            popu,               &
                            iland_area,         &
                            ant_crop_file,      &
                            ant_crop_file_unit, &
                            antcropdata,        &
                            people_file,        &
                            people_file_unit,   &
                            people,             &
                            farm_file,          &
                            farm_file_unit,     &
                            farm,               &
                            farmdata_file,      &
                            farmdata_file_unit, &
                            farmdata

                                           
    CALL get_ant_usage(ant_usage_file_unit, ant_usage_file, antusage)
    CALL get_ant_popudata(ant_popu_file_unit,ant_popu_file,popu)
    CALL get_iland_area(iland_area)
    CALL get_cropdata(ant_crop_file_unit,ant_crop_file,antcropdata)
    CALL get_people(people_file_unit,people_file,people)
    CALL get_farm(farm_file_unit,farm_file,farm)
    CALL get_farmdata(farmdata_file_unit,farmdata_file,farmdata)
    
  END SUBROUTINE load_antdata

  SUBROUTINE get_ant_usage(funit, fname, antuse)
    USE MODELVAR,       ONLY   :   simcfg,num_ant_type
    USE MODELTYPE_ANT,  ONLY   :   ANTUSAGETYPE 
    !>IN&OUT
    INTEGER, INTENT(IN)                            ::         funit
    CHARACTER(LEN = *), INTENT(IN)                 ::         fname
    TYPE(ANTUSAGETYPE), ALLOCATABLE, INTENT(OUT)   ::         antuse(:)
    !>Local variables
    INTEGER i, iuse
    CHARACTER line
    
    !>
    IF(.NOT.ALLOCATED(antuse)) ALLOCATE(antuse(num_ant_type)) 

    OPEN(UNIT = funit, FILE = fname, ACTION = 'READ', STATUS = 'OLD')
      DO i = 1, 1        !<First line is comment
        READ(funit, *)
      END DO
      DO iuse = 1, num_ant_type
        READ(funit, *) line, antuse(iuse)%Uhuman,       &
                             antuse(iuse)%Ehuman,       &
                             antuse(iuse)%Rwwtp,        &
                             antuse(iuse)%Rferment,     &
                             antuse(iuse)%Upig,         &
                             antuse(iuse)%Epig,         &
                             antuse(iuse)%Uchicken,     &
                             antuse(iuse)%Echicken,     &
                             antuse(iuse)%DTsoil ,      &
                             antuse(iuse)%DTwater,      &
                             antuse(iuse)%Kpwater,      &
                             antuse(iuse)%Kow,          &
                             antuse(iuse)%molwt,        &
                             antuse(iuse)%DTmanure,     &
                             antuse(iuse)%Kpsoil,       &
                             antuse(iuse)%kd_c,         &
                             antuse(iuse)%kd_l,         &
                             antuse(iuse)%kd_s,         &
                             antuse(iuse)%Kpsed,        &
                             antuse(iuse)%DTsediment,   &
                             antuse(iuse)%wsol
      END DO
      
    CLOSE(funit)              
    
    CALL log_process('Antibiotic usage loaded.')
    
  END SUBROUTINE get_ant_usage

  SUBROUTINE get_ant_popudata(funit,     &
                          fname,     &
                          array)
    USE MODELVAR, ONLY  :  num_sub
     !>IN&OUT
    INTEGER, INTENT(IN)                    ::      funit
    CHARACTER(LEN=*), INTENT(IN)           ::      fname
    REAL(KIND=8), ALLOCATABLE, INTENT(OUT) ::      array(:,:)
    !>Local
    LOGICAL  ::  alive
    INTEGER  ::  iskip, isub
    INQUIRE(FILE = fname, EXIST = alive)
    IF(.NOT. alive) THEN 
      WRITE(*, *) TRIM(fname), " doesn't exist."
      STOP
    END IF
    IF(.NOT. ALLOCATED(array)) ALLOCATE(array(num_sub, 2))!<1:rural 2:town
    OPEN(UNIT = funit, FILE = fname, STATUS = "OLD", ACTION = "READ")
      DO iskip = 1, 1
        READ(funit, *)
      END DO
      DO isub = 1, num_sub
        READ(funit, *) array(isub, :)
      END DO
    CLOSE(funit)
    CALL log_process('Pupolations data loaded.')
    END SUBROUTINE get_ant_popudata
   
    SUBROUTINE get_iland_area(areaild)
      USE MODELVAR, ONLY: num_cell,   &
                          num_landuse, &
                          landuse,num_sub,subbasin
      REAL(KIND=8),ALLOCATABLE,INTENT(OUT)::areaild(:,:)
      INTEGER i,k,isub,iland
      REAL(KIND=8),ALLOCATABLE  :: cellarea(:,:,:)
      IF(.NOT.ALLOCATED(areaild)) ALLOCATE(areaild(num_sub,num_landuse))
      IF(.NOT.ALLOCATED(cellarea)) ALLOCATE(cellarea(num_cell,num_sub,num_landuse))

      areaild(:,:)=0
      DO k = 1,num_cell
        iland=landuse(k);isub=subbasin(k)
        IF(isub.EQ.-9999) CYCLE
        IF(iland.EQ.-9999) iland= 1
        cellarea(k,isub,iland)= 0.0025
      END DO 
      DO i = 1,num_cell
        DO isub = 1,num_sub
          DO iland = 1,num_landuse
            areaild(isub,iland)=cellarea(i,isub,iland)+areaild(isub,iland)
          END DO
        END DO
      END DO    
    END SUBROUTINE get_iland_area
    !##########################################################################################################
    SUBROUTINE get_cropdata(funit, fname, crop)

      USE MODELTYPE_ANT,  ONLY   :   ANTCROPDATATYPE 
      USE MODELVAR,       ONLY   :   simcfg,crop_luid
  
      !>IN&OUT
      INTEGER, INTENT(IN)                               ::         funit
      CHARACTER(LEN = *), INTENT(IN)                    ::         fname
      TYPE(ANTCROPDATATYPE), ALLOCATABLE, INTENT(OUT)   ::         crop(:)
      !>Local variables
      INTEGER i, icrop
      CHARACTER line

      IF(.NOT.ALLOCATED(crop_luid)) ALLOCATE(crop_luid(3))
      !>Rotate 2 crops
      IF(.NOT.ALLOCATED(crop)) ALLOCATE(crop(3)) !<TODO
  
      OPEN(UNIT = funit, FILE = fname, ACTION = 'READ', STATUS = 'OLD')
        DO i = 1, 1        !<First line is comment
          READ(funit, *)
        END DO
        DO icrop = 1, 3
          READ(funit, *) line, crop(icrop)%luid,               &
                               crop(icrop)%part,               &
                               crop(icrop)%ferttcamount1,        &
                               crop(icrop)%fertctcamount1,       &
                               crop(icrop)%fertotcamount1,       &
                               crop(icrop)%fertdcamount1,       &
                               crop(icrop)%fertday1,           &
                               crop(icrop)%fertdown1,          &
                               crop(icrop)%mantcamount1,        &
                               crop(icrop)%manctcamount1,        &
                               crop(icrop)%manotcamount1,        &
                               crop(icrop)%mandcamount1,        &
                               crop(icrop)%manday1,            &
                               crop(icrop)%mandown1,           &
                               crop(icrop)%ferttcamount2,       &
                               crop(icrop)%fertctcamount2,       &
                               crop(icrop)%fertotcamount2,       &
                               crop(icrop)%fertdcamount2,       &
                               crop(icrop)%fertday2,           &
                               crop(icrop)%fertdown2,          &
                               crop(icrop)%mantcamount2,        &
                               crop(icrop)%manctcamount2,        &
                               crop(icrop)%manotcamount2,        &
                               crop(icrop)%mandcamount2,        &
                               crop(icrop)%manday2,            &
                               crop(icrop)%mandown2,           &
                               crop(icrop)%restcamount,         &
                               crop(icrop)%resctcamount,         &
                               crop(icrop)%resotcamount,         &
                               crop(icrop)%resdcamount,         &
                               crop(icrop)%resdayno,           &
                               crop(icrop)%resdown,            &
                               crop(icrop)%resfast,            &
                               crop(icrop)%uptake1,            &
                               crop(icrop)%uptake2,            &
                               crop(icrop)%uptake3,            &
                               crop(icrop)%baredayno1,         &
                               crop(icrop)%baredayno2,         &
                               crop(icrop)%baredayno3,         &
                               crop(icrop)%baredayno4,         &
                               crop(icrop)%baredayno5,         &
                               crop(icrop)%ccmax1,             &
                               crop(icrop)%ccmax2,             &
                               crop(icrop)%gcmax1,             &
                               crop(icrop)%gcmax2,             &
                               crop(icrop)%PNuptakeRatio,      &
                               crop(icrop)%uptakeupper,        &
                               crop(icrop)%phmax1,             &
                               crop(icrop)%phmax2
          crop_luid(icrop) = crop(icrop)%luid                     
          ! crop(icrop)%fertnamount1 = crop(icrop)%fertnamount1 * 100.  !>kg/ha -> kg/km2
          ! crop(icrop)%fertpamount1 = crop(icrop)%fertpamount1 * 100.
          ! crop(icrop)%mannamount1  = crop(icrop)%mannamount1  * 100.
          ! crop(icrop)%manpamount1  = crop(icrop)%manpamount1  * 100.
          ! crop(icrop)%fertnamount2 = crop(icrop)%fertnamount2 * 100.
          ! crop(icrop)%fertpamount2 = crop(icrop)%fertpamount2 * 100.
          ! crop(icrop)%mannamount2  = crop(icrop)%mannamount2  * 100.
          ! crop(icrop)%manpamount2  = crop(icrop)%manpamount2  * 100.
          ! crop(icrop)%resnamount   = crop(icrop)%resnamount   * 100.
          ! crop(icrop)%respamount   = crop(icrop)%respamount   * 100.
        END DO
        
        CLOSE(funit)              
      
      CALL log_process('ANT_Crop data loaded.')
      
    END SUBROUTINE get_cropdata
    !##########################################################################################################
    !##########################################################################################################
    SUBROUTINE get_farmdata(funit, fname, farmdata)

      USE MODELTYPE_ANT,  ONLY   :   FARMTYPE
      USE MODELVAR,       ONLY   :   simcfg
      USE MODELTYPE
      !>IN&OUT
      INTEGER, INTENT(IN)                               ::         funit
      CHARACTER(LEN = *), INTENT(IN)                    ::         fname
      TYPE(FARMTYPE), ALLOCATABLE, INTENT(OUT)   ::         farmdata(:)
      !>Local variables
      INTEGER i, ifarm,num_farm
      CHARACTER line
      num_farm = 13
      !>
      IF(.NOT.ALLOCATED(farmdata)) ALLOCATE(farmdata(num_farm)) 
  
      OPEN(UNIT = funit, FILE = fname, ACTION = 'READ', STATUS = 'OLD')
        DO i = 1, 1        !<First line is comment
          READ(funit, *)
        END DO
        DO ifarm = 1, num_farm
          READ(funit, *) line, farmdata(ifarm)%number_pig,               &
                               farmdata(ifarm)%number_chicken,           &
                               farmdata(ifarm)%species,                  &
                               farmdata(ifarm)%scale
 
        END DO
        
        CLOSE(funit)              
      
      ! CALL log_process('ANT_Crop data loaded.')
      
    END SUBROUTINE get_farmdata
    !##########################################################################################################
    !########################################################################################################## 
    SUBROUTINE get_people(funit,     &
                          fname,     &
                          array)

      USE MODELVAR, ONLY : nrow_grid, ncol_grid, simcfg, num_cell
      INTEGER, INTENT(IN)               :: funit
      CHARACTER(LEN = *), INTENT(IN)    :: fname
      INTEGER, ALLOCATABLE, INTENT(OUT) :: array(:)
      LOGICAL  ::  alive
      INTEGER  ::  i, j, nskip
      INQUIRE(FILE = fname, EXIST = alive)
      IF(.NOT. alive) THEN 
      WRITE(*, *) TRIM(fname), " doesn't exist."
      STOP
      END IF
      IF(.NOT.ALLOCATED(array)) ALLOCATE(array(num_cell))
      OPEN(UNIT= funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO nskip = 1, 6
      READ(funit, *) 
      END DO
      DO i = 1, nrow_grid
      READ(funit, *) (array((i-1)*ncol_grid+j), j=1,ncol_grid)
      END DO
      CLOSE(funit)

      
      END SUBROUTINE get_people
    !##########################################################################################################
    SUBROUTINE get_farm(funit,     &
                          fname,     &
                          array)

      USE MODELVAR, ONLY : nrow_grid, ncol_grid, simcfg, num_cell
      INTEGER, INTENT(IN)               :: funit
      CHARACTER(LEN = *), INTENT(IN)    :: fname
      INTEGER, ALLOCATABLE, INTENT(OUT) :: array(:)
      LOGICAL  ::  alive
      INTEGER  ::  i, j, nskip
      INQUIRE(FILE = fname, EXIST = alive)
      IF(.NOT. alive) THEN 
      WRITE(*, *) TRIM(fname), " doesn't exist."
      STOP
      END IF
      IF(.NOT.ALLOCATED(array)) ALLOCATE(array(num_cell))
      OPEN(UNIT= funit, FILE = fname, STATUS = 'OLD', ACTION = 'READ')
      DO nskip = 1, 6
      READ(funit, *) 
      END DO
      DO i = 1, nrow_grid
      READ(funit, *) (array((i-1)*ncol_grid+j), j=1,ncol_grid)
      END DO
      CLOSE(funit)
      END SUBROUTINE get_farm
    !##########################################################################################################

END MODULE DATAGET_ANT