
program PROSUM_main

! Created:  18/10/11
! By:       Ed Rowe
! Why:      Shell to run PROSUM (plant PROduction and SUccession Model) 
! For documentation see PROSUM_sub
! v0.8 adapted to run PROSUM_sub_v0.8 (the version used in 1DICZ Oct2014) stand-alone 


!****************************************************************************************
! MODULES USED:
    use PROSUM_module  ! reads time series inputs, and sets some parameters for global use in array declarations

!****************************************************************************************
! DECLARATIONS:    
    implicit none
! Subroutines:
    external PROSUM
! Housekeeping variables:
    integer :: InitialDynamicTerminal   ! control of program stage: 1=Initial, 2= Dynamic, 3=Terminal
    integer :: StandAlone = 1           ! control of run type: 0=run from SoilTrEC1D, 1= run stand-alone
    integer :: nnutrient,&   ! (suffix = _e): 1=C, 2=N, 3=P, 4=Ca, 5=Mg, 6=K
               nlayer,&      ! (suffix = _s): 1=Top SoilLayer,2=second SoilLayer down, ...
               nplantbits,&  ! (suffix = _b): 1=Leaf, 2=Wood, 3=Root
               nplanttypes,& ! (suffix = _t): 1=WoodDecMyc,2=WoodDecNon,3=WoodEverMyc,4=WoodEverNon,5=HerbMyc,6=HerbNon
               Num_months_of_parameters
               
    double precision :: Month_start = 1
    double precision :: Month 

    integer :: ThisSoilLayer

! Outputs for use at main program level 
    logical :: Cover  ! TRUE = there is plant cover, FALSE = wood + leaf C stock is below the threshold of 0.5 t/ha = 4.2 Mol/m2 
    
! Inputs set at main program level 
    ! Time-series inputs (currently read in PROSUM_module):
    double precision  :: Temp_oC,&
                         PAR_uMpm2s,&
                         WaterAvail_mpmo,&
                         AtmosphCO2_uLpL,&
                         Herbivores_kgLivepha
    integer           :: Tillage_TF,&
                         Harvest_TF,&
                         PlantType 

! Variables used in sensitivity analysis loops
!    integer :: ThisSensitivityLoopA,ThisSensitivityLoopB ! loop counters
!    integer, parameter :: NumSensitivityLoopsA = 21 
!    integer, parameter :: NumSensitivityLoopsB = 21  
!    double precision, dimension(NumSensitivityLoopsA) :: SensitivityVariableA 
!    double precision, dimension(NumSensitivityLoopsB) :: SensitivityVariableB 

!****************************************************************************************
! Set array sizes
    nlayer = 4
    nnutrient = 6
    nplantbits = 5
    nplanttypes = 6

    if (StandAlone == 1) then 
          write(*,*) 
          write(*,'(a)') '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
          write(*,'(a)') '~                                                       ~'
          write(*,'(a)') '~  PROSUM                                               ~'
          write(*,'(a)') '~         a simple model of vegetation productivity     ~'
          write(*,'(a)') '~                                                       ~'
          write(*,'(a)') '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'
          write(*,*) 
          write(*,'(a)') 'running...'
          write(*,*) 
    end if ! (StandAlone == 1)

!****************************************************************************************
! LOOP FOR REPEAT RUNS OF THE MODEL FOR SENSITIVITY ANALYSES
!    if (StandAlone == 1) then 
!        SensitivityVariableA = (/0.0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0/)
!        SensitivityVariableB = (/0.0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,1.0/)
!        TempFilePath=trim(basedir)//"PROSUM_sensitivity_outputs.csv"
!        open(309,FILE=TempFilePath)
!        ! write(309,'(" Temp_oC,Herbivores_kgLivepha,Plant_e(1),PlantC_tpha")')
!        write(309,'("Month, NutAvail_e(2),NutAvail_e(3),Plant_e(1),PlantC_tpha")')
!    end if ! (StandAlone == 1)
!
!    do ThisSensitivityLoopB = 1, 1 !NumSensitivityLoopsB 
!    do ThisSensitivityLoopA = 1, 1 !NumSensitivityLoopsA 
    ! write(*,'(2(i11,",",:))') ThisSensitivityLoopA, ThisSensitivityLoopB
    ! use a line similar to this, in the right place, to reset the value you want to vary: 
    ! MyVaryingVariable = SensitivityVariableA(ThisSensitivityLoopA)

!****************************************************************************************
! INITIALISE    
    InitialDynamicTerminal = 1 ! Set flag to 1 = Initial phase
    ! read run length from parameters file
    TempFilePath=trim(basedir)//"PROSUM_parameters.csv"
        OPEN(10,FILE=TempFilePath)
            READ(10,*) ! skip header line
            READ(10,*) Num_months_of_parameters ! ignore following parameters for now
        CLOSE(10)
    Month_end = Num_months_of_parameters 
        
    call SoilTrECProsum_allocate(nlayer,nnutrient,nplantbits,nplanttypes,Num_months_of_parameters)


    Call FillArrays(Num_months_of_parameters, StandAlone)  ! Read time series data from file. 
    Month = Month_start
    ! Set constant values (for now) for variables that will be dynamically supplied by other models
!    BottomDepth_s = (/0.05,0.1,0.15,0.2,0.3,0.4,0.5,1.0,2.0,5.0/)
    BottomDepth_s = (/0.05,0.1,0.5,2.0/)

    Thickness_s(1) = BottomDepth_s(1)
    do ThisSoilLayer = 2,nlayer,1 
        Thickness_s(ThisSoilLayer) = BottomDepth_s(ThisSoilLayer) - BottomDepth_s(ThisSoilLayer - 1)
    end do ! ThisSoilLayer loop
    do ThisSoilLayer = 1,nlayer,1 
        ThicknessProp_s(ThisSoilLayer) = Thickness_s(ThisSoilLayer) / sum(Thickness_s, dim = 1)  
    end do ! ThisSoilLayer loop

    ! PROSUM - set starting values for arguments, and call for the first time 
    Temp_oC             = Monthly_pars(int(Month)-Month_start+1,2)      ! this will also be needed by other modules
    PAR_uMpm2s           = Monthly_pars(int(Month)-Month_start+1,3)
!    WaterAvail_mpmo     = Monthly_pars(int(Month)-Month_start+1,4)      ! this should be supplied by another module
    AtmosphCO2_uLpL     = Monthly_pars(int(Month)-Month_start+1,4)
    Herbivores_kgLivepha     = Monthly_pars(int(Month)-Month_start+1,5)
    Tillage_TF         = int(0.000001 + Monthly_pars(int(Month)-Month_start+1,6))
    Harvest_TF          = int(0.000001 + Monthly_pars(int(Month)-Month_start+1,7))
    PlantType          = int(0.000001 + Monthly_pars(int(Month)-Month_start+1,8))

! Obtain NutAvail_es from time-series input file. In SoilTrEC1D this will be done by 1DTM/CAST/SAFE 

    WaterAvail_s = Monthly_nutrients(Month,2:5)

    NutAvail_es(2,:) = Monthly_nutrients(Month,6:9)
    NutAvail_es(3,:) = Monthly_nutrients(Month,10:13)
    NutAvail_es(4,:) = Monthly_nutrients(Month,14:17)
    NutAvail_es(5,:) = Monthly_nutrients(Month,18:21)
    NutAvail_es(6,:) = Monthly_nutrients(Month,22:25)

    NutAvail_e(2) = NutAvail_es(2,1) + NutAvail_es(2,2) + NutAvail_es(2,3) + NutAvail_es(2,4) 
    NutAvail_e(3) = NutAvail_es(3,1) + NutAvail_es(3,2) + NutAvail_es(3,3) + NutAvail_es(3,4) 
    NutAvail_e(4) = NutAvail_es(4,1) + NutAvail_es(4,2) + NutAvail_es(4,3) + NutAvail_es(4,4) 
    NutAvail_e(5) = NutAvail_es(5,1) + NutAvail_es(5,2) + NutAvail_es(5,3) + NutAvail_es(5,4) 
    NutAvail_e(6) = NutAvail_es(6,1) + NutAvail_es(6,2) + NutAvail_es(6,3) + NutAvail_es(6,4) 

    call PROSUM(InitialDynamicTerminal,StandAlone,Month,Temp_oC,PAR_uMpm2s,AtmosphCO2_uLpL,Herbivores_kgLivepha,Tillage_TF,Harvest_TF,PlantType,Cover,nlayer,nnutrient,nplantbits,nplanttypes)

!****************************************************************************************
! DYNAMIC
    InitialDynamicTerminal = 2 ! Set flag to 2 = Dynamic phase
    ! Call the PROSUM subroutines for each Month of the simulation
    do Month = Month_start,Month_end,1 

    ! PROSUM - set values for input arguments 
        Temp_oC             = Monthly_pars(int(Month)-Month_start+1,2)
        PAR_uMpm2s           = Monthly_pars(int(Month)-Month_start+1,3)
!        WaterAvail_mpmo     = Monthly_pars(int(Month)-Month_start+1,4)
        AtmosphCO2_uLpL     = Monthly_pars(int(Month)-Month_start+1,4)
        Herbivores_kgLivepha     = Monthly_pars(int(Month)-Month_start+1,5)
        Tillage_TF         = int(Monthly_pars(int(Month)-Month_start+1,6))
        Harvest_TF          = int(Monthly_pars(int(Month)-Month_start+1,7))
        PlantType          = int(0.000001 + Monthly_pars(int(Month)-Month_start+1,8))

! SENSITIVITY ANALYSIS SUBSTITUTE LINES:
!    Temp_oC = Temp_oC * 3 *SensitivityVariableA(ThisSensitivityLoopA)
!    Herbivores_kgLivepha = SensitivityVariableB(ThisSensitivityLoopB)*1000

    WaterAvail_s = Monthly_nutrients(Month,2:5)

    NutAvail_es(2,:) = Monthly_nutrients(Month,6:9)
    NutAvail_es(3,:) = Monthly_nutrients(Month,10:13)
    NutAvail_es(4,:) = Monthly_nutrients(Month,14:17)
    NutAvail_es(5,:) = Monthly_nutrients(Month,18:21)
    NutAvail_es(6,:) = Monthly_nutrients(Month,22:25)

! SENSITIVITY ANALYSIS SUBSTITUTE LINES:
!    NutAvail_es(2,:) = NutAvail_es(2,:) * SensitivityVariableA(ThisSensitivityLoopA)   ! N availability
!    NutAvail_es(3,:) = NutAvail_es(3,:) * SensitivityVariableB(ThisSensitivityLoopB)   ! P availability
       
        call PROSUM(InitialDynamicTerminal,StandAlone,Month,Temp_oC,PAR_uMpm2s,AtmosphCO2_uLpL,Herbivores_kgLivepha,Tillage_TF,Harvest_TF,PlantType,Cover,nlayer,nnutrient,nplantbits,nplanttypes)
    enddo ! end of this timestep

!****************************************************************************************
! SENSITIVITY OUTPUTS    
!    if (StandAlone == 1) then 
!!        write(309,'(5(f11.4,",",:))') month, Temp_oC, Herbivores_kgLivepha, Plant_e(1), Plant_e(1) * 12.01 * 10000 / 1000000 
!        write(309,'(5(f11.4,",",:))') month, NutAvail_e(2)*12*14.007*10000/1000, NutAvail_e(3)*12*30.97*10000/1000, Plant_e(1), Plant_e(1)*12.01*10000/1000000  
!    end if 

!****************************************************************************************
! TERMINAL    
    InitialDynamicTerminal = 3 ! Set flag to 3 = Terminal phase
    call PROSUM(InitialDynamicTerminal,StandAlone,Month,Temp_oC,PAR_uMpm2s,AtmosphCO2_uLpL,Herbivores_kgLivepha,Tillage_TF,Harvest_TF,PlantType,Cover,nlayer,nnutrient,nplantbits,nplanttypes)

!    end do ! end of SensitivityLoopA
!    end do ! end of SensitivityLoopA

    if (StandAlone == 1) then 
!        close(309) ! close sensitivity file
        write(*,*) 'Run finished at month ', Month   
        pause
    end if ! (StandAlone == 1)

end program PROSUM_main
