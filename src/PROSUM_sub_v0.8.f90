subroutine PROSUM(InitialDynamicTerminal,StandAlone,Month,Temp_oC,PAR_uMpm2s,AtmosphCO2_uLpL,&
Herbivores_kgLivepha,Tillage_TF,Harvest_TF,PlantType,Cover,nlayer,nnutrient,nplantbits,nplanttypes)

! Created:  18/10/11
! By:       Ed Rowe
! Why:      Simple plant production and succession model
! Documentation:     see \\TBNB\SHARED\PROJECTS\EU SoilTrEC NEC04307\Work Package 3\PROSUM\ReadMe PROSUM.docx
! BASE UNITS: day, metre, Mol, oC 
! Version history:
! v0.1 5/12/11 Implementing from design in Simile \\Tbnb\shared\PROJECTS\EU SoilTrEC NEC04307\Work Package 3\PROSUM\PROSUM v0.1.sml
!  to fit reqs of other SoilTrEC modules. 
!  Array algebra used where possible, with arrays for: chemical elements; SoilLayers; plant bits. 
!  (see PROSUM_module_v0.1.F90) 
!  All array variables are suffixed, e.g. "_e" for per-chemical-element vectors, 
!  "_es" for per-chemical-element x per-SoilLayer arrays   
! 19/3/12 Adding in mycorrhizae and exudates. 
!   Mycorrhizae affect effective availability of nutrient elements, i.e the proportion of the NutAvail_e 
!   (input argument to PROSUM) that is taken up, via effects on the proportion of soil that is explored. 
!   Proportional allocation of [CNPCaMgK] to mycorrhizae, and of C to exudates, is set by input parameters. 
!   Exudates assumed to contain only C (c.f. oxalate), not other elements.  
!   Exudate effects on nutrient availability to be handled in the weathering module.
! 15/6/12 Changed to return nutrient uptake rather than nutrient remaining 
! 20/6/12 Changed to distribute water uptake over soil layers
! 11/9/12 Revised functions for primary production limitations
! 25/9/12 Hard-wired all parameter values into this subroutine, and deleted parameter-reading routine.
! 6/11/12 revised all per-layer and per-element arrays to be allocatable
! 26/11/12 revised nutrient allocation algorithm to use min and max C/N per plant bit, not for whole plant
! 3/7/13 v0.5 - implementing changes agreed at KosterKappel workshop:
!   Expose params that users might want to change e.g. run length for stand-alone version (DONE)
!   Tillage should reset above-ground biomass to zero  (DONE - ALSO BELOW-GROUND CUT 95%)
!   Remove harvested products, and add an exposed HarvestIndex input 
!     (DONE: Harvest Index is set by HarvestRemoval_b. HarvestLitter_b sets the proportion of plant 
!        material THAT REMAINS AFTER HARVEST that then enters the litter pool.)
! 1/8/13 v0.6 - debugging the version that was integrated by Johan. Changes indicated by ppp.  
!   Values for some variables were being lost between calls: moved to PROSUM module, or given 'save' attribute.
! 21/5/14 v0.7 - Error corrected: previously Removal and Litter were read in in the wrong order
! v0.8 Sept2014 bug with excessive water uptake corrected 
! 14/10/14 (no new version number): adapted to run PROSUM_sub_v0.8 (the version used in 1DICZ Oct2014), stand-alone 
!     Changes indicated by bbb
! Things to do list:
!   Fix low production in 1DICZ (NOT YET DONE)
!   Include N fixation (NOT YET DONE)
!   Add fog inputs controlling moss growth (NOT YET DONE)
!   Fix the issue that after harvest, low roots -> water limitation despite irrigation (NOT YET DONE)
!
! 14/10/14 (no new version number): added plant stocks of P, Ca, Mg, K to the outputs file


    use PROSUM_module

!****************************************************************************************
! DECLARATIONS:    
    implicit none
! Arguments passed:
    ! Housekeeping variables from main model:
        integer, intent(in) :: InitialDynamicTerminal,& ! control of program stage: 1=Initial, 2= Dynamic, 3=Terminal
                               StandAlone               ! control of run type: 0=run from SoilTrEC1D, 1= run stand-alone
        integer, intent(in) :: nlayer,&     ! (suffix = _s): 1=Top SoilLayer,2=second SoilLayer down, ...
                               nnutrient,&  ! (suffix = _e): 1=C, 2=N, 3=P, 4=Ca, 5=Mg, 6=K
                               nplantbits,& ! (suffix = _b): 1=Leaf, 2=Wood, 3=Root, 4=Mycorrhizae, 5=Exudate
                               nplanttypes  ! (suffix = _t): 1=WoodDecMyc,2=WoodDecNon,3=WoodEverMyc,4=WoodEverNon,5=HerbMyc,6=HerbNon
    ! Driving time-series variables from main model:
        double precision,  intent(in) :: Month,&
                                         Temp_oC,&
                                         PAR_uMpm2s,&
                                         AtmosphCO2_uLpL,&
                                         Herbivores_kgLivepha  
        integer,           intent(in) :: Tillage_TF,&
                                         Harvest_TF,&
                                         PlantType  ! 1 = Woody deciduous mycorrhizal
                                                    ! 2 = Woody deciduous non-mycorrhizal
                                                    ! 3 = Woody evergreen mycorrhizal
                                                    ! 4 = Woody evergreen non-mycorrhizal
                                                    ! 5 = Herbaceous mycorrhizal
                                                    ! 6 = Herbaceous non-mycorrhizal

    ! Time-series outputs of interest to CAST:
        logical, intent(out) :: Cover  ! TRUE = there is plant cover, FALSE = plant cover is below the threshold
 
 ! Internal variables:
    ! Housekeeping variables
        character (len=120) :: TemporaryChar
        integer :: ThisSoilLayer,ThisNut,ThisPlantType,ThisPlantBit,ThisMOY, &    ! counter variables
                   TemporaryInt, &                              ! for reading in  
                   MOY                                          ! Month of year (Jan = 1, Dec = 12)
        double precision, dimension(nnutrient)  :: NutAccessible_e ,&
                                                   RemobCapacity_e ,&
                                                   Remobilisable_e ,&
                                                   CXratioSenescLeaf_e
! ppp changes 1/8/13: MaxCXratioSenescLeaf_e moved into the PROSUM module and made allocatable
                                                    
    ! States (now declared in the module):
       ! double precision, dimension(nnutrient)  :: Leaf_e,&
       !                                            Wood_e,&
       !                                            Root_e,&
       !                                            Mycor_e        
    ! Rates: 
        double precision, dimension(nnutrient)  :: Uptake_e,&
                                                   LeafLitterfall_e,&
                                                   WoodLitterfall_e,&
                                                   RootLitterfall_e,&
                                                   MycorLitterfall_e
    
    ! Model parameters  
! ppp changes 1/8/13: "save" attribute added to PROSUM parameters
        double precision, save :: SpecificRootLength_mpMC     ,&
                            SpecificMycorLength_mpMC    ,&
                            MaxCfix_Mpm2mo              ,&
                            TempRisePos                 ,&
                            TempRiseWidth               ,&
                            TempFallPos                 ,&
                            TempFallWidth               ,&
                            PARexponent                 ,&
                            WaterUE_MCpm3               ,&
                            CO2Factor                   ,&
                            CO2Exponent                 ,&
                            ThresholdPlantCForCover_Mpm2,& 
                            RootDepthBeta 
                                
        integer, save ::          MOYstart  

    ! Intermediate variables:
        double precision, dimension(nnutrient) :: NutDisposable_e ,&
                                                  MaxCFixPropGivenNut_e
        double precision, dimension(nnutrient,nlayer) :: ExploredVol_es,&        ! soil volume (m3) explored for each element, by SoilLayer
                                                         ExploredVolProp_es,&    ! soil volume explored for each element as prop of SoilLayer volume
                                                         NutAccessible_es,&      ! nutrient (Mol) accessible by element, by SoilLayer
                                                         NutPropAccessible_es    ! nutrient accessible within SoilLayer as prop of total accessible over all SoilLayers, by element
        double precision :: MaxCfixGivenCO2_prop,&
                            MaxCfixGivenTemp_prop,&
                            MaxCfixGivenLight_prop,&
                            MaxCfixGivenWater_prop,&
                            MaxCfixGivenAllPrimaryConstraints_Mpm2,&
                            MaxCFixGivenLimitingNut,&
                            ActualCfixed,&
                            ExudateCflux,&
                            WaterAccessible_mpmo,&
                            Water_transpired
                            
        double precision, dimension(nplantbits) :: KDrought_b, Kautumn_b, KHerbivory_b, KHarvest_b, KTillage_b ! values this month

SELECT CASE (InitialDynamicTerminal)  ! Program stage: CASE 1 = Initial; CASE 2 = Dynamic; CASE 3 = Terminal

!****************************************************************************
CASE (1) ! INITIAL *********************************************************

    ! read values for fixed scalar (single-value) parameters
	    TempFilePath=trim(basedir)//"PROSUM_parameters.csv"
        OPEN(10,FILE=TempFilePath)
            READ(10,*) ! skip header line
            READ(10,*) &
                Month_end ,&
                MOYstart ,&
                SpecificRootLength_mpMC,&
                SpecificMycorLength_mpMC,&
                MaxCfix_Mpm2mo,&
                TempRisePos,&
                TempRiseWidth,&
                TempFallPos,&
                TempFallWidth,&
                PARexponent,&
                WaterUE_MCpm3,&
                CO2Factor,&
                CO2Exponent,&
                ThresholdPlantCForCover_Mpm2,&
                RootDepthBeta,&
                InitLeaf_e(1),&
                InitWood_e(1),& 
                InitRoot_e(1),& 
                InitMycor_e(1)
        CLOSE(10)

    ! read values for parameters that depend on plant type
	    TempFilePath=trim(basedir)//"PROSUM_parameters_by_plant_type.csv"
        OPEN(10,FILE=TempFilePath)
        READ(10,*) ! skip header line
        do ThisPlantType = 1,nplanttypes,1 
            READ(10,*) &
                TemporaryInt,&
                TemporaryChar,&
                AllocProp_bt(:,ThisPlantType),&     ! 5 values for the five plant parts
                Kseasonal_bmt(2,1,ThisPlantType),&  ! values for wood for Jan (copied to other months later)
                Kseasonal_bmt(1,:,ThisPlantType),&  ! 12 values for leaf, Jan-Dec
                Kseasonal_bmt(3,:,ThisPlantType),&  ! 12 values for root, Jan-Dec (copied to mycorrhizae later)
                KharvestRemoval_bt(1,ThisPlantType),& ! Error corrected 21/5/14: previously Removal and Litter were read in in the wrong order
                KharvestRemoval_bt(2,ThisPlantType),&
                KharvestRemoval_bt(3,ThisPlantType),&
                KharvestRemoval_bt(4,ThisPlantType),&
                KharvestLitter_bt(1,ThisPlantType),&
                KharvestLitter_bt(2,ThisPlantType),&
                KharvestLitter_bt(3,ThisPlantType),&
                KharvestLitter_bt(4,ThisPlantType)
        end do !ThisPlantType = 2,nplanttypes,1                
        CLOSE(10)
        do ThisMOY = 2,12,1 
            Kseasonal_bmt(2,ThisMOY,:) = Kseasonal_bmt(2,1,:)   ! values for wood for Jan copied to other months
        end do
        Kseasonal_bmt(4,:,:) = Kseasonal_bmt(3,:,:)         ! values for root copied to mycorrhizae

    ! read values for parameters that depend on element (i.e. nutrient element, or carbon. NB the first diffusion radius is for water not C) 
	    TempFilePath=trim(basedir)//"PROSUM_parameters_by_nutrient_element.csv"
        OPEN(10,FILE=TempFilePath)
        READ(10,*) ! skip header line
        do ThisNut = 1,nnutrient,1 
            READ(10,*) &
                TemporaryChar,&
                DiffusionRadius_e(ThisNut),&
                MinCXratio_eb(ThisNut,1),&
                MinCXratio_eb(ThisNut,2),&
                MinCXratio_eb(ThisNut,3),&
                MinCXratio_eb(ThisNut,4),&
                MinCXratio_eb(ThisNut,5),&
                MaxCXratio_eb(ThisNut,1),&
                MaxCXratio_eb(ThisNut,2),&
                MaxCXratio_eb(ThisNut,3),&
                MaxCXratio_eb(ThisNut,4),&
                MaxCXratio_eb(ThisNut,5),&
                MaxCXratioSenescLeaf_e(ThisNut)
        end do !ThisNut = 1,nnutrient,1                
        CLOSE(10)

    ! read plant parameter values that depend on PlantBit but not PlantType
	    TempFilePath=trim(basedir)//"PROSUM_parameters_by_plant_bit.csv"
        OPEN(10,FILE=TempFilePath)
        READ(10,*) ! skip header line
        do ThisPlantBit = 1,nplantbits,1 
            READ(10,*) &
                TemporaryChar,&
                Kdroughtm_b(ThisPlantBit),&
                Kdroughtc_b(ThisPlantBit),&
                Kherbivorym_b(ThisPlantBit),&
                KherbivoryMax_b(ThisPlantBit),&
                KTillageMax_b(ThisPlantBit)
        end do !ThisPlantBit = 1,nplantbits,1                
        CLOSE(10)

    WeightedMaxCXratio_e(2) = sum(AllocProp_bt(:,PlantType)) / sum(AllocProp_bt(:,PlantType) / MaxCXratio_eb(2,:)) ! Max C/N ratio of all new growth
    WeightedMaxCXratio_e(3) = sum(AllocProp_bt(:,PlantType)) / sum(AllocProp_bt(:,PlantType) / MaxCXratio_eb(3,:)) ! (inverted for weighting)
    WeightedMaxCXratio_e(4) = sum(AllocProp_bt(:,PlantType)) / sum(AllocProp_bt(:,PlantType) / MaxCXratio_eb(4,:)) 
    WeightedMaxCXratio_e(5) = sum(AllocProp_bt(:,PlantType)) / sum(AllocProp_bt(:,PlantType) / MaxCXratio_eb(5,:)) 
    WeightedMaxCXratio_e(6) = sum(AllocProp_bt(:,PlantType)) / sum(AllocProp_bt(:,PlantType) / MaxCXratio_eb(6,:)) 


    InitLeaf_e(2:6)  = InitLeaf_e(1) / MaxCXratio_eb(2:6,1)
    InitWood_e(2:6)  = InitWood_e(1) / MaxCXratio_eb(2:6,2)
    InitRoot_e(2:6)  = InitRoot_e(1) / MaxCXratio_eb(2:6,3)
    InitMycor_e(2:6)  = InitMycor_e(1) / MaxCXratio_eb(2:6,4)

    ! open file for outputs
	    TempFilePath=trim(basedir)//"PROSUM_outputs.csv" ! main drivers and outputs
        OPEN(53,FILE=TempFilePath)
        write(53,'("&
Month,Temp_oC,NutAvail_N_Mpm2mo,NutAvail_P_Mpm2mo,NutAvail_Ca_Mpm2mo,NutAvail_Mg_Mpm2mo,NutAvail_K_Mpm2mo,&
PAR_uMpm2s,WaterAccessible_mpmo,AtmosphCO2_uLpL,Herbivores_kgLivepha,&
MaxCfixGivenTemp_prop,MaxCfixGivenLight_prop,MaxCfixGivenWater_prop,MaxCfixGivenCO2_prop,&
MaxCFixGivenN_prop,MaxCFixGivenP_prop,MaxCFixGivenCa_prop,MaxCFixGivenMg_prop,MaxCFixGivenK_prop,&
Plant_C_Mpm2,Plant_N_Mpm2,Plant_P_Mpm2,Plant_Ca_Mpm2,Plant_Mg_Mpm2,Plant_K_Mpm2,&
Uptake_C_Mpm2mo,Uptake_N_Mpm2mo,Uptake_P_Mpm2mo,Uptake_Ca_Mpm2mo,Uptake_Mg_Mpm2mo,Uptake_K_Mpm2mo,&
RootLengthDensity_Layer1_mpm3,RootLengthDensity_Layer2_mpm3,RootLengthDensity_Layer3_mpm3,RootLengthDensity_Layer4_mpm3,&
MycorLengthDensity_Layer1_mpcm3,MycorLengthDensity_Layer2_mpcm3,MycorLengthDensity_Layer3_mpcm3,MycorLengthDensity_Layer4_mpcm3,&
Litterfall_C_Layer1_Mpm2mo,Litterfall_C_Layer2_Mpm2mo,Litterfall_C_Layer3_Mpm2mo,Litterfall_C_Layer4_Mpm2mo,&
Leaf_C_Mpm2,Wood_C_Mpm2,Root_C_Mpm2,Mycor_C_Mpm2,&
Leaf_N_Mpm2,Wood_N_Mpm2,Root_N_Mpm2,&
Kdrought_Leaf, Kseasonal_Leaf, KHerbivory_Leaf,&
Kdrought_Wood, Kseasonal_Wood, KHerbivory_Wood,&
Kdrought_Root, Kseasonal_Root, KHerbivory_Root &
")')

! Calculate intermediate variables with static i.e. fixed values. 
! Only calculated at the start, since these variables are saved between calls
    Thickness_s(1) = BottomDepth_s(1)
    do ThisSoilLayer = 2,nlayer,1 
        Thickness_s(ThisSoilLayer) = BottomDepth_s(ThisSoilLayer) - BottomDepth_s(ThisSoilLayer - 1)
    end do ! ThisSoilLayer loop
    do ThisSoilLayer = 1,nlayer,1 
        ThicknessProp_s(ThisSoilLayer) = Thickness_s(ThisSoilLayer) / sum(Thickness_s, dim = 1)  
    end do ! ThisSoilLayer loop
    RootProp_s(1) = 1 - RootDepthBeta ** BottomDepth_s(1)
    do ThisSoilLayer = 2,nlayer,1 
        RootProp_s(ThisSoilLayer) = 1 - RootDepthBeta ** BottomDepth_s(ThisSoilLayer) - (1 - RootDepthBeta ** BottomDepth_s(ThisSoilLayer - 1)) 
    end do ! ThisSoilLayer loop
    
! Initialise states
    Leaf_e      = InitLeaf_e
    Wood_e      = InitWood_e 
    Root_e      = InitRoot_e 
    Mycor_e     = InitMycor_e 

! end of INITIAL section

!****************************************************************************
CASE (2) ! DYNAMIC ********************************************************** 
! (one timestep; the timestepping loop is in the main program)
    MOY = mod((int(Month) - MOYstart + 1),12)
    if (MOY == 0) MOY = 12
    Plant_e = Leaf_e + Wood_e + Root_e + Mycor_e ! note that Mycor nutrient assumed available for plant growth i.e. strong control by plant

!if (month > 1) then 
!    pause
!    stop
!end if 
! write(*,*) 'MaxCfixGivenTemp_prop', MaxCfixGivenTemp_prop   


! Calculate nutrient and water amounts available to plants, given amounts in soil cells, and 
! root & mycor length density in previous timestep. NB needs initial root + Mycor > 0. 
! Effects of exudates on nutrient availability are handled by CAST and/or SAFE 
    RootLengthDensity_s = RootProp_s * Root_e(1) * SpecificRootLength_mpMC / Thickness_s        ! m/m3
    MycorLengthDensity_s = RootProp_s * Mycor_e(1) * SpecificMycorLength_mpMC / Thickness_s     ! m/m3
    TotalRootMycLength_s = (RootLengthDensity_s + MycorLengthDensity_s) * Thickness_s           ! m/m2
    do ThisSoilLayer = 1,nlayer,1 
        ExploredVol_es(:,ThisSoilLayer) = 3.1416 * (DiffusionRadius_e * DiffusionRadius_e)* TotalRootMycLength_s(ThisSoilLayer)
        ! NB DiffusionRadius_e(1) is not needed for Carbon, so I am using it for water (!!)
        ExploredVolProp_es(:,ThisSoilLayer) = min(1.0, ExploredVol_es(:,ThisSoilLayer) / Thickness_s(ThisSoilLayer)) 
    end do ! ThisSoilLayer loop
    NutAvail_e = sum(NutAvail_es,dim=2)
    NutAccessible_es(2:6,:) = NutAvail_es(2:6,:) * ExploredVolProp_es(2:6,:)
    NutAccessible_e = sum(NutAccessible_es,dim=2) ! this means sum over 2nd dimension i.e. SoilLayers 
    WaterAccessible_s(:) = WaterAvail_s(:) * ExploredVolProp_es(1,:) 
    WaterAccessible_mpmo=sum(WaterAccessible_s)
    do ThisSoilLayer = 1,nlayer,1 
        NutPropAccessible_es(:,ThisSoilLayer) = NutAccessible_es(:,ThisSoilLayer) / sum(NutAccessible_es,dim=2) ! this means sum over 2nd dimension i.e. soil SoilLayers 
        WaterPropAccessible_s(ThisSoilLayer) = WaterAccessible_s(ThisSoilLayer) / sum(WaterAccessible_s) 
    end do ! ThisSoilLayer loop
    MaxCfixGivenLight_prop  = 1 - exp(PARexponent * PAR_uMpm2s)  
    MaxCfixGivenCO2_prop    = CO2Factor * log(CO2Exponent * AtmosphCO2_uLpL)
    MaxCfixGivenTemp_prop   = 0.5*(TANH(  (Temp_oC-TempRisePos)/TempRiseWidth  ) - TANH(  (Temp_oC-TempFallPos)/TempFallWidth  )) ! Double sigmoid
    MaxCfixGivenWater_prop  = max(0.0,min(1.0,WaterAccessible_mpmo * WaterUE_MCpm3 / MaxCfix_Mpm2mo))
    MaxCfixGivenAllPrimaryConstraints_Mpm2 = MaxCfix_Mpm2mo * min(MaxCfixGivenCO2_prop, MaxCfixGivenTemp_prop, MaxCfixGivenLight_prop, MaxCfixGivenWater_prop)
    CurrentCX_eb(:,1) = Leaf_e(1)/Leaf_e(:)
    CurrentCX_eb(:,2) = Wood_e(1)/Wood_e(:)
    CurrentCX_eb(:,3) = Root_e(1)/Root_e(:)
    CurrentCX_eb(:,4) = Mycor_e(1)/Mycor_e(:)
    PrimaryLimitedGrowth_b = MaxCfixGivenAllPrimaryConstraints_Mpm2 * AllocProp_bt(:,PlantType)  
    NutRelocatable_e = (Leaf_e(:) - Leaf_e(1)/MaxCXratio_eb(:,1)) + (Root_e(:) - Root_e(1)/MaxCXratio_eb(:,3))
    NutRelocatable_e = max(0.0,NutRelocatable_e)
    NutDisposable_e = NutRelocatable_e  + NutAccessible_e  
    NutLimitedGrowth_e = WeightedMaxCXratio_e * NutDisposable_e  ! This is the Cfix possible given each element-limitation.
    ActualCfixed = min(MaxCfixGivenAllPrimaryConstraints_Mpm2, minval(NutLimitedGrowth_e(2:6)))
    Uptake_e(1) = ActualCfixed
                                                      
    MaxCFixPropGivenNut_e = NutLimitedGrowth_e / MaxCfix_Mpm2mo  ! This is not actually used, but output as an indicator
    
    Leaf_e(1)  = Leaf_e(1)  + ActualCfixed*AllocProp_bt(1,PlantType)
    Wood_e(1)  = Wood_e(1)  + ActualCfixed*AllocProp_bt(2,PlantType)
    Root_e(1)  = Root_e(1)  + ActualCfixed*AllocProp_bt(3,PlantType)
    Mycor_e(1) = Mycor_e(1) + ActualCfixed*AllocProp_bt(4,PlantType)
    Exudate_e(1) =            ActualCfixed*AllocProp_bt(5,PlantType) ! (there is no exudate stock)

    MaxUptake_e =   Leaf_e(1) / MinCXratio_eb(:,1) &
                    + Wood_e(1) / MinCXratio_eb(:,2) & 
                    + Root_e(1) / MinCXratio_eb(:,3) & 
                    + Mycor_e(1) / MinCXratio_eb(:,4) & 
                    + Exudate_e(1) / MinCXratio_eb(:,5) &
                    - Plant_e

    do ThisNut = 2,6,1 

        if (MaxUptake_e(ThisNut) > NutDisposable_e(ThisNut)) then ! i.e. there isn't enough to fill luxury storage
            ! take up all accessible N and distribute it over plant parts:
            ! 1. check all are not above MaxCXratio, and if they are add some
                if (Leaf_e(1) / Leaf_e(ThisNut) > MaxCXratio_eb(ThisNut,1)) then 
                    LeafUptake_e(ThisNut) = Leaf_e(1)/MaxCXratio_eb(ThisNut,1) - Leaf_e(ThisNut)
                    Leaf_e(ThisNut) = Leaf_e(ThisNut) + LeafUptake_e(ThisNut)
                    NutDisposable_e(ThisNut) = NutDisposable_e(ThisNut) - LeafUptake_e(ThisNut)
                end if
                if (Wood_e(1) / Wood_e(ThisNut) > MaxCXratio_eb(ThisNut,2)) then 
                    WoodUptake_e(ThisNut) = Wood_e(1)/MaxCXratio_eb(ThisNut,2) - Wood_e(ThisNut)
                    Wood_e(ThisNut) = Wood_e(ThisNut) + WoodUptake_e(ThisNut)
                    NutDisposable_e(ThisNut) = NutDisposable_e(ThisNut) - WoodUptake_e(ThisNut)
                end if
                if (Root_e(1) / Root_e(ThisNut) > MaxCXratio_eb(ThisNut,3)) then 
                    RootUptake_e(ThisNut) = Root_e(1)/MaxCXratio_eb(ThisNut,3) - Root_e(ThisNut)
                    Root_e(ThisNut) = Root_e(ThisNut) + RootUptake_e(ThisNut)
                    NutDisposable_e(ThisNut) = NutDisposable_e(ThisNut) - RootUptake_e(ThisNut)
                end if
                if (Mycor_e(1) / Mycor_e(ThisNut) > MaxCXratio_eb(ThisNut,4)) then 
                    MycorUptake_e(ThisNut) = Mycor_e(1)/MaxCXratio_eb(ThisNut,4) - Mycor_e(ThisNut)
                    Mycor_e(ThisNut) = Mycor_e(ThisNut) + MycorUptake_e(ThisNut)
                    NutDisposable_e(ThisNut) = NutDisposable_e(ThisNut) - MycorUptake_e(ThisNut)
                end if
                Exudate_e(ThisNut) = Exudate_e(1) / MaxCXratio_eb(ThisNut,5) 
                NutDisposable_e(ThisNut) = NutDisposable_e(ThisNut) - Exudate_e(ThisNut) 

            ! 2. Add remaining to Root until CXratio reaches MinCXratio
                RootUptake_e(ThisNut) = min(Root_e(1)/MinCXratio_eb(ThisNut,3) - Root_e(ThisNut), NutDisposable_e(ThisNut))
                Root_e(ThisNut) = Root_e(ThisNut) + RootUptake_e(ThisNut)
                NutDisposable_e(ThisNut) = NutDisposable_e(ThisNut) - RootUptake_e(ThisNut)

            ! 3. Add remaining to Leaf 
                Leaf_e(ThisNut) = Leaf_e(ThisNut) + NutDisposable_e(ThisNut)  
            
            Uptake_e(ThisNut) = NutAccessible_e(ThisNut)
                    
        else ! i.e. if (MaxUptake_e(ThisNut) < NutDisposable_e(ThisNut)) ! i.e. there is enough to fill luxury storage
            ! fill all pools to reach MinCXratio
            Uptake_e(ThisNut) =   Leaf_e(1)      / MinCXratio_eb(ThisNut,1) - Leaf_e(ThisNut) &
                            + Wood_e(1)    / MinCXratio_eb(ThisNut,2) - Wood_e(ThisNut) &
                            + Root_e(1)    / MinCXratio_eb(ThisNut,3) - Root_e(ThisNut) &
                            + Mycor_e(1)   / MinCXratio_eb(ThisNut,4) - Mycor_e(ThisNut) &
                            + Exudate_e(1) / MinCXratio_eb(ThisNut,5)
            Leaf_e(ThisNut)   = Leaf_e(1)     / MinCXratio_eb(ThisNut,1)
            Wood_e(ThisNut)   = Wood_e(1)     / MinCXratio_eb(ThisNut,2)
            Root_e(ThisNut)   = Root_e(1)     / MinCXratio_eb(ThisNut,3)
            Mycor_e(ThisNut)  = Mycor_e(1)    / MinCXratio_eb(ThisNut,4)
            Exudate_e(ThisNut)= Exudate_e(1)  / MinCXratio_eb(ThisNut,5)
        end if ! (MaxUptake_e(ThisNut) > NutDisposable_e(ThisNut))

    end do ! ThisNut loop
    

    CurrentCX_eb(:,1) = Leaf_e(1)/Leaf_e(:)
    CurrentCX_eb(:,2) = Wood_e(1)/Wood_e(:)
    CurrentCX_eb(:,3) = Root_e(1)/Root_e(:)
    CurrentCX_eb(:,4) = Mycor_e(1)/Mycor_e(:)

! Calculate exudate C flux for use by CAST or 1DTM in subsequent timestep. Note that exudate flux is also included in litterfall.
    ExudateCflux_s = Exudate_e(1) * RootProp_s

! Calculate C and nutrient litterfall rates 
    KDrought_b(1) = max(0.0, WaterAccessible_mpmo * Kdroughtm_b(1) + Kdroughtc_b(1))
    KDrought_b(2) = max(0.0, WaterAccessible_mpmo * Kdroughtm_b(2) + Kdroughtc_b(2))
    KDrought_b(3) = max(0.0, WaterAccessible_mpmo * Kdroughtm_b(3) + Kdroughtc_b(3))
    KDrought_b(4) = max(0.0, WaterAccessible_mpmo * Kdroughtm_b(4) + Kdroughtc_b(4))
    
    Kseasonal_b = Kseasonal_bmt(:,MOY,PlantType)
    
    KHerbivory_b(1) = max(0.0, min(KherbivoryMax_b(1), Kherbivorym_b(1) * Herbivores_kgLivepha))
    KHerbivory_b(2) = max(0.0, min(KherbivoryMax_b(2), Kherbivorym_b(2) * Herbivores_kgLivepha))
    KHerbivory_b(3) = max(0.0, min(KherbivoryMax_b(3), Kherbivorym_b(3) * Herbivores_kgLivepha))
    KHerbivory_b(4) = max(0.0, min(KherbivoryMax_b(4), Kherbivorym_b(4) * Herbivores_kgLivepha))

    if (Harvest_TF == 1) then 
        Leaf_e      = Leaf_e -  Leaf_e * KharvestRemoval_bt(1,PlantType)
        Wood_e      = Wood_e -  Wood_e * KharvestRemoval_bt(2,PlantType)
        Root_e      = Root_e -  Root_e * KharvestRemoval_bt(3,PlantType)
        Mycor_e     = Mycor_e - Mycor_e * KharvestRemoval_bt(4,PlantType)
        KHarvest_b = KharvestLitter_bt(:,PlantType) 
      else
        KHarvest_b = 0
    end if ! (Harvest_TF)
    
    if (Tillage_TF == 1) then 
        Leaf_e      = Leaf_e -  Leaf_e * KTillageMax_b(1)  
        Wood_e      = Wood_e -  Wood_e * KTillageMax_b(2)
        Root_e      = Root_e -  Root_e * KTillageMax_b(3)
        Mycor_e     = Mycor_e - Mycor_e * KTillageMax_b(4)
        KTillage_b = KTillageMax_b 
      else
        KTillage_b = 0
    end if ! (Tillage_TF)

   
    CurrentCX_eb(:,1) = Leaf_e(1)/Leaf_e(:)
    CurrentCX_eb(:,2) = Wood_e(1)/Wood_e(:)
    CurrentCX_eb(:,3) = Root_e(1)/Root_e(:)
    CurrentCX_eb(:,4) = Mycor_e(1)/Mycor_e(:)
        
    ! Whether to include nutrient remobilisation before senescence:    
    if (.FALSE.) then !  old routine - ignore remobilisation
        LeafLitterfall_e    = Leaf_e  * max(Kdrought_b(1), Kseasonal_b(1), KHerbivory_b(1), KHarvest_b(1), KTillage_b(1))
    else !  new routine - include remobilisation
        ! If seasonal leaf litterfall is greater than other leaf litterfalls
        if (Kseasonal_b(1) > max(Kdrought_b(1), KHerbivory_b(1), KHarvest_b(1), KTillage_b(1))) then
            LeafLitterfall_e(1) = Leaf_e(1)*Kseasonal_b(1)
            do ThisNut = 2,6,1 
                ! Find capacity for lux uptake in root, and amount that could be remobilised from leaf
                RemobCapacity_e(ThisNut) = max(0.0, Root_e(1) / MinCXratio_eb(ThisNut,3) - Root_e(ThisNut)) 
                Remobilisable_e(ThisNut)  = max(0.0, Leaf_e(ThisNut)*Kseasonal_b(1) - Leaf_e(1)*Kseasonal_b(1)/MaxCXratioSenescLeaf_e(ThisNut)) 
                if (RemobCapacity_e(ThisNut) > Remobilisable_e(ThisNut)) then
                    ! Leaf seneseces at max senescent-leaf CX ratio
                    Root_e(ThisNut) = Root_e(ThisNut) + Remobilisable_e(ThisNut)
                    LeafLitterfall_e(ThisNut) = Leaf_e(1)*Kseasonal_b(1)/MaxCXratioSenescLeaf_e(ThisNut)
                    Leaf_e(ThisNut) = Leaf_e(ThisNut) - Remobilisable_e(ThisNut)  
                else
                    Root_e(ThisNut) = Root_e(ThisNut) + RemobCapacity_e(ThisNut)
                    CXratioSenescLeaf_e(ThisNut) = LeafLitterfall_e(1) / (Leaf_e(ThisNut)*Kseasonal_b(1) - RemobCapacity_e(ThisNut))
                    LeafLitterfall_e(ThisNut) = LeafLitterfall_e(1) / CXratioSenescLeaf_e(ThisNut)
                    Leaf_e(ThisNut) = Leaf_e(ThisNut) - RemobCapacity_e(ThisNut)
                end if ! RemobCapacity_e(ThisNut) > Remobilisable_e(ThisNut) 
            end do ! ThisNut = 2,6,1
        else    ! drop leaf litter without remobilisation
            LeafLitterfall_e    = Leaf_e  * max(Kdrought_b(1), KHerbivory_b(1), KHarvest_b(1), KTillage_b(1))
        end if ! If seasonal litterfall is greater than other litterfalls
    end if ! choice of remobilisation or not


        
    ! assume, for now, that all loss of plant material enters the soil as litterfall (no harvest or removal)
    WoodLitterfall_e    = Wood_e  * max(Kdrought_b(2), Kseasonal_b(2), KHerbivory_b(2), KHarvest_b(2), KTillage_b(2))
    RootLitterfall_e    = Root_e  * max(Kdrought_b(3), Kseasonal_b(3), KHerbivory_b(3), KHarvest_b(3), KTillage_b(3))
    MycorLitterfall_e   = Mycor_e * max(Kdrought_b(4), Kseasonal_b(4), KHerbivory_b(4), KHarvest_b(4), KTillage_b(4))
  
! Update states
    Leaf_e      = Leaf_e - LeafLitterfall_e
    Wood_e      = Wood_e - WoodLitterfall_e
    Root_e      = Root_e - RootLitterfall_e
    Mycor_e     = Mycor_e - MycorLitterfall_e


    CurrentCX_eb(:,1) = Leaf_e(1)/Leaf_e(:)
    CurrentCX_eb(:,2) = Wood_e(1)/Wood_e(:)
    CurrentCX_eb(:,3) = Root_e(1)/Root_e(:)
    CurrentCX_eb(:,4) = Mycor_e(1)/Mycor_e(:)

    Plant_e = Leaf_e + Wood_e + Root_e + Mycor_e
    Cover = ((Leaf_e(1) + Wood_e(1)) > ThresholdPlantCForCover_Mpm2)

    ! distribute NutUptake over SoilLayers in same proportions of total uptake as (nutrient accessible within SoilLayer / total nutrient accessible)
    do ThisSoilLayer = 1,nlayer,1 
        NutUptake_es(1:6,ThisSoilLayer) = Uptake_e(1:6) * NutPropAccessible_es(:,ThisSoilLayer) 
       ! write(*,*)'NutUptake_es calculations',ThisSoilLayer,Uptake_e(1:6), NutPropAccessible_es(:,ThisSoilLayer)
    end do ! ThisSoilLayer loop

    RootLengthDensity_s = RootProp_s * Root_e(1) * SpecificRootLength_mpMC / Thickness_s
    MycorLengthDensity_s = RootProp_s * Mycor_e(1) * SpecificMycorLength_mpMC / Thickness_s

    ! calc total transpiration as a function of water availability and leaf C stock
    !Water_transpired = max(0.0,min(WaterAccessible_mpmo,WaterAccessible_mpmo*Leaf_e(1)/0.7))
    Water_transpired = max(0.0,min(WaterAccessible_mpmo, ActualCfixed / WaterUE_MCpm3))  
    Water_transpired_s = Water_transpired * WaterPropAccessible_s
    
    ! top SoilLayer gets all shoot litter, plus a prop of the (root+mycor) litter equal to root prop in the top SoilLayer
    Litterfall_es(1:6,1) = LeafLitterfall_e + WoodLitterfall_e + &
                                RootLitterfall_e *RootProp_s(1) + &
                                MycorLitterfall_e*RootProp_s(1)
    ! deeper SoilLayers get a prop of the (root+mycor) litter equal to root prop in the SoilLayer
    do ThisSoilLayer = 2,nlayer,1 
        Litterfall_es(1:6,ThisSoilLayer) = RootLitterfall_e *RootProp_s(ThisSoilLayer) + &
                                           MycorLitterfall_e*RootProp_s(ThisSoilLayer)
    end do ! ThisSoilLayer loop                            
    ! all SoilLayers also get a prop of the Exudate (C only) equal to root prop in the SoilLayer
    Litterfall_es(1,:) = Litterfall_es(1,:) + ExudateCflux_s ! add exudate C to litterfall C for all SoilLayers

!    write(53,'(A,",",A,",",56(f10.3,",",:))') &
    write(53,'(56(f10.3,",",:))') &
    Month,Temp_oC,NutAvail_e(2),NutAvail_e(3),NutAvail_e(4),NutAvail_e(5),NutAvail_e(6),&
    PAR_uMpm2s,WaterAccessible_mpmo,AtmosphCO2_uLpL,Herbivores_kgLivepha,&
    MaxCfixGivenTemp_prop,MaxCfixGivenLight_prop,MaxCfixGivenWater_prop,MaxCfixGivenCO2_prop,&
    MaxCFixPropGivenNut_e(2),MaxCFixPropGivenNut_e(3),MaxCFixPropGivenNut_e(4),&
    MaxCFixPropGivenNut_e(5),MaxCFixPropGivenNut_e(6),&
    Plant_e(1),Plant_e(2),Plant_e(3),Plant_e(4),Plant_e(5),Plant_e(6),&
    Uptake_e(1),Uptake_e(2),Uptake_e(3),Uptake_e(4),Uptake_e(5),Uptake_e(6),&
    RootLengthDensity_s(1),RootLengthDensity_s(2),RootLengthDensity_s(3),RootLengthDensity_s(4),& !RootLengthDensity_s(5),&
    MycorLengthDensity_s(1)/1000000,MycorLengthDensity_s(2)/1000000,MycorLengthDensity_s(3)/1000000,&
    MycorLengthDensity_s(4)/1000000,& !MycorLengthDensity_s(5)/1000000,&
    Litterfall_es(1,1),Litterfall_es(1,2),Litterfall_es(1,3),Litterfall_es(1,4),& !,Litterfall_es(1,5)
    Leaf_e(1),Wood_e(1),Root_e(1),Mycor_e(1),&
    Leaf_e(2),Wood_e(2),Root_e(2),&
    Kdrought_b(1), Kseasonal_b(1), KHerbivory_b(1),&
    Kdrought_b(2), Kseasonal_b(2), KHerbivory_b(2),&
    Kdrought_b(3), Kseasonal_b(3), KHerbivory_b(3)

! end of DYNAMIC section

!****************************************************************************
CASE (3) ! TERMINAL ********************************************************* 
 
    call SoilTrECProsum_deallocate
    close(53)! close PROSUM outputs file  

END SELECT ! Program stage

return 
end subroutine PROSUM
