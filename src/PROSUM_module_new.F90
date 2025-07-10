
module PROSUM_module
    implicit none
    type :: prosum_model
    end type prosum_model
    
    double precision, save, allocatable  :: thickness_s(:), &
                                            BottomDepth_s(:), &
                                            ThicknessProp_s(:), & 
                                            RootProp_s(:),&
                                            RootLengthDensity_s(:),&     ! m root per m3 soil
                                            MycorLengthDensity_s(:),&    ! m hypha per m3 soil
                                            TotalRootMycLength_s(:),&    ! total root + hypha in SoilLayer, m per m2 
                                            WaterAccessible_s(:),&       ! water (m) accessible per layer, depends on root length density
                                            WaterPropAccessible_s(:),&   ! water (m) accessible per layer as prop of total accessible over all SoilLayers
                                            WaterAvail_s(:),&
                                            Water_transpired_s(:),&
                                            ExudateCflux_s(:)
                                            
    double precision, save, allocatable  :: InitLeaf_e(:) ,&            ! Mol/m2
                                            InitWood_e(:) ,&            ! Mol/m2
                                            InitRoot_e(:) ,&            ! Mol/m2
                                            InitMycor_e(:) ,&           ! Mol/m2 
                                             !MinCXratio_e(:) ,&          ! Mol C / Mol X
                                             !MaxCXratio_e(:) ,&          ! Mol C / Mol X
                                            DiffusionRadius_e(:),&      ! 
                                            NutAvail_e(:),&             ! Mol/m2/month - total availability for all soil layers
                                            Plant_e(:),&                ! Mol/m2 
                                            Leaf_e(:),&                 ! Mol/m2
                                            Wood_e(:),&                 ! Mol/m2
                                            Root_e(:),&                 ! Mol/m2
                                            Mycor_e(:),&                ! Mol/m2
                                            Exudate_e(:),&              ! Mol/m2
                                            WeightedMaxCXratio_e(:),&   ! MaxC/X for new tissue overall, i.e. weighted by allocation to plant bits
                                            NutRelocatable_e(:),&       ! nutrient available from root+leaf, given max C/nutrient
                                            NutLimitedGrowth_e(:),&     ! Cfix possible given each nutrient limitation
                                            MaxUptake_e(:),&
                                            LeafUptake_e(:),&
                                            WoodUptake_e(:),&
                                            RootUptake_e(:),&
                                            MycorUptake_e(:),&
                                            MaxCXratioSenescLeaf_e(:) 
! ppp changes 1/8/13: MaxCXratioSenescLeaf_e moved from the PROSUM sub into the PROSUM module and made allocatable
                                            
    double precision, save, allocatable  :: NutAvail_es(:,:),&          ! Mol/m2/month in each soil layer
                                            NutUptake_es(:,:),&         ! Mol/m2/month from each soil layer
                                            Litterfall_es(:,:)          ! Mol/m2/month to each soil layer

    double precision, save, allocatable  :: Kdroughtm_b(:)    ,&    ! slope of Kdrought = max(0,mx+c)
                                            Kdroughtc_b(:)    ,&    ! constant in Kdrought = max(0,mx+c)
                                            Kherbivorym_b(:)  ,&    ! slope of Kherbivory = max(0, min(MaxHerbiv, mx))
                                            KherbivoryMax_b(:) ,&   ! MaxHerbiv in Kherbivory = max(0, min(MaxHerbiv, mx))
                                            Kseasonal_b(:)   ,&     ! max litterfall prop according to MOY
                                            KTillageMax_b(:),&      ! props of plant pools falling as litter in tillage months
                                            PrimaryLimitedGrowth_b(:) ! growth as it would be with only primary limitations, allocated to plant bits
    
    double precision, save, allocatable  :: AllocProp_bt(:,:),&        ! allocation of growth to plant bits
                                            KharvestRemoval_bt(:,:),&  ! props of plant pools removed in harvest months
                                            KharvestLitter_bt(:,:)     ! props of plant pools falling as litter in harvest months
                                            
    double precision, save, allocatable  :: MinCXratio_eb(:,:),&    ! minimum C/nutrient (i.e. max conc) per plant bit 
                                            MaxCXratio_eb(:,:),&    ! maximum C/nutrient (i.e. min conc) per plant bit
                                            CurrentCX_eb(:,:)
                                            
    double precision, save, allocatable  :: Kseasonal_bmt(:,:,:)       ! prop litterfall per plant bit per month-of-year per plant type

    double precision, save, allocatable  :: Monthly_pars(:,:)       ! dimension(Num_months_of_parameters,Num_Monthly_pars+1)  
    double precision, save, allocatable  :: Monthly_nutrients(:,:)   ! dimension(Num_months_of_parameters,Num_Monthly_NutrientAvailabilities+1) 

!*********************************************************************************
! Things probably not needed, because the main program will handle I/O differently:
     character(len=120), parameter :: basedir = ""
!    character(len=120), parameter :: basedir = "P:\EU SoilTrEC NEC04307\Data WP3 modelling Ed\PROSUM\PROSUM_v0.3\UK meadow\"
!    character(len=120), parameter :: basedir = "P:\EU SoilTrEC NEC04307\Data WP3 modelling Ed\PROSUM\PROSUM_v0.3\GreekWoodland\"
!    character(len=120), parameter :: basedir = "P:\EU SoilTrEC NEC04307\Data WP3 modelling Ed\PROSUM\Lysina\"
!    character(len=120), parameter :: basedir = "P:\EU SoilTrEC NEC04307\Data WP3 modelling Ed\SoilTrEC1D\Tomatoes\"
!    character(len=120), parameter :: basedir = "C:\Ed\PROSUM\Lysina\"

    character(len=120) :: TempFilePath, SiteName,TreatmentName
    integer Month_end
    integer ThisMonth
    integer, parameter ::   Num_Monthly_pars = 7
    double precision, dimension(Num_Monthly_pars+1) :: TempInputsVector 
    integer, parameter :: Num_Monthly_NutrientAvailabilities = 24
    double precision, dimension(Num_Monthly_NutrientAvailabilities+1) :: TempMonthlyInputsVector 
 
! Expose every PROSUM_sub global in this module so the wrapper can see it:
    integer :: Month_start
    integer :: nlayer, nnutrient, nplantbits, nplanttypes
    integer :: Num_months

    real,    allocatable :: Temp_oC(:)
    real,    allocatable :: PAR_uMpm2s(:)
    double precision, allocatable :: AtmosphCO2_uLpL(:)
    real,    allocatable :: Herbivores_kgLivepha(:)
    real,    allocatable :: Cover(:)
    integer, allocatable :: Tillage_TF(:)
    integer, allocatable :: Harvest_TF(:)
    integer, allocatable :: PlantType(:)
    
contains  
    subroutine SoilTrECProsum_allocate(nlayer,nnutrient,nplantbits,nplanttypes,Num_months_of_parameters)

        integer :: nlayer       ! (suffix = _s): 1=Top SoilLayer,2=second SoilLayer down, ...
        integer :: nnutrient    ! (suffix = _e): 1=C, 2=N, 3=P, 4=Ca, 5=Mg, 6=K
        integer :: nplantbits   ! (suffix = _b): 1=Leaf, 2=Wood, 3=Root, 4=Mycorrhizae, 5=Exudate
        integer :: nplanttypes  ! (suffix = _t): 
                                    ! 1 = Woody deciduous mycorrhizal
                                    ! 2 = Woody deciduous non-mycorrhizal
                                    ! 3 = Woody evergreen mycorrhizal
                                    ! 4 = Woody evergreen non-mycorrhizal
                                    ! 5 = Herbaceous mycorrhizal
                                    ! 6 = Herbaceous non-mycorrhizal
        
        integer :: Num_months_of_parameters

        allocate(Monthly_pars(Num_months_of_parameters,Num_Monthly_pars+1))
        allocate(Monthly_nutrients(Num_months_of_parameters,Num_Monthly_NutrientAvailabilities+1))

        allocate(thickness_s(nlayer))
        allocate(BottomDepth_s(nlayer))
        allocate(ThicknessProp_s(nlayer))
        allocate(RootProp_s(nlayer))
        allocate(RootLengthDensity_s(nlayer))
        allocate(MycorLengthDensity_s(nlayer))
        allocate(TotalRootMycLength_s(nlayer))
        allocate(WaterAccessible_s(nlayer))
        allocate(WaterPropAccessible_s(nlayer))
        allocate(WaterAvail_s(nlayer))
        allocate(Water_transpired_s(nlayer))
        allocate(ExudateCflux_s(nlayer))

        allocate(InitLeaf_e(nnutrient))
        allocate(InitWood_e(nnutrient))
        allocate(InitRoot_e(nnutrient))
        allocate(InitMycor_e(nnutrient))
        allocate(DiffusionRadius_e(nnutrient))
        allocate(NutAvail_e(nnutrient))
        allocate(Plant_e(nnutrient))
        allocate(Leaf_e(nnutrient))
        allocate(Wood_e(nnutrient))
        allocate(Root_e(nnutrient))
        allocate(Mycor_e(nnutrient))
        allocate(Exudate_e(nnutrient))
        allocate(WeightedMaxCXratio_e(nnutrient))
        allocate(NutRelocatable_e(nnutrient))       
        allocate(NutLimitedGrowth_e(nnutrient))       
        allocate(MaxUptake_e(nnutrient))
        allocate(LeafUptake_e(nnutrient))
        allocate(WoodUptake_e(nnutrient))
        allocate(RootUptake_e(nnutrient))
        allocate(MycorUptake_e(nnutrient))
        allocate(MaxCXratioSenescLeaf_e(nnutrient))

        allocate(NutAvail_es(nnutrient, nlayer))
        allocate(NutUptake_es(nnutrient, nlayer))
        allocate(Litterfall_es(nnutrient, nlayer))
        
        allocate(Kdroughtm_b(nplantbits))      
        allocate(Kdroughtc_b(nplantbits))      
        allocate(Kherbivorym_b(nplantbits))    
        allocate(KherbivoryMax_b(nplantbits))  
        allocate(Kseasonal_b(nplantbits))      
        allocate(KTillageMax_b(nplantbits)) 
        allocate(PrimaryLimitedGrowth_b(nplantbits))
        
        allocate(AllocProp_bt(nplantbits,nplanttypes))
        allocate(KharvestRemoval_bt(nplantbits,nplanttypes))
        allocate(KharvestLitter_bt(nplantbits,nplanttypes)) 

        allocate(MinCXratio_eb(nnutrient,nplantbits))
        allocate(MaxCXratio_eb(nnutrient,nplantbits))
        allocate(CurrentCX_eb(nnutrient,nplantbits))

        allocate(Kseasonal_bmt(nplantbits,12,nplanttypes))    
        
    end subroutine SoilTrECProsum_allocate
        
    subroutine FillArrays(Num_months_of_parameters,StandAlone)
        integer :: Num_months_of_parameters,StandAlone
        
        ! Read time series data from file
	    TempFilePath=trim(basedir)//"PROSUM_time_series_inputs.csv"
        OPEN(10,FILE=TempFilePath)
            READ(10,*) ! skip header line
            do ThisMonth=1,Num_months_of_parameters,1
                READ(10,*) SiteName,TreatmentName, TempInputsVector
                Monthly_pars(ThisMonth,:) = TempInputsVector
            end do
        CLOSE(10)

        ! PROSUM_time_series_soil_availabilities.csv is only needed for the stand-alone PROSUM not the 1DICZ version
        ! Currently assumes there are four soil layers. 
        if (StandAlone == 1) then
	            TempFilePath=trim(basedir)//"PROSUM_time_series_soil_availabilities.csv"
                OPEN(10,FILE=TempFilePath)
                    READ(10,*) ! skip header line
                    do ThisMonth=1,Num_months_of_parameters,1
                        READ(10,*) SiteName,TreatmentName, TempMonthlyInputsVector
                        Monthly_nutrients(ThisMonth,:) = TempMonthlyInputsVector
                    end do
                CLOSE(10)
        end if ! (StandAlone == 1)

    end subroutine FillArrays


    subroutine SoilTrECProsum_deallocate
        deallocate(Monthly_pars)
        deallocate(Monthly_nutrients)
    
        deallocate(thickness_s)
        deallocate(BottomDepth_s)
        deallocate(ThicknessProp_s)
        deallocate(RootProp_s)
        deallocate(RootLengthDensity_s)
        deallocate(MycorLengthDensity_s)
        deallocate(TotalRootMycLength_s)
        deallocate(WaterAccessible_s)
        deallocate(WaterPropAccessible_s)
        deallocate(WaterAvail_s)
        deallocate(Water_transpired_s)
        deallocate(ExudateCflux_s)

        deallocate(InitLeaf_e)
        deallocate(InitWood_e)
        deallocate(InitRoot_e)
        deallocate(InitMycor_e)
        deallocate(DiffusionRadius_e)
        deallocate(NutAvail_e)
        deallocate(Plant_e)
        deallocate(Leaf_e)
        deallocate(Wood_e)
        deallocate(Root_e)
        deallocate(Mycor_e)
        deallocate(Exudate_e)
        deallocate(WeightedMaxCXratio_e)
        deallocate(NutRelocatable_e)       
        deallocate(NutLimitedGrowth_e)       
        deallocate(MaxUptake_e)
        deallocate(LeafUptake_e)
        deallocate(WoodUptake_e)
        deallocate(RootUptake_e)
        deallocate(MycorUptake_e)
        deallocate(MaxCXratioSenescLeaf_e)

        deallocate(NutAvail_es)
        deallocate(NutUptake_es)
        deallocate(Litterfall_es)

        deallocate(Kdroughtm_b)      
        deallocate(Kdroughtc_b)      
        deallocate(Kherbivorym_b)    
        deallocate(KherbivoryMax_b)  
        deallocate(Kseasonal_b)      
        deallocate(KTillageMax_b) 
        deallocate(PrimaryLimitedGrowth_b)

        deallocate(AllocProp_bt)
        deallocate(KharvestRemoval_bt)
        deallocate(KharvestLitter_bt) 

        deallocate(MinCXratio_eb)
        deallocate(MaxCXratio_eb)
        deallocate(CurrentCX_eb)
        deallocate(Kseasonal_bmt)    

    end subroutine SoilTrECProsum_deallocate

!*********************************************************************************

end module PROSUM_module
