      Program CASM_XXX
c
c     Version 1.00
c     This version combines the CASM stream, pond, and emergent wetland models into a single source code
c     Author: Steven M. Bartell
c             August 2020

c     The Comprehensive Aquatic Simulation Model (CASM) simulates the daily 
c     production dynamics of populations inhabiting a water column, 
c     a littoral zone or benthic zone.
c
c     The CASM also describes the dynamics of dissolved organic matter,
c     particulate organic matter, settled detritus and 
c     concentrations of nitrogen, phosphorus, silica and dissolved oxygen.
c
c     Author: S.M. Bartell
c             May 2008
c
	  implicit real*4(a-h,o-z)
      character*80 ruler,biofil,webfil,envfil,ms_ref,ms_eff
	  character*80 outf,outtox,outbio,outtef,outhst,outdrv
	  character*120 expfil,toxfil
	  character*16 chemnm
        common/dbg/idebug
        common/lblout/outf(28),expfil,toxfil,outtef
        common/csm1/nsys
        common/csm2/ncycle,   !value of 1 includes internal cycling,  0 turns off cycling
     .              limit_ps  !value of 0 uses simple multiplication, 1 uses geometric mean
        common/csm3/delta_t   !time step, units = days
c
        common/res1/isyndrome,islope
        common/TEF1/zexp   !for 365-d constant exposure = zexp (ppb)
        common/tef_q3/knum !knum=1, output producer TEFs only
c                          !knum=2, output producer and consumer TEFs
c
c
      common/toxpar/itoxmdl,chemnm
c
      Write(*,*)'Comprehensive Aquatic System Model (CASM)'
      Write(*,*)'Pesticide Risk Assessment         '
      Write(*,*)'Syngenta Crop Protection, LLC     '
      Write(*,*)' '
c
23    format(a80)
c
      open(3,file=
     .'c:\casm_cmb\cmb_cntrl\casmxxx_control.dat', 
     .    status='unknown')
c
        read(3,23)ruler
        read(3,23)ruler
        read(3,'(i1)')nsys
        if(nsys.eq.1)write(*,*)'Generic Midwestern Lower-order Stream'
        if(nsys.eq.2)write(*,*)'Generic Midwestern Farm Pond         '
        if(nsys.eq.3)write(*,*)'Generic Midwestern Emergent Wetland  '
c
      if(nsys.gt.3)then
       write(*,*)'Unrecognized aquatic system selection'
       write(*,*)'CASM-CMB is terminating'
       goto 399
      endif !nsys error trap
c 
      write(*,*)' '
      write(*,*)'Main input files:'
c     Read filename for bioenergetics and habitat quality parameter values
         read(3,23) ruler
         read( 3,'(a80)')biofil
         write(*,'(a80)')biofil
         open( 5, file=biofil, status='unknown')
c     Read filename for diet matrix
         read(3,23) ruler
         read( 3,'(a80)')webfil
         write(*,'(a80)')webfil
         open(99, file=webfil, status='unknown')
c     Read filename for environmental input data
         read(3,23)ruler
         read( 3,'(a80)')envfil
         write(*,'(a80)')envfil
         open(7,file=envfil, status='old')
c
c     CASM time step
        read(3,23)ruler
        read(3,'(f5.3)')delta_t
        write(*,*)'Time step (d): ',delta_t
c
        read(3,23)ruler
        read(3,'(i1)')idebug
c       
c     Select internal nutrient cycling or no cycling
         read(3,23) ruler
         read(3,'(i1)') ncycle
c
c     Select mode of photosynthesis limitation
         read(3,23) ruler
         read(3,'(i1)') limit_ps
c
c     Read filename for exposure scenario
         read(3,23) ruler
         read(3,'(a120)')expfil
         open(17, file=expfil, status='unknown')
c
c     Define constant exposure conc
         read(3,23) ruler
         read(3,'(f4.0)') zexp
c
c     Added SKN June 2015
c     Select Toxicity Estimation Approach 0: TEF Model used in Bartell etal. (2014) and Nair etal. (2015)
c     1: Based on AQUATOX Model
         read(3,23) ruler
         read(3,23) ruler
         read(3,'(i1)')itoxmdl
c     End SKN mod
c     Select type of exposure-response function, used only for the TEF approach, but information read anyway
         read(3,23) ruler
         read(3,23) ruler
         read(3,'(i1)') islope
c     Added SKN July 2015
c     Enter name of chemical modeled in 16 characters
         read(3,23) ruler
         read(3,'(a16)') chemnm
         
c     End SKN mod
c     Read filename for toxicity scenario: toxfile input modified to include AQUATOX approach
         read(3,23) ruler
         read(3,'(a120)')toxfil
         open(27, file=toxfil, status='unknown')
c
c     Select GSS or PSS
         read(3,23) ruler
	     read(3,'(i1)') isyndrome
c
c     Select TEF QA output
         read(3,23) ruler
         read(3,'(i1)') knum
c
        read(3,23) ruler
c     Filenames for output files
        read(3,'(a80)')outtox !casm_tox output file
        read(3,'(a80)')outbio !casm_bio output file
        read(3,'(a80)')outtef !casm_tef output file
        read(3,'(a80)')outhst !casm_hst output file
        read(3,'(a80)')outdrv !casm_hst output file
c
c     Enviro-tox output from casm simulations
	  open( 8,file=outtox, status ='unknown')
c     Biomass vs time output from casm simulation
	  open(12,file=outbio, status ='unknown')
c     TEF QA vs time output from casm_oh simulation
	  open(14,file=outtef, status ='unknown')
c     De-bugging output from casm_krnl simulation
	  open(186,file=outhst,status ='unknown')
c     Derivative output from casm_krnl simulation
	  open(177,file=outdrv,status ='unknown')
c
      read(3,23)ruler	
      read(3,'(a80)')ms_ref !CASM-CMB biomass reference output file
      read(3,'(a80)')ms_eff !CASM-CMB biomass toxic effects output file
c
       open(990, file=ms_ref, status='unknown') 
       open(992, file=ms_eff, status='unknown')
c
      Write(12,*)'Comprehensive Aquatic Systems Model'
      Write(12,*)'Syngenta Atrazine LOC Project'
c
        if(nsys.eq.1)write(12,*)'Generic Midwestern stream   '
        if(nsys.eq.2)write(12,*)'Generic Midwestern farm pond'
        if(nsys.eq.3)write(12,*)'Generic Midwestern wetland  '
c
      write(14,2)
2     format('CASM-Atrazine: Quality Aassurance of TEF Calculation',
     .       ' and Implementation')
c
      write(*,*)' '
c
c     output to TEF QA file: simulation summary
      if(zexp.lt.0.)write(14,914)expfil
914   format('Exposure file: ',a120)
      if(zexp.ge.0.)write(14,912) zexp
912   format('Constant 365-d exposure (ppb): ',f6.2)
c
      write(14,916)toxfil
916   format('Toxicity file: ',a120)
c
      if(islope.eq.0)write(14,918)
918   format('Exposure:response: Probit EC50-based slopes,intercepts')
      if(islope.eq.1)write(14,920)
920   format('Exposure:response: Probit E-R function, EPA slopes')
      if(islope.eq.2)write(14,921)
921   format('Exposure:response: Triangular E-R function and slopes')
      if(islope.eq.3)write(14,923)
923   format('Exposure:response: Piecewise linear E-R function')
c
      if(isyndrome.eq.0)write(14,922)
922   format('Using General Stress Syndrome')
      if(isyndrome.eq.1)write(14,924)
924   format('Using Photosynthetic Stress Syndrome')
c
      if(knum .eq.1)write(14,926)
926   format('TEF QA output for producers only')
      if(knum .eq.2)write(14,928)
928   format('TEF QA output for producers and consumers')
c
      write(14,*)' '
c
940   continue
c
      if(zexp.lt.0.)write(*,914)expfil
      if(zexp.ge.0.)write(*,912) zexp
      write(*,916)toxfil
      if(islope.eq.0)write(*,918)
      if(islope.eq.1)write(*,920)
      if(islope.eq.2)write(*,921)
      if(islope.eq.3)write(*,923)
c
      if(isyndrome.eq.0)write(*,922)
      if(isyndrome.eq.1)write(*,924)
c
c
      if(knum.eq.1)
     .   print*,'TEF QA output for producers only'
      if(knum.eq.2)
     .   print*,'TEF QA output for producers and consumers'
      write(*,*)' '
c
c     Input parameter values
      call input
      print*,'Subroutine INPUT completed...' 
c
      dt=delta_t !define time step for model integration
c 
c     advective transport for stream and wetland applications, but not for farm pond
      if(nsys.ne.2)call advect(dt) !subroutine to calculate daily advective losses for affected state variables
c
c     Save input initial biomass and baseline parameter values 
      call sav_b !initial biomass valuess
      call sav_p !baseline parameter values
c
	  if(nsys.eq.1)print*,'CASM(0) initiated for Midwestern stream...'
	  if(nsys.eq.2)print*,'CASM(0) initiated for Midwestern farm pond'
	  if(nsys.eq.3)print*,'CASM(0) initiated for Midwestern wetland..'
	  call casm(0) !performs baseline or reference simulation
	  print*,
     . 'CASM(0) reference simulation completed.'
c
c
      if(itoxmdl.eq.0) then ! Toxicity assay simulation is only done for TEF-based assessment
c     Perform toxicity assay simulations
        print*,'Scenario toxicity assays initiated...'
        if(itoxmdl.eq.0) call daytox
        print*,'Scenario toxicity assays completed...'
      endif !TEF toxicity assigned
c 
	  call rstor_b !restores initial biomass values prior to treatment simulation
      call rstor_p !restores baseline parameter values prior to treatment simulation
c
	  call casm(1) !performs treatment simulation                
c
	  write(*,199)
199   format(
     . 'CASM(1) exposure simulation completed.')
c
c     Calculate community similarity index values
      call simdex_rev
      print*,'Completed similarity coefficient calculations...'
c
      call TEF_QA
      print*,'TEF QA output was generated...'
c
20    continue  
      print*,'CASM Normal exit.'

      open(123,file='casm_ended.dat',status='unknown')
	  write(123,*)'Ended'
c
399   stop
	  end
c                  
c
	  Block Data
      implicit real*4(a-h,o-z)
      character*14 carnam(4),guild_nam(3),
     .             envnam(13)
      character*16 gldnam_st, gldnam_pw
      common/blknam/gldnam_st(12),gldnam_pw(12)
      common/ibd1/carnam,guild_nam
      common/ibd2/envnam 
      data envnam/
     .'DINe          ',
     .'DIPe          ',
     .'DSie          ',
     .'DO-epi        ',
     .'DO-hyp        ',
     .'WaterColDOC   ',
     .'WaterColPOC   ',
     .'SedimentDOC   ',
     .'SedimentPOC   ',
     .'TIS           ',
     .'DINh          ',
     .'DIPh          ',
     .'DSih          '/
c        
      data carnam/
     .'WaterColDOC   ',
     .'WaterColPOC   ',
     .'SedimentDOC   ',
     .'SedimentPOC   '/
c
      data guild_nam/
     .'TotalPhytopl  ',
     .'TotalPeriphyt ',
     .'TotalZooplnkt '/
c 
	 data gldnam_st/           !for CASM stream application
c            Guild names 
     .      'Periphyton 1    ',
     .      'Periphyton 2    ',
     .      'Macrophytes     ',
     .      'Emergent plants ',
     .      'Zooplankton     ',
     .      'Pel omniv fish  ',
     .      'Pel pisc fish   ',
     .      'Pel bacteria    ',
     .      'Benth inverts   ',
     .      'Ben omniv fish  ',
     .      'Ben pisc fish   ',
     .      'Sed bacteria    '/
c
	  data gldnam_pw/           !for CASM pond and wetland applications
c            Guild names 
     .      'Phytoplankton   ',
     .      'Periphyton      ',
     .      'Macrophytes     ',
     .      'Emergent plants ',
     .      'Zooplankton     ',
     .      'Pel omniv fish  ',
     .      'Pel pisc fish   ',
     .      'Pel bacteria    ',
     .      'Benth inverts   ',
     .      'Ben omniv fish  ',
     .      'Ben pisc fish   ',
     .      'Sed bacteria    '/
c 
      end
cc
c     Input program control variables, model growth parameters, food web interactions,
c     environmental data, and toxicity data
c     Begin sub input
      Subroutine input
      implicit real*4(a-h,o-z)
      character rem*1, comment*40
      character*25 envdat,chmlbl
      character*80 infile
      character*80 ruler
      character*120 ruler1
      character*215 header 
      character*265 header2
      character*110 header3
      character*14 popnam,name1
      character*31 kname(80)
      character*120 expfil,toxfil,teffil
      character chmnm*16, chemnm*16
c
      common/res1/isyndrome,islope
      common/res2/slope(80),xcept(80)
      common/res3/nplants
      common/csm1/nsys
      common/csm4/npops     
c
      common/t/nsp(4),nsc(8)
      common/name/popnam(80)
c
c     initial biomass values and bioenergetics parameters
      common/p/  bp(4,10),   !initial biomass
     .           ps(4,10),   !max photosyhnethetic rate
     .         pte1(4,10),pxk1(4,10),   !Thornton-Lessem temperature parameters   
     .         pte2(4,10),pxk2(4,10),   
     .         pte3(4,10),pxk3(4,10),   
     .         pte4(4,10),pxk4(4,10),   
     .           si(4,10),  !Light saturation intensity 
     .          snk(4,10),  !Sinking rate
     .          xkp(4,10),xkn(4,10),xks(4,10), !Michaelis-Menten constants for P, N, and Si 
     .          pm(4,10),   !Mortality rate
     .        resp(4,10),   !Photorespiration rate
     .        resd(4,10),   !Dark respiration rate
c
c             Habitat factors for salinity, current velocity, and depth
     .        psplt(4,10),pspol(4,10),pspou(4,10),psput(4,10),  !salinity
     .        ppvlt(4,10),ppvol(4,10),ppvou(4,10),ppvut(4,10),  !velocity
     .        ppdlt(4,10),ppdol(4,10),ppdou(4,10),ppdut(4,10),  !depth
c
c             Phenology         
     .         ddi(4,10),   !Degree-days required for growth initiation
     .         dds(4,10),   !Degree-days for onset of senescence
     .              xlk
c
      common/c/  bc(8,5),  !Initial biomass 
     .          cmx(8,5),  !max consumption rate
     .         cte1(8,5),cxk1(8,5),   !Thornton-Lessem temperature parameters   
     .         cte2(8,5),cxk2(8,5),   
     .         cte3(8,5),cxk3(8,5),   
     .         cte4(8,5),cxk4(8,5),   
     .         rsda(8,5),    rs(8,5),           !specific dynamic action rate, respiration rate
     .          tro(8,5),   trm(8,5),           !temperature dependence of respiration
     .            f(8,5),     u(8,5),  cm(8,5), !egestion, excretion, mortality rates
     .          dft(8,5),                       !invertebrate drift, 1/d 
c
c             Habitat factors for dissolved oxygen, salinity, current velocity, and depth
     .         DOlt(8,5), DOol(8,5), DOou(8,5), DOut(8,5),  !dissolved oxygen
     .        psclt(8,5),pscol(8,5),pscou(8,5),pscut(8,5),  !salinity
     .        ccvlt(8,5),ccvol(8,5),ccvou(8,5),ccvut(8,5),  !velocity
     .        ccdlt(8,5),ccdol(8,5),ccdou(8,5),ccdut(8,5),  !depth
     .             bxkn,      bxkp
c
      common/cl/w(84,40),a(84,40),he(84,40)
c
c
c     Variables necessary for daily effects factors
      common/micfac/ fact(80), cmort(80)
      common/blkmic/xiday(80),enpt(80),effc(80),pclc50(80),idetr(80),
     .         fracLip(80),elemr(80),wetwt(80),q10(80),rb(80),uptrt(80),
     .              xnoec(80),envc(365),kappa
      common/blkmict/slpfct,shpfct,alogkow,iab,ihydr,ik2,pKa,pH,wtod,
     .          frcdetr,lethal,ibarb
      common/toxpar/itoxmdl,chemnm
c     End - variables for daily effects factors
c
      common/env/ein(365),te(365),th(365),xpe(365),xne(365),xse(365),
     .           depth(365),cvel(365),tis(365),POCe(365),wind(365),
     .           salinity(365)      
      common/env2/degdays(365)
      common/p_init/  fpp,fpn,fps,fpdo
      common/c_init/  fcp,fcn,fcs,fcdo
      common/adv/rlngth,rwdth,rslope
c
      common/decomp/rcle,tcle_o,tcle_m, !decomposition labile POC epil      
     .              rcre,tcre_o,tcre_m, !              refractory POC epil
     .              rclh,tclh_o,tclh_m, !              labile POC hypol     
     .              rcrh,tcrh_o,tcrh_m, !              refractory POC hypol
     .              stl                 !POC settling rate (1/d)
c
      common/ext/exH2O,exDOC,exPOC,exTIS !light extinction parameters
      common/ext2/fd1,fd2,fd3,fd4
c
c     iedd: initial exposure day; led: last exposure day
      common/trk/ied,led
      common/sim/idfexp
c
      common/TEF1/zexp
c
      print*,'Subroutine INPUT initiated...'
      read(5,12)ruler
      write(12,12)ruler
      read(5,12)ruler
      write(12,12)ruler
12    format(a80)
c
      np=4 !number of producer guilds
c
      read(5,*) (nsp(i), i=1,np)
99    format(8i6)
      write(12,99)(nsp(i), i=1,np)

c     Reading the number of consumer categories
c     followed by the number of consumers in each category
      read(5,12)ruler
      write(12,12)ruler
      read(5,12)ruler
      write(12,12)ruler
c
      nc=8 !number of consumer guilds
c
      read(5,99) (nsc(i), i=1,nc)
      write(12,99) nc,(nsc(i), i=1,nc)
c
c     calculate number of modeled populations
      npops=nsp(1)+nsp(2)+nsp(3)+nsp(4)+
     .      nsc(1)+nsc(2)+nsc(3)+nsc(4)+
     .      nsc(5)+nsc(6)+nsc(7)+nsc(8)
c
c     Primary producers: phytoplankton, periphyton, macrophytes, emergents 
      write(12,*)' '
      write(12,*)'Bioenergetics Growth Parameters for the CASM:'
      write(12,*)' '
      write(12,68)
68    format('Producer Population Parameters:'/)
c
      do m=1,4
      read(5,13)header2
      write(12,13)header2
13    format(a265)
      enddo
c
c
      ipop=0
      do j=1,np
      read(5,12)ruler
      write(12,12)ruler
      nn=nsp(j)
         do i=1,nn
         ipop=ipop+1
         read(5,51) popnam(ipop),! species name
     .           bp(j,i),   !initial biomass
     .           ps(j,i),   !max photosyhnethetic rate
     .         pte1(j,i),pxk1(j,i),   !Thornton-Lessem temperature parameters   
     .         pte2(j,i),pxk2(j,i),   
     .         pte3(j,i),pxk3(j,i),   
     .         pte4(j,i),pxk4(j,i),   
     .           si(j,i),  !Light saturation intensity 
     .          snk(j,i),  !Sinking rate
     .          xkp(j,i),xkn(j,i),xks(j,i), !Michaelis-Menten constants for P, N, and Si 
     .          pm(j,i),   !Mortality rate
     .        resp(j,i),   !Photorespiration rate
     .        resd(j,i),   !Dark respiration rate
c
c             Habitat factors for salinity, current velocity, and depth
     .        psplt(j,i),pspol(j,i),pspou(j,i),psput(j,i),  !salinity
     .        ppvlt(j,i),ppvol(j,i),ppvou(j,i),ppvut(j,i),  !velocity
     .        ppdlt(j,i),ppdol(j,i),ppdou(j,i),ppdut(j,i),  !depth
c
c             Phenology parameters
     .        ddi(j,i),dds(j,i)
c
51         format(a14,2x,e8.2,f6.2,3x,4(1x,f3.0,1x,f4.2),f6.0,7x,f5.3,
     .         3x,3f7.3,f8.3,f9.3,4x,f8.3,3x,4(4x,f3.0),1x,4(2x,f4.1),
     .         5x,4(1x,f5.2),2(5x,f5.0))
c
           write(12,51)popnam(ipop),
     .           bp(j,i),   !initial biomass
     .           ps(j,i),   !max photosyhnethetic rate
     .         pte1(j,i),pxk1(j,i),   !Thornton-Lessem temperature parameters   
     .         pte2(j,i),pxk2(j,i),   
     .         pte3(j,i),pxk3(j,i),   
     .         pte4(j,i),pxk4(j,i),   
     .           si(j,i),  !Light saturation intensity 
     .          snk(j,i),  !Sinking rate
     .          xkp(j,i),xkn(j,i),xks(j,i), !Michaelis-Menten constants for P, N, and Si 
     .          pm(j,i),   !Mortality rate
     .        resp(j,i),   !Photorespiration rate
     .        resd(j,i),   !Dark respiration rate
c
c             Habitat factors for salinity, current velocity, and depth
     .        psplt(j,i),pspol(j,i),pspou(j,i),psput(j,i),  !salinity
     .        ppvlt(j,i),ppvol(j,i),ppvou(j,i),ppvut(j,i),  !velocity
     .        ppdlt(j,i),ppdol(j,i),ppdou(j,i),ppdut(j,i),  !depth
c
     .        ddi(j,i), dds(j,i) !phenology: degree days
c
         enddo
      enddo
c
      read(5,'(a14,2x,f4.2)') name1, xlk
      write(12,'(a14,2x,f4.2)') name1, xlk
c
      write(12,78)
78    format(/'Consumer Population Parameters:'/)
c
      do m=1,5
      read(5,17) header2
      if(m.ne.1)write(12,17) header2
17    format(a265)
      enddo
c
c     Consumers: zooplankton, planktivores, bacterioplankton,
c                benthic invertebrates, fish, sed. bacteria
c
      do j=1,nc
      read(5,12) ruler
      write(12,12)ruler
      nn=nsc(j)
         do i=1,nn
         ipop=ipop+1
         read(5,52) popnam(ipop),! species name
     .                bc(j,i),  ! initial biomass,             gC/m3
     .               cmx(j,i),  ! max feeding rate,        gC/gC/day
     .         cte1(j,i),cxk1(j,i),   !Thornton-Lessem temperature parameters   
     .         cte2(j,i),cxk2(j,i),   
     .         cte3(j,i),cxk3(j,i),   
     .         cte4(j,i),cxk4(j,i),   
     .         rsda(j,i),    rs(j,i),           !specific dynamic action rate, respiration rate
     .          tro(j,i),   trm(j,i),           !temperature dependence of respiration
     .            f(j,i),     u(j,i),  cm(j,i), !egestion, excretion, mortality rates
c
c             Habitat factors for dissolved oxygen, salinity, current velocity, and depth
     .         DOlt(j,i), DOol(j,i), DOou(j,i), DOut(j,i),  !dissolved oxygen
     .        psclt(j,i),pscol(j,i),pscou(j,i),pscut(j,i),  !salinity
     .        ccvlt(j,i),ccvol(j,i),ccvou(j,i),ccvut(j,i),  !velocity
     .        ccdlt(j,i),ccdol(j,i),ccdou(j,i),ccdut(j,i),  !depth
c
     .        dft(j,i) !invertebrate drift (1/d)
c
52    format(a14,2x,e8.2,f6.2,3x,4(1x,f3.0,1x,f4.2),f7.3,4x,f7.5,5x,
     .       2(2x,f3.0),8x,f5.3,5x,f5.3,6x,f7.5,2x,4(2x,f4.1),1x,
     .       4(3x,f4.1),2x,4(2x,f4.1),2x,4(2x,f4.1),4x,f4.2)  
c
           write(12,52)popnam(ipop),bc(j,i),cmx(j,i),
     .         cte1(j,i),cxk1(j,i),   !Thornton-Lessem temperature parameters   
     .         cte2(j,i),cxk2(j,i),   
     .         cte3(j,i),cxk3(j,i),   
     .         cte4(j,i),cxk4(j,i),   
     .         rsda(j,i),    rs(j,i),           !specific dynamic action rate, respiration rate
     .          tro(j,i),   trm(j,i),           !temperature dependence of respiration
     .            f(j,i),     u(j,i),  cm(j,i), !egestion, excretion, mortality rates
c
c             Habitat factors for dissolved oxygen, salinity, current velocity, and depth
     .         DOlt(j,i), DOol(j,i), DOou(j,i), DOut(j,i),  !dissolved oxygen
     .        psclt(j,i),pscol(j,i),pscou(j,i),pscut(j,i),  !salinity
     .        ccvlt(j,i),ccvol(j,i),ccvou(j,i),ccvut(j,i),  !velocity
     .        ccdlt(j,i),ccdol(j,i),ccdou(j,i),ccdut(j,i),  !depth
c
     .        dft(j,i) !invertebrate drift
c
         enddo
      enddo
c
      read(5,12) ruler
c     reading in the half saturation value for N and P (mg/L)for bacterioplankton
      read(5,54) name1,bxkn,bxkp
      write(12,54)name1,bxkn,bxkp
54    format(a14,1x,f5.0,1x,f5.0)
c
c     input initial conditions for:
c       fpp = fraction of plant carbon that is P
c       fpn = fraction of plant carbon that is N
c       fps = fraction of plant carbon that is Si
c       fpdo= plant carbon equivalent in diss. oxygen
c       fcp = fraction of consumer carbon that is P
c       fcn = fraction of consumer carbon that is N
c       fcs = fraction of consumer carbon that is Si
c       fcdo= consumer carbon equivalent in diss. oxygen
c
c
      read(5,12)   ruler
      write(12,12) ruler
      read(5,12)   ruler
      write(12,12) ruler
      read(5,911)  fpp,fpn,fps,fpdo
      write(12,911)fpp,fpn,fps,fpdo
c
      read(5,12)   ruler
      write(12,12) ruler
      read(5,12)   ruler
      write(12,12) ruler
      read(5,911)  fcp,fcn,fcs,fcdo
      write(12,911)fcp,fcn,fcs,fcdo
c
911   format(5(f10.3,1x))
c      
      write(12,*)' '
      write(12,*)' '
c
c     stream segment physical dimensions
      read(5,12)   ruler
      write(12,12) ruler
      read(5,12)   ruler
      write(12,12) ruler
      read(5,917)  rlngth,rwdth,rslope
      write(12,917)rlngth,rwdth,rslope
917   format(3(f10.0,1x))
c
      read(5,12)   ruler
      write(12,12) ruler
      read(5,12)   ruler
      write(12,12) ruler
      read(5,915)  rcle,tcle_o,tcle_m  !decomposition labile POC epil 
      read(5,915)  rcre,tcre_o,tcre_m  !              refractory POC epil
      read(5,915)  rclh,tclh_o,tclh_m  !              labile POC hypol     
      read(5,915)  rcrh,tcrh_o,tcrh_m  !              refractory POC hypol
c
      write(12,915)rcle,tcle_o,tcle_m
      write(12,915)rcre,tcre_o,tcre_m  
      write(12,915)rclh,tclh_o,tclh_m       
      write(12,915)rcrh,tcrh_o,tcrh_m  
915   format(6x,f4.2,2(2x,f4.1))
c
c     POC settling rate (1/d)
      read(5,12)   ruler
      write(12,12) ruler
      read(5,12)   ruler
      write(12,12) ruler
      read(5,915)  stl
      write(12,915)stl
c
c     light extinction parameters
      read(5,12)   ruler
      write(12,12) ruler
      read(5,12)   ruler
      write(12,12) ruler
      read(5,12)   ruler
      write(12,12) ruler
      read(5,919)  exH2O,exDOC,exPOC,exTIS
      write(12,919)exH2O,exDOC,exPOC,exTIS
919   format(4(f5.3,1x))
c 
c     depths for light attenuation
      read(5,12)   ruler
      write(12,12) ruler
      read(5,12)   ruler
      write(12,12) ruler
      read(5,12)   ruler
      write(12,12) ruler
      read(5,919)  fd1,fd2,fd3,fd4
      write(12,919)fd1,fd2,fd3,fd4
c
c 
c     The following code defines the foodweb connections
c     throughout the model and can be used to modify
c     foodweb relations for site specific applications
c     of the model. Values range between 0. and 1.0 for
c     both w(,) and a(,).
c          w(i,j) --- preference of prey i by predator j
c          a(i,j) --- assimlation of ingested prey i
c                     by predator j
c          he(i,j)--- handling efficiency of predator j to prey i 
c
c
      do i=1,84              ! initialize
         do j=1,40
           w(i,j)=0.
           a(i,j)=0.
          he(i,j)=1.0
         enddo
      enddo
      write(12,*)'Food web file...'
c
2     read(99,9997, END=3) rem
      if(rem.ne.';') then
       backspace (99)
       read(99,9999,END=3,ERR=4) k,l,w(k,l),a(k,l),he(k,l),comment
       write(12,9999)            k,l,w(k,l),a(k,l),he(k,l),comment
9999  format(2i3,3(f10.2),a40)
9997  format(a1)
        if (k.gt.84.or.l.gt.40) then
          write(*,*) 'Error in the food web connection file'
          write(*,*) 'Indices are too large'
        elseif (k.eq.0.or.l.eq.0) then
          go to 2  
        endif  
      endif 
      go to 2
3     continue
      go to 5
4     write(*,*) 'Error in the foodweb connection input file'
5     continue
c
c     Inout environmental data
c
      read(7,19)header3
19    format(a110)
      write(12,*)' '
      write(12,19)header3
c
c
      do m=1,6
      read(7,19)  header3
      write(12,19)header3
      enddo
c
      do i=1,365
	  read(7,'(i8,12f8.3)')
     .           id,te(i),th(i),ein(i),xne(i),xpe(i),xse(i),depth(i),
     .           cvel(i),tis(i),POCe(i),wind(i),salinity(i) 
c
	  write(12,'(3x,i3,12f8.3)')
     .            i,te(i),th(i),ein(i),xne(i),xpe(i),xse(i),depth(i),
     .            cvel(i),tis(i),POCe(i),wind(i),salinity(i) 
c
        enddo                                                 
        close(7)
c
c     calculate accumulated degree-days
      degdays(1)=te(1)
      do i=2,365
      degdays(i)=degdays(i-1)+te(i)
      enddo
c
c     calculate mean and stdev of current velocity for periphyton sloughing
      call cvstat  !cvstat places mean and stdev in common block slgh for sloughing calculations
c
c     Input contaminant exposure concentrations and toxicity data
      read(17,'(a120)') expfil
c
	  read(17,12)ruler
	  read(17,12)ruler
	  read(17,12)ruler
c
      read(17,'(18x,i4)' ) ied
      read(17,'(18x,i4)' ) led
      write(8,375) ied
375   format(1x,'Init exposure day: ',i4)
      write(8,377) led
377   format(1x,'Last exposure day: ',i4)
c
c     
      if(zexp.lt.0)write(8,'(a120)') expfil
      if(zexp.gt.0)write(8,913)zexp
913   format(1x,'Constant daily exposure:  ',f7.2,
     .       ' (ug/L) overrides exposure file.')
c
c
      read(17,12) ruler
      write(8,379)
379   format(' Day   Pesticide (ug/L)')
      do i=1,365
        read(17,'(5x,i3,f11.8)') ialpha, envc(i)
c   
c       constant exposure experiments
        if(zexp.ge.0.)envc(i)=zexp
c   
        write(8,'(i4,f11.8)') ialpha, envc(i)
      enddo  
c
c     find DOY of first non-zero atrazine concentration
      ichk=0
      do i=1,365
	  if(ichk.eq.1)goto 777
	  if(envc(i).lt.0.1)goto 777 !Shyam changed the day of first exposure value to 0.1 from 0
	    idfexp=i
		ichk=1
777   continue
      enddo
c 
c
c     input plant toxicity data
      nplants=nsp(1)+nsp(2)+nsp(3)+nsp(4)
c
c     Input toxicity data
c           Note: toxicity data file formats are different for CASM generic slopes, intercepts
c                 and the other three E-R functions (EPA probits, triangular, piecewise linear)
c     SKN Added Aug 2015
      if(itoxmdl.eq.0) then
c     TEF-based earlier CASM approach implemented here
c     END SKN
      if(islope.eq.0)then
c
c     input original CASM toxicity data format
      read(27,12) ruler
c
      read(27,'(a25)')chmlbl
      read(27,12) ruler
c
      write(8,*)' '
      write(8,'(a25)')chmlbl
      write(8,*)'    Pop#     Enpt         Days       Effc        NOEC' 
c
c     NOTE: enpt() does is not used in any calculations because
c           the probit function assumes that the endpoint is 50%
c           This is a vestige of previous versions that used a linear
c           exposure-response function. (S.Bartell 4 Apr 01)
c	  	        
      do i=1,npops
	  read(27,'(15x,f8.4,2x,f8.4,2x,f10.0,1x,f8.0)')
     .         enpt(i),xiday(i),effc(i),xnoec(i)
        write(8,'(5x,i3,4(3x,f10.0))')i, enpt(i),xiday(i),effc(i),
     .                                xnoec(i)  
        enpt(i)=enpt(i)/100. !convert percentage to fractional impact
      enddo 
c	  
c 
c     NOTE: EC50-derived slopes and intercepts replace previous assumption of zero effects at zero exposure
c           This requires use of log10(exposure conc) in calculation of toxic effects factors
c           S.M. Bartell July 2007
c     calculate EC50-derived slopes and intercepts
      do i=1,npops
c      slopes
c      delta-y probits: (5.0000-0.0001=4.9999) uses Bliss 1934 assignment of 0.0001 probits to NOECs
c      delta-x exposure concentration in log10 units
       slope(i)=0.
       xcept(i)=0.
c
       if(xnoec(i).eq.0.) xnoec(i)=1.0 !avoids taking log10 of zero
       if(xnoec(i).lt.effc(i))  !usual case
     .	     slope(i)=4.9999/(alog10(effc(i))-alog10(xnoec(i)))
       if(xnoec(i).ge.effc(i))  !infeasible, define delta-x as log10 of EC50 or LC50
     .         slope(i)=4.9999/alog10(effc(i))
c      EC50-derived intercepts
c      use point (LC50,5.000) to determine intercept
       xcept(i)= 5.0000 - (slope(i)*alog10(effc(i)))      
c
      enddo
c
      endif !EC50-derived slopes
c
c     
c     input toxicity data for either probit, triangular, or piecewise linear E-R functions
      if(islope.ne.0)then
c
      do mm=1,4
      read (27,12) ruler
      write(8,12) ruler
      enddo
c
c
      do i=1,nplants
      read ( 27,202)jpop,slope(i),xiday(i),effc(i),kname(i)
      write(8,202)jpop,slope(i),xiday(i),effc(i),kname(i)  
c 
c     calculate intercept using slope and EC50
      if(islope.eq.1)xcept(i)= 5.0 - slope(i)*alog10(effc(i)) !probit E-R function
      if(islope.eq.2)xcept(i)= 0.      !no intercept used in triangular E-R function
      if(islope.eq.3)xcept(i)= 0.5 - slope(i)*alog10(effc(i)) !piecewise linear E-R function 
      enddo
202   format(i8,f10.2,5x,f6.2,f8.1,8x,a31)
c
c     input consumer toxicity data - same content as in original CASM toxicity scenario 1
      do i=1,2
      read (27,12) ruler
      write(8,12) ruler
      enddo
c
      do i=nplants+1,npops
	  read(27,'(13x,f8.4,2x,f6.2,f12.1,f8.1)')
     .         enpt(i),xiday(i),effc(i),xnoec(i)
      write(8,'(5x,i3,f12.2,f8.1,f10.1,f12.1)')
     .              i, enpt(i),xiday(i),effc(i),xnoec(i)  
        enpt(i)=enpt(i)/100. !convert percentage to fractional impact
c
c      NOTE: EC50-derived slopes and intercepts used for consumers for all E-R user options
c      delta-y probits: (5.0000-0.0001=4.9999) uses Bliss 1934 assignment of 0.0001 probits to NOECs
c      delta-x exposure concentration in log10 units
       slope(i)=0.
       xcept(i)=0.
c
       if(xnoec(i).eq.0.) xnoec(i)=1.0 !avoids taking log10 of zero
       if(xnoec(i).lt.effc(i))  !usual case
     .	     slope(i)=4.9999/(alog10(effc(i))-alog10(xnoec(i)))
       if(xnoec(i).ge.effc(i))  !infeasible, define delta-x as log10 of EC50 or LC50
     .         slope(i)=4.9999/alog10(effc(i))
c      EC50-derived intercepts
c      use point (LC50,5.000) to determine intercept
               xcept(i)= 5.0000 - (slope(i)*alog10(effc(i)))      
c
      enddo
c     
      endif ! probit, triangular, piecewise slopes
c     SKN ADDED Aug 2015
      endif ! Earlier TEF-based toxicity version incorporated 
c     AQUATOX-based toxicity approach inputs loaded here
      if(itoxmdl.eq.1) then
	  open(123,file='casm_toxinptest.dat',status='unknown')
        read(27,12)ruler
        write(123,12)ruler
        read(27,12)ruler
        write(123,12)ruler
        read(27,'(a16)') chmnm
        write(123,'(a16)') chmnm
        if(chmnm.ne.chemnm) then
            write(*,2025)chemnm
        endif
        read(27,12)ruler
        write(123,12)ruler
        read(27,'(i1)')lethal
        write(123,'(i1)')lethal
        do i=1,2
            read(27,12)ruler
            write(123,12)ruler
        enddo
        do i=1,nplants
            read(27,2021)jpop,slope(i),xiday(i),effc(i),pclc50(i),
     .      elemr(i), kname(i)
            write (123,2021)jpop,slope(i),xiday(i),effc(i),pclc50(i),
     .      elemr(i), kname(i)
        enddo
        do i=1,6
            read(27,12)ruler1
            write(123,12)ruler1
        enddo
        do i=nplants+1,npops
	      read(27,2022)
     .         enpt(i),xiday(i),effc(i),xnoec(i),pclc50(i),idetr(i),
     .         fracLip(i),elemr(i),wetwt(i),q10(i),rb(i),uptrt(i),
     .         kname(i)
		  write (123,2026)  
     .         enpt(i),xiday(i),effc(i),xnoec(i),pclc50(i),idetr(i),
     .         fracLip(i),elemr(i),wetwt(i),q10(i),rb(i),uptrt(i),
     .         kname(i)
        enddo
        do i=1,4
            read(27,12)ruler
            write(123,12)ruler
        enddo
        read(27,2023)slpfct,shpfct,alogkow,iab,ihydr,ik2,ibarb
        write(123,2023)slpfct,shpfct,alogkow,iab,ihydr,ik2,ibarb
        read(27,12)ruler
        write(123,12)ruler
        read(27,2024)pKa,pH
        write(123,2024)pKa,pH
        read(27,12)ruler
        write(123,12)ruler
        read(27,2024)wtod,frcdetr
        write(123,2024)wtod,frcdetr
        endif
2021    format(i8,f10.2,5x,f6.2,f8.1,6x,f9.2,3x,f7.4,2x,a31)
2022    format(13x,f8.4,2x,f6.2,f11.0,1x,f8.1,3x,f9.0,
     .     3x,i1,7x,f4.0,3x,E9.2,2x,f6.0,8x,f4.0,1x,f6.0,1x,f8.0,1x,a31)
2023    format(3f8.2,4(7x,i1))
2024    format(2f8.2)
2025    format('Wrong toxicity file selected for ',a16,'.')
2026    format(13x,f8.4,2x,f6.2,f11.3,1x,f8.2,3x,E9.2,
     .  3x,i1,7x,f4.2,3x,E9.2,2x,E10.3,8x,f4.2,1x,E10.3,1x,E8.1,1x,a31)
c END SKN Completed addition of AQUATOX-based toxicity input
        close(17)
        close(27)
        close(123)
        close(127)
        close(227)
c
      return
      end
c
c
c    Subroutine to calculate mean and stdev of daily water current velocities
      Subroutine cvstat
      implicit real*4(a-h,o-z)
c
      common/env/ein(365),te(365),th(365),xpe(365),xne(365),xse(365),
     .           depth(365),cvel(365),tis(365),POCe(365),wind(365),
     .           salinity(365)      
      common/slgh/xmean,xstdev !values used in estimating periphyton sloughing
c
      xmean=0.
      xstdev=0.
      sum=0.
c
c     mean value
      do i=1,365
      sum=sum+cvel(i)
      enddo
      xmean=sum/365.
c
      sum=0.
c     standard deviation
      do i=1,365
      sum=sum+(cvel(i)-xmean)**2.0
      enddo
      xstdev=sqrt(sum/364.)
c
      return
      end
c
c     This routine saves the original input biomass values 
c     i.e., the first 40 valuess in parmp(); the first 40 in parmc()..
c
      Subroutine sav_b
      implicit real*4(a-h,o-z)
      common/p/parmp(1281)
      common/c/parmc(1362)
      common/px/parmpx(1281)
      common/cx/parmcx(1362)
c
      open(111,file='sav_b_cmb.dat',status='unknown')
      write(111,*)'biomass values'
      do i=1,40
        parmpx(i)=parmp(i)
        write(111,*)i,parmp(i)
      enddo
      write(111,*)'parmc'
      do i=1,40
        parmcx(i)=parmc(i)
        write(111,*)i,parmc(i)
      enddo
      close(111)
      return
      end
c
c     This routine saves the original input parameter values 
c
      Subroutine sav_p
      implicit real*4(a-h,o-z)
      common/p/parmp(1281)
      common/c/parmc(1362)
      common/px/parmpx(1281)
      common/cx/parmcx(1362)
c
      open(112,file='sav_p_cmb.dat',status='unknown')
      write(112,*)'param values'
      do i=41,1281
        parmpx(i)=parmp(i)
        write(112,*)i,parmp(i)
      enddo
      write(112,*)'parmc'
      do i=41,1362
        parmcx(i)=parmc(i)
     	write(112,*)i,parmc(i)
      enddo
      close(112)
      return
      end
c
c
c     This routine restores the original biomass values
c     read from file on input, or values recalculated
c     for simulations other than days 001-365
c
      Subroutine rstor_b
      implicit real*4(a-h,o-z)
      common/p/parmp(1281)
      common/c/parmc(1362)
      common/px/parmpx(1281)
      common/cx/parmcx(1362)
c
      do i=1,40
        parmp(i)=parmpx(i)
      enddo
      do i=1,40
        parmc(i)=parmcx(i)
      enddo
      return
      end
c
c
c     This routine restores the original growth parameter values
c     read from file on input
c
      Subroutine rstor_p
      implicit real*4(a-h,o-z)
      common/p/parmp(1281)
      common/c/parmc(1362)
      common/px/parmpx(1281)
      common/cx/parmcx(1362)
c
      do i=41,1281
        parmp(i)=parmpx(i)
      enddo
      do i=41,1362
        parmc(i)=parmcx(i)
      enddo
      return
      end
c
c
c     The following subroutines execute the CASM model
c
c     nr=iteration number (0 = reference, 1 = exposure scenario)
c
      Subroutine casm(nr)
      implicit real*4(a-h,o-z)
      integer bpflag(4,10), bcflag(8,5)
      real*4 frkld(4,10),frkldc(8,5)
      character*16 chemnm
c
      common/t/nsp(4),nsc(8)
      common/rat1/fg(4,10)
      common/p/  bp(4,10),                     !Initial biomass, DOY=1)
     .           ps(4,10),                     !max photosyhnethetic rate
     .         pte1(4,10),pxk1(4,10),          !Thornton-Lessem temperature parameters   
     .         pte2(4,10),pxk2(4,10),   
     .         pte3(4,10),pxk3(4,10),   
     .         pte4(4,10),pxk4(4,10),   
     .           si(4,10),                     !Light saturation intensity 
     .          snk(4,10),                     !Sinking rate
     .          xkp(4,10),xkn(4,10),xks(4,10), !Michaelis-Menten constants for P, N, and Si 
     .          pm(4,10),                      !Natural mortality rate
     .        resp(4,10),                      !Photorespiration rate
     .        resd(4,10),                      !Dark respiration rate
c
c             Habitat factors for salinity, current velocity, and depth
     .        psplt(4,10),pspol(4,10),pspou(4,10),psput(4,10),  !salinity
     .        ppvlt(4,10),ppvol(4,10),ppvou(4,10),ppvut(4,10),  !velocity
     .        ppdlt(4,10),ppdol(4,10),ppdou(4,10),ppdut(4,10),  !depth
c
c             Phenology         
     .         ddi(4,10),   !Degree-days required for growth initiation
     .         dds(4,10),   !Degree-days for onset of senescence
     .              xlk
c
      common/c/  bc(8,5),             !Initial biomass 
     .          cmx(8,5),             !Max consumption rate
     .         cte1(8,5),cxk1(8,5),   !Thornton-Lessem temperature parameters   
     .         cte2(8,5),cxk2(8,5),   
     .         cte3(8,5),cxk3(8,5),   
     .         cte4(8,5),cxk4(8,5),   
     .         rsda(8,5),    rs(8,5),           !specific dynamic action rate, respiration rate
     .          tro(8,5),   trm(8,5),           !temperature dependence of respiration
     .            f(8,5),     u(8,5),  cm(8,5), !egestion, excretion, mortality rates
     .          dft(8,5),                       !invertebrate drift, 1/d 
c
c             Habitat factors for dissolved oxygen, salinity, current velocity, and depth
     .         DOlt(8,5), DOol(8,5), DOou(8,5), DOut(8,5),  !dissolved oxygen
     .        psclt(8,5),pscol(8,5),pscou(8,5),pscut(8,5),  !salinity
     .        ccvlt(8,5),ccvol(8,5),ccvou(8,5),ccvut(8,5),  !velocity
     .        ccdlt(8,5),ccdol(8,5),ccdou(8,5),ccdut(8,5),  !depth
     .             bxkn,      bxkp
c
      common/env/ein(365),te(365),th(365),xpe(365),xne(365),xse(365),
     .           depth(365),cvel(365),tis(365),POCin(365),wind(365),
     .           salinity(365)      
      common/env2/degdays(365)
c
      common/decomp/rcle,tcle_o,tcle_m, !decomposition labile POC epil      
     .              rcre,tcre_o,tcre_m, !              refractory POC epil
     .              rclh,tclh_o,tclh_m, !              labile POC hypol     
     .              rcrh,tcrh_o,tcrh_m, !              refractory POC hypol
     .              stl                 !POC settling rate (1/d)
c
      common/arg1/om(2,2),yo(40),egn(40),cl(84),tux(40),trsdax(40)
      common/hstry/bmass(80,365), znut(365,12),phar(365,5)
      common/txphys/tnut(365,12)
      common/p_init/  fpp,fpn,fps,fpdo
      common/c_init/  fcp,fcn,fcs,fcdo
      common/cadv/adv(365) !advective loss (1/d)
c
      common/csm1/nsys
      common/csm4/npops
      common/csm2/ncycle,   !value of 1 includes internal cycling,  0 turns off cycling
     .            limit_ps  !value of 0 uses simple multiplication, 1 uses geometric mean
      common/csm3/delta_t   !time step, units = days
c
      common/dbg/idebug
c
      common/res1/isyndrome,islope
      common/tef_q4/delp_r(4,10,365),delp_e(4,10,365)    !daily specific growth rates, %/day
      common/ext/exH2O,exDOC,exPOC,exTIS
      common/ext2/fd1,fd2,fd3,fd4
c
c     arrays for Steinhaus similarity coefficient
      common/stein/rmass(80,365), !daily biomass values for reference simulation
     .             tmass(80,365)  !daily biomass values for treatment simulations
c
c     led: last exposure day
      common/trk/ied,led
c
c     arrays to plot biomass vs time
      common/plotr/pltref(80,365)
      common/plote/plteff(80,365)
c      
      common/daypry1/dlyprey(84,365) !daily prey biomass values
      common/daypry2/dlyenv(13) 
c     guild daily net production
      common/anprod1/pg_netprod(4,365), !producer guilds
     .               cg_netprod(8,365)  !consumer guilds
c
      common/blkmic/xiday(80),enpt(80),effc(80),pclc50(80),idetr(80),
     .         fracLip(80),elemr(80),wetwt(80),q10(80),rb(80),uptrt(80),
     .              xnoec(80),envc(365),kappa
      common/blkmict/slpfct,shpfct,alogkow,iab,ihydr,ik2,pKa,pH,wtod,
     .          frcdetr,lethal,ibarb
      common/toxpar/itoxmdl,chemnm
c
      dimension ss(80),bpinit(4,10),bcinit(8,5)
      dimension pp(4,10)
c
       if(nr.eq.0)kunit=990 !output to master_ref file
       if(nr.eq.1)kunit=992 !output to master_eff file
c 
	   np=4 !number of producer guilds
       nc=8 !number of consumer guilds
c
      nperday=ifix(0.001+(1.0/delta_t)) !the number of sub-daily loops, inverse of the time step, delta_t

c     if not casm(0), be sure deriv info is turned off to
c     avoid generation of ~70 mB file!!!
      if(nr.gt.0)idebug=0
c
c
      do i=1,80
        do j=1,365
        bmass(i,j)=0.
        enddo
      enddo
c
c     initialize arrays for Steinhaus similarity
c     
      if(nr.eq.0)then
        do i=1,npops
	    do j=1,365
	     rmass(i,j)=0.
         tmass(i,j)=0.
	    enddo
	  enddo
c
c     initialize phys-chem tracking arrays
      do i=1,365
	   do j=1,12
	    znut(i,j)=0.
	    tnut(i,j)=0.
       enddo
	  enddo
      endif !Steinhaus initialization
c
c     initial nutrient conditions, mg/L = g/m3
      en=0.
      ep=0.
      es=0.
      hn=en
      hp=ep
      hs=es
c
c     organic matter pools, mg/L
      om(1,1)=0.
      om(1,2)=0.
      om(2,1)=0.
      om(2,2)=0.
c
c      
c     Initial oxygen concentration assumed at temperature-dependent saturation
      temp0=te(1)
      edo=O2sat(temp0)  !O2 conc (mgO2/L) in epilminion
      hdo=edo        !                 in hypolimnion 
c
c     ZERO initial biomass - condition
c
      do k=1,np
      do ki=1,10
         bpflag(k,ki)=0
         bpinit(k,ki)=0.0
      enddo
      enddo
c      
      do k=1,nc
      do ki=1,5
       bcflag(k,ki)=0
	   bcinit(k,ki)=0.0
      enddo
      enddo   
c         
      do j=1,np
        nn=nsp(j)
        do i=1,nn
          if(bp(j,i).gt.0.0) bpflag(j,i)=1
          if(bp(j,i).gt.0.0) bpinit(j,i)=bp(j,i) 
        enddo
      enddo
c          
      do j=1,nc
        nn=nsc(j)
        do i=1,nn      
          if(bc(j,i).gt.0.0) bcflag(j,i)=1
	    if(bc(j,i).gt.0.0) bcinit(j,i)=bc(j,i)
        enddo
      enddo
c           
c
         do j=1,npops
           ss(j)=0.
	   enddo
c
c
c     Begin daily loop
      do 12345 i=1,365      
c
      iday=i
c
c     initialize dlyprey() for each modeled year
      if(iday.eq.1)then
        do iprey=1,84
	    do k=1,365
           dlyprey(iprey,k)=0.
          enddo
        enddo
      endif
c 
c     daily depth values for stream depth (m) and current velocity (m/s)
      z=depth(i)+0.01 !assume at least 0.01 m of water to guard against input depth value of zero
      v= cvel(i)
c
c     Conversion from gm/2 to mg/L: used to convert biomass to 
c     nutrient equivalents, e.g., excretion, respiration, etc.
c                   mg/L = 1/depth(m) * g/m2 * 1000 mg/g * m3/1000 L
      cnv=1.0/z   ! z=water depth (m)
c
c     for atrazine exposure scenario simulation
c        SKN added toxicity assessment based on AQUATOX (itoxmdl=1)
      if(itoxmdl.eq.0) then
       if(nr.eq.1) call genp(i)     !adjust the bioenergetics parameters using TEF values
      endif
c
c
c     temperature input data 
      wte=te(i)      ! wte = water temperature in epilimnion
      wth=th(i)      ! wth = water temperature in hypolimnion
c
c     solar radiation input data
      rr=ein(i) * 0.5 !convert Io to PAR 
      phar(i,1)=rr
      if(wte.lt.3.0)rr=rr*0.33  ! simulate ice-cover effect on light
c
c     initialize nutrients to zero each day if no recycling
      if(ncycle.eq.0)then
	  ep=0.
	  en=0.
	  es=0.
      endif
c
c     nutrient loading from input data (mg/L)
c     adjust eplimnetic p, n, si concentrations (mg/L)
c     --------------------- 
      ep=ep+xpe(i)
      en=en+xne(i)
      es=es+xse(i)
c
c    adjust nutrients in hypolimnion, if not stratified
      if(ncycle.eq.1) then
       if(i.eq.1 .or. wte.eq.wth)hp=hp+ep
       if(i.eq.1 .or. wte.eq.wth)hn=hn+en
       if(i.eq.1 .or. wte.eq.wth)hs=hs+es
      endif
c
c     inputs of POC 
      om(1,2)=om(1,2)+ POCin(i) 
c
      if(ncycle.eq.1)then
c     adjust dissolved oxygen for air:water exchange
      call reaer(wte,z,v,xkaer) !calculates reaeration coefficient (1/d)
      O2se=O2sat(wte)
      edo=edo+xkaer*(O2se-edo)  !units: mg O2/L/d
c
      if(edo.gt.O2se) edo=O2se
c     hypolimnion temperature adjustment
      O2sh=O2sat(wth)
      hdo=hdo+xkaer*(edo-hdo)
      if(hdo.gt.O2sh) hdo=O2sh
      endif
c     
c
c     Accumulators
c     Producer population dynamics
      tdp=0   !accumulator for DIP utilization. (mg/L)
      tdn=0.  !accumulator for DIN utilization  (mg/L)
      tds=0.  !accumulator for SI  utilization  (mg/L)
      spe=0.  !accumulator for plant gross production (gC/m2)
      trsp=0. !accumulator for plant respiration      (gC/m2)
c
c     Consumer population dynamics
      sre=0.    ! epil. accumulator for  respiration,
      sfe=0.    !                        egestion
      sue=0.    !                        ecretion
      sme=0.    !                        mortality
      srh=0.    ! hypol. accumulator for respiration,
      sfh=0.    !                        egestion
      suh=0.    !                        ecretion
      smh=0.    !                        mortality
c
c     Begin sub-daily loop
      do 12344 istep=1,nperday
c
c
      if(idebug.eq.1 .or. idebug.eq.2)write(177,2000)
2000  Format(//'CASM Primary Producer Populations'/
     .1x,'Day  i  j  cdp      cdn      cds      tmp',
     .'      gi       hsal     hvel     hdep     hmod     xft',
     .'      ps       fg')

c     light extinction due to daily values of non-bio components of extinction
       xkx=0.
c
       y1=exH2O
	   y2=exDOC*om(1,1)
	   y3=exPOC*om(1,2)
	   y4=exTIS*tis(i)
c
       xkx=y1+y2+y3+y4 !Adapted from Park and Clough 2004 
c
c     sum plant biomass for shading effects
c     initialize daily biomass sums for use in light extinction calculations
      z1=0. !phytoplankton
      z2=0. !periphyton
      z3=0. !SAV
      z4=0. !emergent aquatic plants
c
c
      do j=1,10
       z1=z1+bp(1,j) !phytoplankton biomass (shades phytoplankton, periphyton, macrophytes)
       z2=z2+bp(2,j) !periphyton biomass    (shades periphyton)
       z3=z3+bp(3,j) !macrophyte biomass    (shades phytoplankton and periphyton)
       z4=z4+bp(4,j) !emergent biomass      (shades macrophytes and periphyton)
      enddo
c
      do 50 ii=1,np
            nn=nsp(ii)
	      if(ii.eq.1)h=depth(i)*fd1 ! depth for phytoplankton calculation
	      if(ii.eq.2)h=depth(i)*fd2 !           periphyton
	      if(ii.eq.3)h=depth(i)*fd3 !           macrophytes
	      if(ii.eq.4)h=depth(i)*fd4 !           emergents
c
c     light limitation of photosynthesis
          z5=0.
          xke=0.
          ksys=nsys
          z5=exBIO(ksys,ii,z,z1,z2,z3,z4)   !recall: z is depth(i)
c 
          xke=xkx+z5
c          
          reff=rr*exp(-xke*h)                            !Thomann and Mueller 1987
          phar(i,ii+1)=reff
c
      do 50 j=1,nn
c
      fg(ii,j)=0. !initialize 
c
c     phenology check for initiation
c
      if(degdays(i).lt.ddi(ii,j))goto 47 !if accumulated degree-days < requirement for initiation,
c                                         skip to next population
	    gi=(reff/si(ii,j))*exp((-reff/si(ii,j)) + 1.0) !Thomann and Mueller 1987
        gi=amax1(0.,gi)
c
c     nutrient limitation of phytoplankton and periphyton photosynthesis
         condp=ep/(ep+(xkp(ii,j)))                    ! P-limitation: unitless (0-1)
         condn=en/(en+(xkn(ii,j)))                    ! N-limitation: unitless (0-1)
         conds=1.0                                    ! Si-limitiation: unitless: assume 1 for non-diatoms
         if(xks(ii,j).gt.0.)conds=es/(es+(xks(ii,j))) ! Si-limitation: unitless (0-1)
         cond=amin1(condp,condn,conds)                ! assume Liebig Law of Minimum for nutrient limitation
c
c     assume macrophytes and emergents are not limited by water column nutrients
         if(ii.eq.3 .or. ii.eq.4)cond=1.0
c                                     
c     producer gross photosynthesis
c     temperature effects 
         te1=pte1(ii,j) !Thornton-Lessem input parameters
         te2=pte2(ii,j)
         te3=pte3(ii,j)
         te4=pte4(ii,j)
c	    
         xk1=pxk1(ii,j)
         xk2=pxk2(ii,j)
         xk3=pxk3(ii,j)
         xk4=pxk4(ii,j)
c
         wt_ht=wte
         temp=ht2(te1,xk1,te2,xk2,te3,xk3,te4,xk4,wt_ht) !temperature modifier
c
c     salinity effects
         x1=psplt(ii,j)
         x2=pspol(ii,j)
         x3=pspou(ii,j)
         x4=psput(ii,j)
         sal=salinity(i)
c
         hsal=tz(x1,x2,x3,x4,sal) !salinity modifier
c
c     velocity effects
         x1=ppvlt(ii,j)
         x2=ppvol(ii,j)
         x3=ppvou(ii,j)
         x4=ppvut(ii,j)
         vel=cvel(i)
c
         hvel=tz(x1,x2,x3,x4,vel) !velocity modifier
c
c     depth effects
         x1=ppdlt(ii,j)
         x2=ppdol(ii,j)
         x3=ppdou(ii,j)
         x4=ppdut(ii,j)
         dep=depth(i)
c
         hdep=tz(x1,x2,x3,x4,dep) !depth modifier
c
c     define habitat modification as minimum of modifiers
         hmod=amin1(hsal,hvel,hdep)
c
	  xft=0.
      if(limit_ps .eq.0) xft=(temp*cond*gi*hmod)  ! use product of temp, nutrient, light regulation of growth: unitless
      if(limit_ps .eq.1) xft=(temp*cond*gi*hmod)**0.25  ! use geo-mean of temp, nutrient, light regulation of growth: unitless
c
         fg(ii,j)=delta_t*ps(ii,j)*xft  ! population gross photosynthesis rate (really gross growth rate):  1/d
c
c
47    continue
c
      if(idebug.eq.1 .and. bpflag(ii,j).eq.1
     . .and. degdays(i).ge.ddi(ii,j))
     .   write(177,2222)i,ii,j,condp,condn,conds,temp,gi,
     .                        hsal,hvel,hdep,hmod,
     .                        xft,ps(ii,j),fg(ii,j)
c
      if(idebug.eq.2 .and. bpflag(ii,j).eq.1
     . .and. degdays(i).ge.ddi(ii,j))
     .   write(177,2222)i,ii,j,condp,condn,conds,temp,gi,
     .                        hsal,hvel,hdep,hmod,
     .                        xft,ps(ii,j),fg(ii,j)
2222  format(1x,i3,2(1x,i2),12(1x,e8.3))
c
c
      if(idebug.eq.1 .and. bpflag(ii,j).eq.1
     . .and. degdays(i).lt.ddi(ii,j))
     .   write(177,2223)i,ii,j
      if(idebug.eq.2 .and. bpflag(ii,j).eq.1
     . .and. degdays(i).lt.ddi(ii,j))
     .   write(177,2223)i,ii,j
2223  format(1x,i3,2(1x,i2),'  Insufficient degree-days,',
     .                        ' population not active')
c
50    continue

c
52    continue
c
c
      pplus=0.   !sum of phyto and peri biomass for nutrient update
c
      do 57 ii=1,np
      nn=nsp(ii)
      do 56 j=1,nn
c     phenology check for initiation
      if(degdays(i).lt.ddi(ii,j))goto 56 !if accumulated degree-days < requirement for initiation,
c                                         skip to next population
         pp(ii,j)=0.
c     population net gain
         pp(ii,j)=bp(ii,j)*fg(ii,j)*(1.0-resp(ii,j))    !photoresp. gC/m2/d * unitless = gC/m2/d
         if(pp(ii,j).lt.0.) pp(ii,j)=0.
         spe=spe+pp(ii,j)
c     total gross production
         if(ii.le.2) pplus=pplus + (bp(ii,j)*fg(ii,j))
         if(pplus.lt.0.) pplus=0.
c
      if(ncycle.eq.1)then
c     update total nutrient demands
c
         if(ii.le.2)tdp=tdp+(pplus*fpp*cnv)          ! DIP demand (mg/L)
         if(ii.le.2)tdn=tdn+(pplus*fpn*cnv)          ! DIN demand (mg/L)
         if(xks(ii,j).gt.0.) tds=tds+(pplus*fps*cnv) !  Si demand (mg/L)
      endif
c
56    continue
57    continue
c
c
c     Calculate consumption for all predators and prey
       wte_con=wte
       wth_con=wth
       dt=delta_t
       call cns(dt,wte_con,wth_con,iday)
c
c     Note that cns() calculates daily consumption, assimilation, egestion, and excretion components
c     for all consumer populations, as well as population losses to grazing and predation, array cl(): gC/m2/d  
c
      if(idebug.eq.1 .or. idebug.eq.2)write(177,2002)
2002  format(//1x,'Day  i  j kk   b(t-1)     grpod      fg         ',
     .            'dk resp    sink       pmort      sluff      gr loss',
     .            '    delp       b(t)')
c
      ipop=0
      do 61 ii=1,np            ! Update producer biomass
      nn=nsp(ii)
        do 59 j=1,nn
c
        ipop=ipop+1
        kk=j+(ii-1)*10
c
      delp=0.
c     phenology check for initiation
      if(degdays(i).lt.ddi(ii,j))goto 159 !if accumulated degree-days < requirement for initiation,
c                                         skip to next population
        bp_0=bp(ii,j)
	    prsp=delta_t*resd(ii,j)*bp(ii,j)   ! Dark respiration loss (1/d * gC/m2)
	    trsp=trsp+prsp                     ! Accumulate respiration to calculate DO uptake 
        sink=delta_t*snk(ii,j)*bp(ii,j)    ! Sinking loss      (1/d * gC/m2)
        pmrt=delta_t*pm(ii,j)*bp(ii,j)     ! Mortality loss    (1/d * gC/m2)
	    pred=cl(kk)                        ! Grazing/predation (1/d * gC/m2)
c
        if(degdays(i).gt.dds(ii,j))
     .    pmrt=delta_t*2.0*pm(ii,j)*bp(ii,j) ! Senescence is double the natural mortality rate
c
          xslf=0.
          sluff=0. ! Sloughing        (1/d * gC/m2)
          if(ii.eq.2)call slough(iday,xslf)
          sluff=delta_t*xslf*bp(ii,j)        ! Sloughing is function of current velocity
c
c     update organic matter                                !NOTE: conversions of loss terms from gC/m2 to mgC/L using cnv factror
          if(ii.eq.1) om(1,1)=om(1,1)+(prsp*cnv)         !DOCe  receives phytoplankton respired C 
	      if(ii.ge.2) om(2,1)=om(2,1)+(prsp*cnv)         !DOCh  receives periphtyon and macrophyte respired C
          if(ii.eq.1) om(1,2)=om(1,2)+((sink+pmrt) *cnv) !POCe  receives phytoplankton sinking and mortality C losse, which then settle to POMh 
          if(ii.ge.2) om(2,2)=om(2,2)+((sluff+pmrt)*cnv) !POCh  receives periphtyon sloughing and mortality, and macrophyte mortality C losses
c
c     plant population daily growth increment, delp
            delp=pp(ii,j)-(prsp+sink+pmrt+sluff) - pred    ! gC/m2/delta_t
c           Note: the components to the above derivative have been previously adjusted by the delta_t        
c
c     store positive daily net production by guild
            if(nr.eq.0)pg_netprod(ii,i)=pg_netprod(ii,i)+amax1(0.,delp)

c     calculate daily specific growth rates (without grazing)for producers, %/day
            zdelp=0.
            if(bp(ii,j).gt.0.)
     .      zdelp=100.*(pp(ii,j)-prsp-sink-pmrt-sluff)/bp(ii,j)
c
            if(nr.eq.0) delp_r(ii,j,i)=delp_r(ii,j,i)+zdelp !sums the sub-day increments to daily growth rates
            if(nr.eq.1) delp_e(ii,j,i)=delp_e(ii,j,i)+zdelp
c
            bp(ii,j)=amax1(bpinit(ii,j)*0.1,bp(ii,j)+delp)   ! update biomass, NOTE: 10% of initial biomass used as refuge value 
c
c	Lethal Effects from AQUATOX Formaulation SKN added August 2015 for plants
		  if(nr.eq.1.and.itoxmdl.eq.1) then
		  llm=ipop
		  conc=envc(iday)
	      if(itoxmdl.eq.1.and.lethal.eq.0) then !Lethal toxicity, external model
			eta=-2.0*slpfct/alog(0.5) !E431
			ak=-alog(0.5)/(pclc50(llm)**eta) !E430
			cumFrKl=1.0-exp(-ak*(conc**eta)) !E429
			frrst=bp_0/bp(ii,j) !fraction resistant in previous time step
			if(frrst.gt.1.0) then frrst=1.0
			if(iday.eq.1.and.istep.eq.1) then
				frkld(ii,j)=0.0 !FracKilled in E417
			else
				frkld(ii,j)=
     .				amax1(0.0,cumFrKl-frkld(ii,j)) !FracKilled in E417
			endif
			psnd=frrst*bp(ii,j)*frkld(ii,j)
     .			+(1.0-frrst)*bp(ii,j)*cumFrKl !E417
		  elseif(itoxmdl.eq.1) then !Lethal toxicity, internal model and sublethal toxicity
			if(ii.le.2) then
				if(ihydr.eq.1.and.iab.eq.0) then !ionized and hydrophilic acids
					dissno=1.0/(1.0+10.0**(pH-pKa))  !E311
					corrIon=0.1 
					akow=10.0**alogkow
					bcfalg=2.57*(akow**0.93)*dissno+(1.0-dissno)*
     .						corrIon*0.257*(akow**0.93)		!E342				
					if(ik2.eq.0) then
						elemr(llm)=1.0/
     .					(1.58+0.000015*akow*dissno) !E359
					endif
				elseif(ihydr.eq.1.and.iab.eq.1) then !ionised and hydrophilic bases
					dissno=1.0/(1.0+10.0**(pKa-pH)) !E312
					corrIon=0.01
					bcfalg=2.57*(akow**0.93)*dissno+(1.0-dissno)*
     .						corrIon*0.257*(akow**0.93)	!E342				
					if(ik2.eq.0) then
						elemr(llm)=1.0/
     .	   					(1.58+0.000015*akow*dissno) !E359
					endif
				elseif(ihydr.eq.0) then !hydrophobic and non-ionized
					bcfalg=10.0**(0.41+0.91*alogkow) !E341
				endif
				aILC50=bcfalg*pclc50(llm) !E410
				aLCInf=aILC50*(1.0-exp(-elemr(llm)*xiday(llm)))
				if(xiday(llm).gt.(4.605/elemr(llm))) then
					cLethal=aLCInf
				else
					if(ik2.eq.0) then
						elemr(llm)=2.45E5/
     .					 (akow*0.2*wtod) !lipid content of algae) 20%% - AQUATOX 
					endif
					cLethal=aLCInf/(1.0-exp(-elemr(llm)*xiday(llm)))
				endif
				cInt=bcfalg*conc*(1.0-exp(-(0.5*elemr(llm))))
				cumFrKl=1.0-
     .				exp(-(cInt/cLethal)**(1.0/shpfct))
				if(cumFrKl.gt.0.95) cumFrKl=1.0
			elseif(ii.eq.3) then
				if(ihydr.eq.1.and.iab.eq.0) then !ionized and hydrophilic acids
					dissno=1.0/(1.0+10.0**(pH-pKa))
					akow=10.0**alogkow
					bcfmac=0.00575*(akow**0.98)*(dissno+0.2)					
					if(ik2.eq.0) then
						elemr(llm)=1.0/
     .					 (1.58+0.000015*akow*dissno)
					endif
				elseif(ihydr.eq.1.and.iab.eq.1) then !ionised and hydrophilic bases
					dissno=1.0/(1.0+10.0**(pKa-pH))
					akow=10.0**alogkow
					bcfmac=0.00575*(akow**0.98)*(dissno+0.2)					
					if(ik2.eq.0) then
						elemr(llm)=1.0/
     .					 (1.58+0.000015*akow*dissno)
					endif
				elseif(ihydr.eq.0) then !hydrophobic and non-ionized
					bcfmac=10.0**(0.98*alogkow-2.24)
					if(ik2.eq.0) then
						elemr(llm)=2.45E5/
     .					 (akow*0.002*wtod) !lipid content of macrophytes ) 0.2% - AQUATOX 
					endif
				endif
				aILC50=bcfmac*pclc50(llm)
				aLCInf=aILC50*(1.0-exp(-elemr(llm)*xiday(llm)))
				if(xiday(llm).gt.(4.605/elemr(llm))) then
					cLethal=aLCInf
				else
					cLethal=aLCInf/(1.0-exp(-elemr(llm)*xiday(llm)))
				endif
				cInt=bcfmac*conc*(1.0-exp(-(0.5*elemr(llm))))
				cumFrKl=1.0-
     .				exp(-(cInt/cLethal)**(1.0/shpfct))
				if(cumFrKl.gt.0.95) cumFrKl=1.0
			elseif(ii.eq.4) then
				cumFrKl=0.0
			endif
			frrst=bp_0/bp(ii,j)
			if(frrst.gt.1.0) then frrst=1.0
			if(iday.eq.1.and.istep.eq.1) then
				frkld(ii,j)=0.0
			else
				frkld(ii,j)=
     .				amax1(0.0,cumFrKl-frkld(ii,j))
			endif
			psnd=frrst*bp(ii,j)*frkld(ii,j)
     .			+(1.0-frrst)*bp(ii,j)*cumFrKl
			afphoto=effc(llm)/pclc50(llm)
			fracph=exp(-((cInt/(cLethal*afphoto))**(1/shpfct)))
			ppchange=pp(ii,j)*(1.0-fracph)
		  endif
	      bp(ii,j)=bp(ii,j)-psnd*delta_t-ppchange
	endif

159      if(bpflag(ii,j).eq.0) bp(ii,j)=0.
            kspp=(ii-1)*10+j                   ! species tag
c
            if(nr.eq.0)bmass(kspp,i)=bp(ii,j)  ! reference biomass vs time array  
            if(nr.eq.0)rmass(ipop,i)=bp(ii,j)  ! reference biomass vs time array  
            if(nr.eq.1)tmass(ipop,i)=bp(ii,j)  ! treatment biomass vs time array  
c
            dlyprey(ipop,i)=bp(ii,j)           ! daily output biomass   
c 
            ss(ii)=ss(ii)+bp(ii,j)             ! Update trophic levels
c
      if(idebug.eq.1 .and. bpflag(ii,j).eq.1 
     .               .and. degdays(i).ge.ddi(ii,j))
     .  write(177,2003)i,ii,j,kk,bp_0,pp(ii,j),fg(ii,j),
     .                prsp, sink,pmrt,sluff,pred,delp,bp(ii,j)
      if(idebug.eq.2 .and. bpflag(ii,j).eq.1 
     .               .and. degdays(i).ge.ddi(ii,j))
     .  write(177,2003)i,ii,j,kk,bp_0,pp(ii,j),fg(ii,j),
     .                prsp, sink,pmrt,sluff,pred,delp,bp(ii,j)
2003  format(1x,i3,3(1x,i2),10(1x,e10.3))
c      
      if(idebug.eq.1 .and. bpflag(ii,j).eq.1 
     .               .and. degdays(i).lt.ddi(ii,j))
     .  write(177,2005)i,ii,j,kk,bpinit(ii,j)
      if(idebug.eq.2 .and. bpflag(ii,j).eq.1 
     .               .and. degdays(i).lt.ddi(ii,j))
     .  write(177,2005)i,ii,j,kk,bpinit(ii,j)
2005  format(1x,i3,3(1x,i2),1x,e10.3,
     .      '   Insufficient degree-days, population not active')
c
59    continue
61	  continue

c
      if(ncycle.eq.1)then
c     Update nutrients and dissolved oxygen, units = mg/L
        ep=amax1(0.,ep-tdp)                    ! Update nutrients
        en=amax1(0.,en-tdn) 
        es=amax1(0.,es-tds)
        edo=amax1(0.,edo+((spe-trsp)*fpdo*cnv)) ! Producer influences on DO (converts gC/m2 to mgO2/L)
        edo=amin1(O2se,edo)                     ! check for supersaturation
      endif !nutrient cycling update
c
c
c     Consumer population dynamics
c
      if(idebug.eq.2)
     .   write(177,4001)
4001  format(//'CASM Consumer Populations'/
     .1x,'Day ii  j   b(t-1)    cons     resp       xmort',
     .'      drift      emerg      delc       b(t)')
c
c
      do 12343 ii=1,nc
      nn=nsc(ii)
        do 12342 j=1,nn
          k1=j+(ii-1)*5
          k2=k1+40
	    ipop=ipop+1
c         
c     salinity effects
         x1=psclt(ii,j)
         x2=pscol(ii,j)
         x3=pscou(ii,j)
         x4=pscut(ii,j)
         sal=salinity(i)
c
         hsal=tz(x1,x2,x3,x4,sal) !salinity modifier
c
c     velocity effects
         x1=ccvlt(ii,j)
         x2=ccvol(ii,j)
         x3=ccvou(ii,j)
         x4=ccvut(ii,j)
         vel=cvel(i)
c
         hvel=tz(x1,x2,x3,x4,vel) !velocity modifier
c
c     depth effects
         x1=ccdlt(ii,j)
         x2=ccdol(ii,j)
         x3=ccdou(ii,j)
         x4=ccdut(ii,j)
         dep=depth(i)
c
         hdep=tz(x1,x2,x3,x4,dep) !depth modifier
c
c     dissolved oxygen effects
      hdiso=1.0
      if(ncycle.eq.1)then
         x1=DOlt(ii,j)
         x2=DOol(ii,j)
         x3=DOou(ii,j)
         x4=DOut(ii,j)
         if(ii.le.4) diso2=edo
         if(ii.gt.4) diso2=hdo
c
         hdiso=tz(x1,x2,x3,x4,diso2) !DO modifier
      endif !dissolved oxygen limitation
c
c
c     define habitat modification as minimum of modifiers
      hmod=amin1(hsal,hvel,hdep,hdiso)
c
c         hmod=1.0 !test hmod effects
c
          b_0=bc(ii,j)
c
          tm1=tro(ii,j)    !temperature effects on respiration
          tm2=trm(ii,j)
          wte_ht=wte
          wth_ht=wth
        if(ii.le.4)tx=ht(wte_ht,tm1,tm2)
	    if(ii.gt.4)tx=ht(wth_ht,tm1,tm2)
          crsp=delta_t*rs(ii,j)*bc(ii,j)*tx          ! standard respiration loss: 1/d * gC/m2 = gC/m2/d
c
          xm=delta_t*cm(ii,j)*bc(ii,j)               ! mortality loss:            1/d * gC/m2 = gC/m2/d
          cgain=yo(k1)*hmod                         ! included delta_t to old CASM routine consumption gain: gC/m2/d (from cns subroutine)
c
c         adjust benthic insects for emergence and drift
          emerg=0. !current model does not include emergence
c
          drift=delta_t*dft(ii,j)*bc(ii,j) !constant fraction of benthinc invertebrates 
c
c     Note: excretion, egestion, assimilation, and specific dynamic action
c           are computed in subroutine cns( ) 
c
c     consumer population daily growth increment, delc (gC/m2/d)
          delc=cgain-(cl(k2))-(crsp+xm+drift+emerg)
c             
c     consumer guild positive daily net production
          if(nr.eq.0)cg_netprod(ii,i)=cg_netprod(ii,i)+amax1(0.,delc)
c
          bc(ii,j)=amax1(bcinit(ii,j)*0.1,(bc(ii,j)+delc))! update biomass, NOTE: 10% of initial biomass used as refuge
      if(nr.eq.1.and.itoxmdl.eq.1) then
		llm=ipop
		conc=envc(iday)
	    cInt=0.0
		cLethal=1.0
		if(itoxmdl.eq.1.and.lethal.eq.0) then !Lethal toxicity, external model
			eta=-2.0*slpfct/alog(0.5)
			ak=-alog(0.5)/(pclc50(llm)**eta)
			cumFrKl=1.0-exp(-ak*(conc**eta))
			frrst=b_0/bc(ii,j)
			if(frrst.gt.1.0) frrst=1.0
			if(iday.eq.1.and.istep.eq.1) then
				frkldc(ii,j)=0.0
			else
				frkldc(ii,j)=
     .				amax1(0.0,cumFrKl-frkldc(ii,j))
			endif
			psnc=frrst*bc(ii,j)*frkldc(ii,j)
     .			+(1.0-frrst)*bc(ii,j)*cumFrKl
		elseif(itoxmdl.eq.1) then !Lethal toxicity, internal model and sublethal model
			if(ii.eq.1) then !zooplankton
				capc=890.0
				if(idetr(llm).eq.0.and.ihydr.eq.0) then
					bcfzoo=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.1.and.ihydr.eq.0) then
					bcfzoo=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.0.and.ihydr.eq.1) then
				    if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						bcfzoo=0.3663*(akow**0.752)*(dissno+0.01)
					else
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						bcfzoo=0.3663*(akow**0.752)*(dissno+0.01)
					endif
				elseif(idetr(llm).eq.1.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						corrIon=0.1
						akom=1.38*(akow**0.82)*dissno
     .					+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfzoo=fracLip(llm)*akom*(dissno+0.01)/frcdetr
					else
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						corrIon=0.01
						akom=1.38*(akow**0.82)*dissno
     .					+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfzoo=fracLip(llm)*akom*(dissno+0.01)/frcdetr
					endif
				endif
				aILC50=bcfzoo*pclc50(llm)
				if(ik2.eq.0) then
					if(ibarb.eq.1) then
						elemr(llm)=capc*(wetwt(llm)**(-0.197))/
     .						(fracLip(llm)*akow)
					else
						if(wetwt(llm).lt.5.0) then
						elemr(llm)=0.536*alogkow*alog10(dissno)
     .						+0.065*(wetwt(llm)**rb(llm))/fracLip(llm)
						else
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.116*(wetwt(llm)**rb(llm))/fracLip(llm)
						endif
					endif
				endif
				aLCInf=aILC50*(1.0-exp(-elemr(llm)*xiday(llm)))
				if(xiday(llm).gt.(4.605/elemr(llm))) then
					cLethal=aLCInf
				else
					cLethal=aLCInf/(1.0-exp(-elemr(llm)*xiday(llm)))
				endif
				cInt=bcfzoo*conc*(1.0-exp(-(0.5*elemr(llm))))
				cumFrKl=1.0-
     .				exp(-(cInt/cLethal)**(1.0/shpfct))
				if(cumFrKl.gt.0.95) cumFrKl=1.0
		endif
			if(ii.eq.2.or.ii.eq.3) then !pof,ppf
				capc=445.0
				if(idetr(llm).eq.0.and.ihydr.eq.0) then
					bcfpf=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.1.and.ihydr.eq.0) then
					bcfpf=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.0.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						bcfpf=0.3663*(akow**0.752)*(dissno+0.01)
						akbF=fracLip(llm)*wtod*akow*(dissno+0.01)
					else
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						bcfpf=0.3663*(akow**0.752)*(dissno+0.01)
						akbF=fracLip(llm)*wtod*akow*(dissno+0.01)
					endif
				elseif(idetr(llm).eq.1.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						corrIon=0.1
						akom=1.38*(akow**0.82)*dissno
     .					+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfpf=fracLip(llm)*akom*(dissno+0.01)/frcdetr
						akbF=fracLip(llm)*wtod*akow*(dissno+0.01)
					else
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						corrIon=0.01
						akom=1.38*(akow**0.82)*dissno
     .					+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfpf=fracLip(llm)*akom*(dissno+0.01)/frcdetr
						akbF=fracLip(llm)*wtod*akow*(dissno+0.01)
					endif
				endif
				aILC50=bcfpf*pclc50(llm)
				if(ik2.eq.0) then
					if(ibarb.eq.1) then
						elemr(llm)=capc*(wetwt(llm)**(-0.197))/
     .						(fracLip(llm)*akow)
					else
						if(wetwt(llm).lt.5.0) then
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.065*(wetwt(llm)**rb(llm))/fracLip(llm)
						else
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.116*(wetwt(llm)**rb(llm))/fracLip(llm)
						endif
					endif
				endif
				aLCInf=aILC50*(1.0-exp(-elemr(llm)*xiday(llm)))
				if(xiday(llm).gt.(4.605/elemr(llm))) then
					cLethal=aLCInf
				else
					cLethal=aLCInf/(1.0-exp(-elemr(llm)*xiday(llm)))
				endif
				cInt=bcfpf*conc*(1.0-exp(-(0.5*elemr(llm))))
				cumFrKl=1.0-
     .				exp(-(cInt/cLethal)**(1.0/shpfct))
				if(cumFrKl.gt.0.95) cumFrKl=1.0
			endif
			if(ii.eq.4) then !pelagic bacteria
				capc=890.0
				if(idetr(llm).eq.0.and.ihydr.eq.0) then
					bcfpb=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.1.and.ihydr.eq.0) then
					bcfpb=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.0.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						bcfpb=0.3663*(akow**0.752)*(dissno+0.01)
					else
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						bcfpb=0.3663*(akow**0.752)*(dissno+0.01)
					endif
				elseif(idetr(llm).eq.1.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						corrIon=0.1
						akom=1.38*(akow**0.82)*dissno
     .					+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfpb=fracLip(llm)*akom*(dissno+0.01)/frcdetr
					else
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						corrIon=0.01
						akom=1.38*(akow**0.82)*dissno
     .					+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfpb=fracLip(llm)*akom*(dissno+0.01)/frcdetr
					endif
				endif
				aILC50=bcfpb*pclc50(llm)
				if(ik2.eq.0) then
					if(ibarb.eq.1) then
						elemr(llm)=capc*(wetwt(llm)**(-0.197))/
     .						(fracLip(llm)*akow)
					else
						if(wetwt(llm).lt.5.0) then
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.065*(wetwt(llm)**rb(llm))/fracLip(llm)
						else
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.116*(wetwt(llm)**rb(llm))/fracLip(llm)
						endif
					endif
				endif
				aLCInf=aILC50*(1.0-exp(-elemr(llm)*xiday(llm)))
				if(xiday(llm).gt.(4.605/elemr(llm))) then
					cLethal=aLCInf
				else
					cLethal=aLCInf/(1.0-exp(-elemr(llm)*xiday(llm)))
				endif
				cInt=bcfpb*conc*(1.0-exp(-(0.5*elemr(llm))))
				cumFrKl=1.0-
     .				exp(-(cInt/cLethal)**(1.0/shpfct))
				if(cumFrKl.gt.0.95) cumFrKl=1.0
			endif
			if(ii.eq.5) then !benthic invertebrates
				capc=890.0
				if(idetr(llm).eq.0.and.ihydr.eq.0) then
					bcfbi=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.1.and.ihydr.eq.0) then
					bcfbi=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.0.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						bcfbi=0.3663*(akow**0.752)*(dissno+0.01)
					else
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						bcfbi=0.3663*(akow**0.752)*(dissno+0.01)
					endif
				elseif(idetr(llm).eq.1.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						corrIon=0.1
						akom=1.38*(akow**0.82)*dissno
     .					+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfbi=fracLip(llm)*akom*(dissno+0.01)/frcdetr
					else
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						corrIon=0.01
						akom=1.38*(akow**0.82)*dissno
     .					+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfbi=fracLip(llm)*akom*(dissno+0.01)/frcdetr
					endif
				endif
				aILC50=bcfbi*pclc50(llm)
				if(ik2.eq.0) then
					if(ibarb.eq.1) then
						elemr(llm)=capc*(wetwt(llm)**(-0.197))/
     .						(fracLip(llm)*akow)
					else
						if(wetwt(llm).lt.5.0) then
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.065*(wetwt(llm)**rb(llm))/fracLip(llm)
						else
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.116*(wetwt(llm)**rb(llm))/fracLip(llm)
						endif
					endif
				endif
				aLCInf=aILC50*(1.0-exp(-elemr(llm)*xiday(llm)))
				if(xiday(llm).gt.(4.605/elemr(llm))) then
					cLethal=aLCInf
				else
					cLethal=aLCInf/(1.0-exp(-elemr(llm)*xiday(llm)))
				endif
				cInt=bcfbi*conc*(1.0-exp(-(0.5*elemr(llm))))
				cumFrKl=1.0-
     .				exp(-(cInt/cLethal)**(1.0/shpfct))
				if(cumFrKl.gt.0.95) cumFrKl=1.0
			endif
			if(ii.eq.6.or.ii.eq.7) then !bof,bpf
				capc=445.0
				if(idetr(llm).eq.0.and.ihydr.eq.0) then
					bcfbf=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.1.and.ihydr.eq.0) then
					bcfbf=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.0.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						bcfbf=0.3663*(akow**0.752)*(dissno+0.01)
						akbF=fracLip(llm)*wtod*akow*(dissno+0.01)
					else
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						bcfbf=0.3663*(akow**0.752)*(dissno+0.01)
						akbF=fracLip(llm)*wtod*akow*(dissno+0.01)
					endif
				elseif(idetr(llm).eq.1.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						corrIon=0.1
						akom=1.38*(akow**0.82)*dissno
     .						+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfbf=fracLip(llm)*akom*(dissno+0.01)/frcdetr
						akbF=fracLip(llm)*wtod*akow*(dissno+0.01)
					else
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						corrIon=0.01
						akom=1.38*(akow**0.82)*dissno
     .						+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfbf=fracLip(llm)*akom*(dissno+0.01)/frcdetr
						akbF=fracLip(llm)*wtod*akow*(dissno+0.01)
					endif
				endif
				aILC50=bcfbf*pclc50(llm)
				if(ik2.eq.0) then
					if(ibarb.eq.1) then
						elemr(llm)=capc*(wetwt(llm)**(-0.197))/
     .						(fracLip(llm)*akow)
					else
						if(wetwt(llm).lt.5.0) then
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.065*(wetwt(llm)**rb(llm))/fracLip(llm)
						else
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.116*(wetwt(llm)**rb(llm))/fracLip(llm)
						endif
					endif
				endif
				aLCInf=aILC50*(1.0-exp(-elemr(llm)*xiday(llm)))
				if(xiday(llm).gt.(4.605/elemr(llm))) then
					cLethal=aLCInf
				else
					cLethal=aLCInf/(1.0-exp(-elemr(llm)*xiday(llm)))
				endif
				cInt=bcfbf*conc*(1.0-exp(-(0.5*elemr(llm))))
				cumFrKl=1.0-
     .				exp(-(cInt/cLethal)**(1.0/shpfct))
				if(cumFrKl.gt.0.95) cumFrKl=1.0
			endif
			if(ii.eq.8) then !benthic bacteria
				capc=890.0
				if(idetr(llm).eq.0.and.ihydr.eq.0) then
					bcfbb=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.1.and.ihydr.eq.0) then
					bcfbb=10.0**(0.7520*alogkow-0.4362)*wtod
				elseif(idetr(llm).eq.0.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						bcfbb=0.3663*(akow**0.752)*(dissno+0.01)
					else
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						bcfbb=0.3663*(akow**0.752)*(dissno+0.01)
					endif
				elseif(idetr(llm).eq.1.and.ihydr.eq.1) then
					if(iab.eq.0) then
						dissno=1.0/(1.0+10.0**(pKa-pH))
						akow=10.0**alogkow
						corrIon=0.1
						akom=1.38*(akow**0.82)*dissno
     .						+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfbb=fracLip(llm)*akom*(dissno+0.01)/frcdetr
					else
						dissno=1.0/(1.0+10.0**(pH-pKa))
						akow=10.0**alogkow
						corrIon=0.01
						akom=1.38*(akow**0.82)*dissno
     .						+(1.0+dissno)*corrIon*1.38*(akow**0.82)
						bcfbb=fracLip(llm)*akom*(dissno+0.01)/frcdetr
					endif
				endif
				aILC50=bcfbb*pclc50(llm)
				if(ik2.eq.0) then
					if(ibarb.eq.1) then
						elemr(llm)=capc*(wetwt(llm)**(-0.197))/
     .						(fracLip(llm)*akow)
					else
						if(wetwt(llm).lt.5.0) then
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.065*(wetwt(llm)**rb(llm))/fracLip(llm)
						else
							elemr(llm)=0.536*alogkow*alog10(dissno)+
     .						 0.116*(wetwt(llm)**rb(llm))/fracLip(llm)
						endif
					endif
				endif
				aLCInf=aILC50*(1.0-exp(-elemr(llm)*xiday(llm)))
				if(xiday(llm).gt.(4.605/elemr(llm))) then
					cLethal=aLCInf
				else
					cLethal=aLCInf/(1.0-exp(-elemr(llm)*xiday(llm)))
				endif
				cInt=bcfbb*conc*(1.0-exp(-(0.5*elemr(llm))))
				cumFrKl=1.0-
     .				exp(-(cInt/cLethal)**(1.0/shpfct))
				if(cumFrKl.gt.0.95) cumFrKl=1.0
			endif
			frrst=b_0/bc(ii,j)
			if(frrst.gt.1.0) frrst=1.0
			if(iday.eq.1.and.istep.eq.1) then
				frkldc(ii,j)=0.0
			else
				frkldc(ii,j)=
     .				amax1(0.0,cumFrKl-frkldc(ii,j))
			endif
			psnc=frrst*bc(ii,j)*frkldc(ii,j)
     .			+(1.0-frrst)*bc(ii,j)*cumFrKl
			afgrowth=effc(llm)/pclc50(llm)
			afrepro=effc(llm)/pclc50(llm)
			redgr=1.0-exp(-((cInt/(cLethal*afgrowth))**(1/shpfct)))
			redre=1.0-exp(-((cInt/(cLethal*afrepro))**(1/shpfct)))
		  endif
		  redcg=cgain*0.2*redgr   ! reduction in consumption gain: gC/m2/d (from cns subroutine)
c
		  bc(ii,j)=bc(ii,j)-psnc*delta_t-redcg
      endif 
c
      if(bcflag(ii,j).eq.0)bc(ii,j)=0. 

      if(idebug.eq.2 .and. bcflag(ii,j).eq.1)
     .  write(177,4002)i,ii,j,b_0,cgain,crsp,xm,drift,emerg,
     .                delc,bc(ii,j)
4002  format(1x,i3,2(1x,i2),2(2x,e8.3),6(2x,e9.3))      
c
4003  format('Consumer check'/
     .       i3,1x,i3,2(1x,i2),3(1x,e8.3),1x,f5.3,5(1x,e9.3))      
c
c         
          ss(ii+3)=ss(ii+3)+bc(ii,j)        ! update trophic levels
          kspp=(ii-1)*5+j+40                ! species tag
c
          if(nr.eq.0)bmass(kspp,i)=bc(ii,j) ! reference biomass vs time array
          if(nr.eq.0)rmass(ipop,i)=bc(ii,j) ! reference biomass vs time plot array
          if(nr.eq.1)tmass(ipop,i)=bc(ii,j) ! reference biomass vs time plot array
c
          dlyprey(ipop,i)=bc(ii,j)          ! daily output biomass   
c
c     used to accumulate organic matter state variables - gC/m2, later converted to mg/L
	  If(itoxmdl.eq.1) then !sublethal effect from AQUATOX
		egn(k1)=egn(k1)*(1.0+0.2*redgr)
	  endif
        if(k1.le.20)sfe=amax1(0.,sfe+(egn(k1)))             !egestion  - epi
        if(k1.le.20)sue=amax1(0.,sue+(tux(k1)))             !excretion - epi
        if(k1.le.20)sme=amax1(0.,sme+(xm))                  !mortality  - epi
c     sum of epil respiration        
        if(k1.le.20)sre=amax1(0.,sre+(crsp+trsdax(k1)))     !respiration - epi
c
        if(k1.gt.20)sfh=amax1(0.,sfh+(egn(k1)))             !egestion  - hypo
        if(k1.gt.20)suh=amax1(0.,suh+(tux(k1)))             !excretion - hypo
        if(k1.gt.20)smh=amax1(0.,smh+(xm))                  !mortality - hypo
c     sum of hypol respiration        
        if(k1.gt.20)srh=amax1(0.,srh+(crsp+trsdax(k1)))     !respiration - hypo
c
12342 continue 
12343 continue
c
c     IMPORTANT: CASM revised decomposition calculations
c     Decomposition - temperature dependent
c     labile DOCe rate - epil
      dcle=delta_t*rcle*ht(wte,tcle_o,tcle_m)      
c     refractory POCe rate - epil
      dcre=delta_t*rcre*ht(wte,tcre_o,tcre_m)
c     labile DOCh rate - hypol
      dclh=delta_t*rclh*ht(wth,tclh_o,tclh_m)      
c     refractory POCh rate - hypol
      dcrh=delta_t*rcrh*ht(wth,tcrh_o,tcrh_m)
c
c     Epil DOC, om(1,1)
      zom11=om(1,1)
      om(1,1)=amax1(0.0,om(1,1)+
     .        (sue*cnv)+dcre*om(1,2)-dcle*om(1,1)-cl(81)*cnv)
      if(idebug.eq.2)then
      write(177,*)' '
      write(177,5001)
5001  format('CASM  Decomposition DOCe'/
     .' Day  DOCe(t)    SUE        CNV        dcre       ',
     .'POCe       dcle       DOCe(t-1)  cl(81)')
      write(177,5002)iday,om(1,1),sue,cnv,dcre,om(1,2),
     .               dcle,zom11,cl(81)
5002  format(i4,1x,8(1x,e10.4))  
      endif
c
c     Epil POC, om(1,2)
      zom12=om(1,2)
      om(1,2)=amax1(0.0,om(1,2)+((sfe+sme)*cnv)-dcre*om(1,2)
     .              -delta_t*stl*om(1,2)-cl(82)*cnv)
      if(idebug.eq.2)then
      write(177,*)' '
      write(177,5003)
5003  format('CASM  Decomposition POCe'/
     .' Day  POCe(t)    SFE        SME        CNV        dcre       ',
     .'POCe(t-1)  delta_t    stl        POCe(t-1)  cl(82)')
      write(177,5004)iday,om(1,2),sfe,sme,cnv,dcre,zom12,
     .     delta_t,stl,zom12,cl(82)
5004  format(i4,1x,10(1x,e10.4))  
      endif
c
c     Hypol DOC, om(2,1)
      zom21=om(2,1)
      om(2,1)=amax1(0.0,om(2,1)+(suh*cnv)+dcrh*om(2,2)-dclh*om(2,1)
     .              -cl(83)*cnv)
      if(idebug.eq.2)then
      write(177,*)' '
      write(177,5005)
5005  format('CASM  Decomposition DOCh'/
     .' Day  DOCh(t)    SUH        CNV        dcrh       ',
     .'POCh       dclh       DOCh(t-1)  cl(83)')
      write(177,5006)iday,om(2,1),suh,cnv,dcrh,om(2,2),
     .               dclh,zom21,cl(83)
5006  format(i4,1x,8(1x,e10.4))  
      endif
c
c     Hypol POC, om(2,2)
      zom22=om(2,2)
      om(2,2)=amax1(0.0,om(2,2)+((sfh+smh)*cnv)
     .              +delta_t*stl*om(1,2)-dcrh*om(2,2)-cl(84)*cnv) 
      if(idebug.eq.2)then
      write(177,*)' '
      write(177,5007)
5007  format('CASM  Decomposition POCh'/
     .' Day  POCh(t)    SFH        SMH        CNV        dcrh       ',
     .'POCh(t-1)  delta_t    stl        POCe(t-1)  cl(84)')
      write(177,5008)iday,om(2,2),sfh,smh,cnv,dcrh,zom22,
     .     delta_t,stl,om(1,2),cl(84)
5008  format(i4,1x,10(1x,e10.4))  
      endif
c
      if(ncycle.eq.1) then
c     Add decomposed labile material to nutrient pools (mg/L)
      omx=dcle*om(1,1)
       ep=ep+omx*fcp
       en=en+omx*fcn
       es=es+omx*fcs
c
      omx=dclh*om(2,1)
       hp=hp+omx*fcp
       hn=hn+omx*fcn
       hs=hs+omx*fcs
c
c     Dissolved oxygen cost of decomposition (mg/L)
      edo=amax1(0.0,edo-(dcle*om(1,1)+dcre*om(1,2))*(fcdo))
      hdo=amax1(0.0,hdo-(dclh*om(2,1)+dcrh*om(2,2))*(fcdo))
      edo=amax1(0.0,edo-sre*fcdo*cnv)        ! consumer respiration (mgO2/L)
      hdo=amax1(0.0,hdo-srh*fcdo*cnv)        ! demand on oxygen     (mgO2/L)
c
c     add consumer excretion to nutrients (mg/L)
      ep=amax1(0.0,ep+sue*fcp*cnv)      ! eplimnetic   P, N, Si (mg/L)
      en=amax1(0.0,en+sue*fcn*cnv)      
      es=amax1(0.0,es+sue*fcs*cnv) 
      hp=amax1(0.0,hp+suh*fcp*cnv)      ! hypolimnetic P, N, Si (mg/L)
      hn=amax1(0.0,hn+suh*fcn*cnv)
      hs=amax1(0.0,hs+suh*fcs*cnv)
c
c
c     mix if wte = wth
      if(wte.eq.wth)then 
        ep=(ep+hp)/2.
	  hp=ep
        en=(en+hn)/2.
	  hn=en
        es=(es+hs)/2.
	  hs=es
        edo=(edo+hdo)/2.
	  hdo=edo
        om(1,1)=(om(1,1)+om(2,1))/2.
	  om(2,1)=om(1,1)
        om(1,2)=(om(1,2)+om(2,2))/2.
	  om(2,2)=om(1,2)
      endif !mixing
c
      endif !nutrient cycling
c
c
      if(nr.eq.0)then
         znut(i,1)=en         !(mg/L)
         znut(i,2)=ep
         znut(i,3)=edo
         znut(i,4)=es
         znut(i,5)=hn
         znut(i,6)=hp
         znut(i,7)=hdo
         znut(i,8)=hs
         znut(i,9) =om(1,1)
         znut(i,10)=om(1,2)
         znut(i,11)=om(2,1)
         znut(i,12)=om(2,2)
c 
         dlyenv(1) =en
         dlyenv(2) =ep
         dlyenv(3) =es
         dlyenv(4) =edo
         dlyenv(5) =hdo
         dlyenv(6) =om(1,1)   !(g C/m3)
         dlyenv(7) =om(1,2)
         dlyenv(8) =om(2,1)
         dlyenv(9) =om(2,2)*z !(g C/m2)
         dlyenv(11)=hn
         dlyenv(12)=hp
         dlyenv(13)=hs
		 endif !reference nutrient reporting     
c
c
      if(nr.eq.1)then
         tnut(i,1)=en         !(mg/L)
         tnut(i,2)=ep
         tnut(i,3)=edo
         tnut(i,4)=es
         tnut(i,5)=hn
         tnut(i,6)=hp
         tnut(i,7)=hdo
         tnut(i,8)=hs
         tnut(i,9)=om(1,1)
         tnut(i,10)=om(1,2)
         tnut(i,11)=om(2,1)
         tnut(i,12)=om(2,2)
c
         dlyenv(1) =en
         dlyenv(2) =ep
         dlyenv(3) =es
         dlyenv(4) =edo
         dlyenv(5) =hdo
         dlyenv(6) =om(1,1)   !(g C/m3)
         dlyenv(7) =om(1,2)
         dlyenv(8) =om(2,1)
         dlyenv(9) =om(2,2)*z !(g C/m2)
         dlyenv(11)=hn
         dlyenv(12)=hp
         dlyenv(13)=hs
		 endif !treatment nutrient reporting     
c
12344 continue !nperday loop
c
      if(ncycle.eq.1)then
c     define upstream boundary condition for as assumed identical reach
      ubc_ep=ep
      ubc_en=en
      ubc_es=es
      ubc_hp=hp
      ubc_hn=hn
      ubc_hs=hs
      ubc_edo=edo
      ubc_hdo=hdo
      ubc_om11=om(1,1)
      ubc_om12=om(1,2)
      ubc_om21=om(2,1)
      ubc_om22=om(2,2)

c     adjust for advection
       ep=adv(i)*ubc_ep - adv(i)*ep  ! eplimnetic   P, N, Si (mg/L)
       en=adv(i)*ubc_en - adv(i)*en      
       es=adv(i)*ubc_es - adv(i)*es
c
       hp=adv(i)*ubc_hp - adv(i)*hp  ! hypolimnetic P, N, Si (mg/L)
       hn=adv(i)*ubc_hn - adv(i)*hn
       hs=adv(i)*ubc_hs - adv(i)*hs
       edo=adv(i)*ubc_edo - adv(i)*edo ! epilimnetic DO  (mg/L)
       hdo=adv(i)*ubc_hdo - adv(i)*hdo ! hypolimnetic DO (mg/L)
      endif !nutrient advection
c
      dlyenv(10)=tis(iday)
      call CASMday_prey(kunit,iday)
12345 continue !daily loop
c 
c     adjust for advection
      om(1,1)=amax1(0.,adv(i)*ubc_om11 - adv(i)*om(1,1))
      om(1,2)=amax1(0.,adv(i)*ubc_om12 - adv(i)*om(1,2))
      om(2,1)=amax1(0.,adv(i)*ubc_om21 - adv(i)*om(2,1))
c
      return
      end
c
c
c     This subroutine calculates the feeding rates of all 
c     consumer populations in the casm_px model using the second
c     order feeding equation derived by DeAngelis et al. 1975.
      Subroutine cns(dt,te,th,iday)
      implicit real*4(a-h,o-z)
      common/p/  bp(4,10),   !initial biomass
     .           ps(4,10),   !max photosyhnethetic rate
     .         pte1(4,10),pxk1(4,10),   !Thornton-Lessem temperature parameters   
     .         pte2(4,10),pxk2(4,10),   
     .         pte3(4,10),pxk3(4,10),   
     .         pte4(4,10),pxk4(4,10),   
     .           si(4,10),  !Light saturation intensity 
     .          snk(4,10),  !Sinking rate
     .          xkp(4,10),xkn(4,10),xks(4,10), !Michaelis-Menten constants for P, N, and Si 
     .          pm(4,10),   !Mortality rate
     .        resp(4,10),   !Photorespiration rate
     .        resd(4,10),   !Dark respiration rate
c
c             Habitat factors for salinity, current velocity, and depth
     .        psplt(4,10),pspol(4,10),pspou(4,10),psput(4,10),  !salinity
     .        ppvlt(4,10),ppvol(4,10),ppvou(4,10),ppvut(4,10),  !velocity
     .        ppdlt(4,10),ppdol(4,10),ppdou(4,10),ppdut(4,10),  !depth
c             Phenology         
     .         ddi(4,10),   !Degree-days required for growth initiation
     .         dds(4,10),   !Degree-days for onset of senescence
     .              xlk
c
      common/c/  bc(8,5),  !Initial biomass 
     .          cmx(8,5),  !max consumption rate
     .         cte1(8,5),cxk1(8,5),   !Thornton-Lessem temperature parameters   
     .         cte2(8,5),cxk2(8,5),   
     .         cte3(8,5),cxk3(8,5),   
     .         cte4(8,5),cxk4(8,5),   
     .         rsda(8,5),    rs(8,5),           !specific dynamic action rate, respiration rate
     .          tro(8,5),   trm(8,5),           !temperature dependence of respiration
     .            f(8,5),     u(8,5),  cm(8,5), !egestion, excretion, mortality rates
     .          dft(8,5),                       !invertebrate drift, 1/d 
c
c             Habitat factors for dissolved oxygen, salinity, current velocity, and depth
     .         DOlt(8,5), DOol(8,5), DOou(8,5), DOut(8,5),  !dissolved oxygen
     .        psclt(8,5),pscol(8,5),pscou(8,5),pscut(8,5),  !salinity
     .        ccvlt(8,5),ccvol(8,5),ccvou(8,5),ccvut(8,5),  !velocity
     .        ccdlt(8,5),ccdol(8,5),ccdou(8,5),ccdut(8,5),  !depth
     .             bxkn,      bxkp
c
      common/arg1/om(2,2),yo(40),egn(40),cl(84),tux(40),trsdax(40)
      common/cl/w(84,40),a(84,40),he(84,40)
      common/env2/degdays(365)
c
      dimension pry(84),prd(40),c(84,40),tyo(40),
     .          trsda(40),tu(40),sm(84),tempc(40),c_base(40)
c                                  
      do i=1,84
         pry(i)=0.
         cl(i)=0.
         sm(i)=0.
         do j=1,40
            c(i,j)=0.
            prd(j)=0.        ! initialize
            tyo(j)=0.
            trsda(j)=0.
            tu(j)=0.
            yo(j)=0.
            egn(j)=0.
            tux(j)=0.
            trsdax(j)=0.
          enddo
      enddo
c
c     plant food array
      nn=0
      do i=1,4
         do j=1,10
         nn=nn+1
c        phenology check: if plant growth not yet initiated, it is not available for grazing
         phen=1.0
	   if(degdays(iday).lt.ddi(i,j))phen=0. 
         pry(nn)=bp(i,j)*phen
         enddo
      enddo
c
c     consumer prey and predator arrays
      nn=0
      do i=1,8
         do j=1,5
         nn=nn+1
         nnn=nn+40
         pry(nnn)=bc(i,j)
         prd(nn)= bc(i,j)
           if(bc(i,j).gt.0.)then
              tyo(nn)  =dt*cmx(i,j)
              trsda(nn)=rsda(i,j)
              tu(nn)   =u(i,j)
c          temperature effects on consumption
           te1=cte1(i,j) !Thornton-Lessem input parameters
           te2=cte2(i,j)
           te3=cte3(i,j)
           te4=cte4(i,j)
c	    
           xk1=cxk1(i,j)
           xk2=cxk2(i,j)
           xk3=cxk3(i,j)
           xk4=cxk4(i,j)
c
           if(i.le.4)   ! epil. temperature function (unitless)
     .       tempc(nn)=ht2(te1,xk1,te2,xk2,te3,xk3,te4,xk4,te) !temperature modifier
           if(i.gt.4)   ! hypol.temperature function (unitless)
     .       tempc(nn)=ht2(te1,xk1,te2,xk2,te3,xk3,te4,xk4,th) !temperature modifier
c
          endif
         enddo
      enddo
c
      pry(81)=om(1,1)
      pry(82)=om(2,1)   ! POM epi
      pry(83)=om(1,2)
      pry(84)=om(2,2)
c
c     loop over all consumers and decomposers
      do j=1,40
      d=0.
         do i=1,84
           d=d+w(i,j)*(pry(i)-0.02*pry(i))
         enddo
      d=d+prd(j)
         do i=1,84
           ci=tempc(j)*tyo(j)*prd(j)*w(i,j)*he(i,j)*
     .        (pry(i)-0.020*pry(i))
           if(d.gt.0.) c(i,j)=ci/d
         enddo
      enddo
c
      do i=1,84              ! check for prey conservation
      sm(i)=0.
         do j=1,40
           sm(i)=sm(i)+c(i,j)  
         enddo
      enddo
c
c
      do i=1,84
        if(sm(i).gt.0.)then
c	   
        if(sm(i).gt.pry(i))then
          do j=1,40
            c(i,j)=c(i,j)/sm(i)*(pry(i)-0.020*pry(i))
          enddo
        endif
c
      sm(i)=0.
         do j=1,40
           sm(i)=sm(i)+c(i,j)  
         enddo
c
      if(sm(i).gt.pry(i)) print*,'warning conservation ',
     .sm(i),pry(i),i
c
         do j=1,40                                 ! pathways:
           cl(i)=cl(i)+c(i,j)
           tux(j)=tux(j)+c(i,j)*tu(j)              ! excretion
           trsdax(j)=trsdax(j)+c(i,j)*trsda(j)     ! dynamic action
           c(i,j)=c(i,j)-c(i,j)*(tu(j)+trsda(j))   ! adjust consumption
           yo(j)=yo(j)+c(i,j)*a(i,j)               ! assimilation
           egn(j)=egn(j)+(1.-a(i,j))*c(i,j)        ! egestion
         enddo
c
      endif !sm() check
c
      enddo !do i=1,84 loop
c
      return
      end
c
c
c
c     This function modifies physiological maximum rates
c     of photosynthesis and respiration in relation
c     to temperature deviations from the optimal
c     growth temperature defined for each CASM population
      Function ht(t,topt,tmax)
      implicit real*4(a-h,o-z)
      v=(tmax-t)/(tmax-topt)
      if(v.gt.0.) goto 31
      ht=0.001
      return
31    ht=(v**1.558)*exp(1.558*(1.0-v))
      return
      end
c
c
c     Thornton-Lessem (1978) temperature function
      Function ht2(te1,xk1,te2,xk2,te3,xk3,te4,xk4,T)
      implicit real*4(a-h,o-z)
      tt7=1.0/(te4-te3)
      tt5=1.0/(te2-te1)
c
      t5=tt5*alog(xk2*(1.0-xk1)/(0.02*xk1))
      t7=tt7*alog(xk3*(1.0-xk4)/(0.02*xk4))
      t4=exp(t5*(T-te1))
      t6=exp(t7*(te4-T))
c
      gcta=(xk1*t4)/(1.0+xk1*(t4-1.0))
      gctb=(xk4*t6)/(1.0+xk4*(t6-1.0))
c
      ht2=gcta*gctb
      return
      end
c
c
c     This trapezoidal function modifies growth in relation
c     to habitat parameters: depth, velocity, salinity, dissolved oxygen 
      Function tz(x1,x2,x3,x4,x)
      implicit real*4(a-h,o-z)
c
      tz=1.0
      if(x4.eq.0.)return !check to see if factor is applicable (x4=0 in data implies x1=x2=x3=x4=0)
c
c     habitat parameter lies outside feasible range
      tz=0.
      if(x.le.x1)return
      if(x.ge.x4)return
c
c     habitat parameter lies within optimal range
      tz=1.0
      if(x.ge.x2 .and. x.le.x3)return
c
c     habitat parameter is less than optimal, but feasible
      if(x.gt.x1 .and. x.lt.x2)
     .   tz=(x-x1)/(x2-x1) 
c     habitat parameter is greater than optimal, but feasible
      if(x.gt.x3 .and. x.lt.x4)
     .   tz=1.0 - ((x-x3)/(x4-x3))
      return
      end
c
c       
c     Function for calculating plant biomass component of light extinction
      Function exBIO(nsys,ii,z,z1,z2,z3,z4)
      implicit real*4(a-h,o-z)
      data exPHYT/0.100/,   !0.10 used originally
     .     exPERI/0.125/,   !0.05
     .     exMACR/0.100/,   !0.10
     .     exEMRG/0.150/
c
      if(nsys.eq.1)then   !for general stream, first two plant guilds are periphyton
      exPHYT=exPERI
       if(ii.eq.1)exBIO=(exPHYT*z1 + exPERI*z2 + exMACR*z3
     .	                + exEMRG*z4)/z
       if(ii.eq.2)exBIO=(exPHYT*z1 + exPERI*z2 + exMACR*z3
     .                  + exEMRG*z4)/z
       if(ii.eq.3)exBIO=(                        exMACR*z3
     .                  + exEMRG*z4)/z
       if(ii.eq.4)exBIO=0.
      endif
c     
      if(nsys.ne.1)then
       if(ii.eq.1)exBIO=(exPHYT*z1 +             exMACR*z3)/z
       if(ii.eq.2)exBIO=(exPHYT*z1 + exPERI*z2 + exMACR*z3
     .                  + exEMRG*z4)/z
       if(ii.eq.3)exBIO=(exPHYT*z1 +             exMACR*z3
     .                  + exEMRG*z4)/z
       if(ii.eq.4)exBIO=0.
      endif
c 
	return
	end
c
c
c     Function to calculate the saturation concentration (mg/L) for dissolved oxygen
c     as a function of temperature adapted from Park and Clough 2004
      Function O2sat(t)
      implicit real*4(a-h,o-z)
      tK=273.15+t !Kelvin temperature
c
      a1=-173.4927
      a2=24963.39/tK
      a3=143.3482*alog(tK/100.)
      a4=-0.21849*tK
c
      O2sat=1.4277*exp(a1+a2+a3+a4)
c
      return
      end
c
c
c     Subroutine to calculate reaeration coefficient for streams as f(temp,depth,velocity)
c     Adapted from Park and Clough 2004
      Subroutine reaer(t,d,v,xkrT)
      implicit real*4(a-h,o-z)
c      t= temp (C)
c      d=mean depth (m)
c      v=current velocity (m/s)
c      xkr=reaeration coefficient (1/d)
      if(v.lt. 0.518)  td=0.
      if(v.ge. 0.518)  td=4.411*(v**2.9135)
      if(d.le.td)      xkr=5.049*(v**0.97)*(d**(-1.67))
      if(d.gt.td)      xkr=3.930*(v**0.50)*(d**(-1.50))
      if(d.lt.0.61)    xkr=5.349*(v**0.67)*(d**(-1.85))
      if(t.lt.3.0)     xkr=0. !zero value under ice cover
      xkrT=xkr*1.024**(t-20.) !temperature-dependence of reaeration
c
      return
      end
c
c
c     Subroutine to approximate periphyton sloughing due to current velocity
      Subroutine slough(iday,xslf)
      implicit real*4 (a-h,o-z)
      common/env/ein(365),te(365),th(365),xpe(365),xne(365),xse(365),
     .           depth(365),cvel(365),tis(365),POCe(365),wind(365),
     .           salinity(365)      
      common/slgh/cv_mean,cv_stdev
c
      xslf=0.
      stdev2=cv_mean+(2.0*cv_stdev) !trigger is 2 standard deviations greater than mean current velocity
      if(cvel(iday) .le. stdev2) return
c
      stdev3=cv_mean+(3.0*cv_stdev)
      prop=(cvel(iday)-stdev2)/(stdev3-stdev2)
      if(prop.gt.1.0) prop=1.0   
      xslf=exp(prop-1.0)
c
	  return
      end
c
c
c     Subroutine to estimate daily advective loss for lotic system applications 
      Subroutine advect(dt)
      implicit real*4 (a-h,o-z)
      common/env/ein(365),te(365),th(365),xpe(365),xne(365),xse(365),
     .           depth(365),cvel(365),tis(365),POCe(365),wind(365),
     .           salinity(365)      
      common/adv/rlngth,rwdth,rslope
      common/cadv/adv(365)
c
c     calculate daily fraction of reach volume advected
      rvol=0.
      advol=0.
c
      do i=1,365
      rvol=rlngth*rwdth*depth(i)          !m3
      advol=dt*cvel(i)*rwdth*depth(i)*86400. !m3/time step
      adv(i)=amin1(1.00,(advol/rvol))      !1/d, not to exceed 1.0 (avoids pseudo-dispersion)
      enddo
c
      return
      end
c
c
      Subroutine getTEF(kday)
      implicit real*4(a-h,o-z)
      common/csm4/npops
      common/dlytx/dtef(160,365)
      common/micfac/cfact(80),cmort(80)
c
	  kappa=npops
c	             
      do i=1,kappa
	   m=i+kappa
         cfact(i)=dtef(i,kday)   !daily toxic effects
         cmort(i)=0.
      enddo
      return
      end
c
c     Subroutine to calculate daily toxic effects factors
      Subroutine daytox
      implicit real*4(a-h,o-z)
c
      dimension fact(80),pmax(80)
      character chmnm*16, chemnm*16
      real*4 slpfct,shpfct,alogkow,pKa,pH,wtod,frcdetr
      integer iab,ihydr,ik2,lethal
      common/csm4/npops
      common/t/nsp(4),nsc(8)
      common/blkmic/xiday(80),enpt(80),effc(80),pclc50(80),idetr(80),
     .         fracLip(80),elemr(80),wetwt(80),q10(80),rb(80),uptrt(80),
     .              xnoec(80),envc(365),kappa
      common/blkmict/slpfct,shpfct,alogkow,iab,ihydr,ik2,pKa,pH,wtod,
     .          frcdetr,lethal,ibarb
      common/toxpar/itoxmdl,chemnm
      common/dlytx/dtef(160,365)
      common/dbg/idebug
      common/micfac/cfact(80),cmort(80)
c
      common/res1/isyndrome,islope
      common/res3/nplants
      common/res2/slope(80),xcept(80) !E-R function slopes and intercepts
c
      common/tef_q2/pmax_out(365,80) !for use in TEF QA subroutine
c
      common/p/  bp(4,10),   !initial biomass
     .           ps(4,10),   !max photosyhnethetic rate
     .         pte1(4,10),pxk1(4,10),   !Thornton-Lessem temperature parameters   
     .         pte2(4,10),pxk2(4,10),   
     .         pte3(4,10),pxk3(4,10),   
     .         pte4(4,10),pxk4(4,10),   
     .           si(4,10),  !Light saturation intensity 
     .          snk(4,10),  !Sinking rate
     .          xkp(4,10),xkn(4,10),xks(4,10), !Michaelis-Menten constants for P, N, and Si 
     .          pm(4,10),   !Mortality rate
     .        resp(4,10),   !Photorespiration rate
     .        resd(4,10),   !Dark respiration rate
c
c             Habitat factors for salinity, current velocity, and depth
     .        psplt(4,10),pspol(4,10),pspou(4,10),psput(4,10),  !salinity
     .        ppvlt(4,10),ppvol(4,10),ppvou(4,10),ppvut(4,10),  !velocity
     .        ppdlt(4,10),ppdol(4,10),ppdou(4,10),ppdut(4,10),  !depth
c             Phenology         
     .         ddi(4,10),   !Degree-days required for growth initiation
     .         dds(4,10),   !Degree-days for onset of senescence
     .              xlk
c
      common/c/  bc(8,5),  !Initial biomass 
     .            g(8,5),  !max consumption rate
     .         cte1(8,5),cxk1(8,5),   !Thornton-Lessem temperature parameters   
     .         cte2(8,5),cxk2(8,5),   
     .         cte3(8,5),cxk3(8,5),   
     .         cte4(8,5),cxk4(8,5),   
     .         rsda(8,5),    rs(8,5),           !specific dynamic action rate, respiration rate
     .          tro(8,5),   trm(8,5),           !temperature dependence of respiration
     .            f(8,5),     u(8,5),  cm(8,5), !egestion, excretion, mortality rates
     .          dft(8,5),                       !invertebrate drift, 1/d 
c
c             Habitat factors for dissolved oxygen, salinity, current velocity, and depth
     .         DOlt(8,5), DOol(8,5), DOou(8,5), DOut(8,5),  !dissolved oxygen
     .        psclt(8,5),pscol(8,5),pscou(8,5),pscut(8,5),  !salinity
     .        ccvlt(8,5),ccvol(8,5),ccvou(8,5),ccvut(8,5),  !velocity
     .        ccdlt(8,5),ccdol(8,5),ccdou(8,5),ccdut(8,5),  !depth
     .             bxkn,      bxkp
c
      data physmax/100./ !maximum rate multiplier
c
      np=4 !number of producer guilds
      nc=8 !number of consumer guilds
c
c
c     Calculate the daily effects factors
c
c     Initialize dtef() matrix
	  do j=1,160
	   do k=1,365
	     dtef(j,k)=0.
	   enddo
      enddo
c

      do kday=1,365 !begin loop for day k
c
         do i=1,npops
           cfact(i)=0.
           cmort(i)=0.
           fact(i)=0.0
	   enddo  
c
c     exposure concentration for day 
      conc=envc(kday)
	if(conc.gt.0.)then
c
c     log10 of daily exposure concentration
      x=alog10(conc)
c
	call rstor_b
	call rstor_p
c
c
c     CASM Producer Populations
c     calculate expected effect using exposure-response function and
c     daily exposure concentration
c
c     option for exposure-reponse functions

c     EC50-derived slopes, intercepts
      if(islope.eq.0)then
       do i=1,nplants
         pmax(i)=0.
c
         a=xcept(i) !intercept
         b=slope(i) !slope
c
         probits=0.
         if(conc.gt.0)probits=(b*x)+a
c      
         pz=probits
         if(effc(i).gt.0.) call invprob(pz,y2)
         pmax(i)=y2
         if(pmax(i).lt.0.)   pmax(i)=0.
         if(pmax(i).ge.1.0)  pmax(i)=0.9999  !avoids taking ln(0)...
c
       pmax_out(kday,i)=pmax(i)*100. !convert to percent
c
       enddo
      endif !EC-50 derived slopes, intercepts
c
c     EPA one-liner slopes and intercepts
      if(islope.eq.1)then
       do i=1,nplants
         pmax(i)=0.
c
         a=xcept(i) !intercept
         b=slope(i) !slope
c
         probits=0.
         if(conc.gt.0)probits=(b*x)+a
c      
         pz=probits
         if(effc(i).gt.0.) call invprob(pz,y2)
         pmax(i)=y2
         if(pmax(i).lt.0.)   pmax(i)=0.
         if(pmax(i).ge.1.0)  pmax(i)=0.9999  !avoids taking ln(0)...
c
       pmax_out(kday,i)=pmax(i)*100. !convert to percent
c
       enddo
      endif !EPA one-liner slopes and intercepts
c
c     Triangular E-R function
      if(islope.eq.2)then
       do i=1,nplants
         pmax(i)=0.
c
      z=1/slope(i) ! 1/slope
      x50=alog10(effc(i))
      s=slope(i)
c
      a1=x50-z
      a2=x50+z
c
      if(x.lt.a1)y=0.
      if(x.lt.a1)goto 12
c
      if(x.gt.a2)y=1.0
      if(x.gt.a2)goto 12
c
      if(x.gt.a1 .and. x.le.x50)
     .   y=0.5*(1.0+s*(x-x50))**2
c
      if(x.gt.x50 .and. x.lt.a2)
     .   y=1 - 0.5*(1.0-s*(x-x50))**2
c
12    continue
c
         pmax(i)=y
         if(pmax(i).lt.0.)   pmax(i)=0.
         if(pmax(i).ge.1.0)  pmax(i)=0.9999  !avoids taking ln(0)...
         pmax_out(kday,i)=pmax(i)*100. !convert to percent
c
       enddo
      endif !Triangular E-R functions
c
c     Piecewise linear E-R function ("hockey stick")
      if(islope.eq.3)then
       do i=1,nplants
         pmax(i)=0.
c
      x50=alog10(effc(i))
c
      a1=(slope(i)*x50-0.5)/slope(i)
      a2=(0.5+slope(i)*x50)/slope(i)
c
      if(x.le.a1)y=0.
      if(x.le.a1)goto 35
c
c
      if(x.gt.a1)
     .   y=(slope(i)*x)+xcept(i)
c
35    continue
c
         pmax(i)=y
         if(pmax(i).lt.0.)   pmax(i)=0.
         pmax_out(kday,i)=pmax(i)*100. !convert to percent
       enddo
c
      endif !Piece-wise E-R functions
c
c
c     calculate toxic effects factors
      is=0
      do ll=1,np
         kllm=nsp(ll)
	    do k=1,kllm
	      is=is+1
	      if(pmax(is).le.0.)goto 40
	      if(bp(ll,k).le.0.)goto 40
c
		  anab=0.
		  cat1=0.
		  cat2=0.    
c      calculate r_0: base growth rate units: 1/day     
          anab=ps(ll,k)                         !sum anabolic inputs
		  cat1=ps(ll,k)*resp(ll,k)              !photorespiration not impacted by TEF
		  cat2 = resd(ll,k)+snk(ll,k)+pm(ll,k)  !sum remaining catabolic losses impacted by TEF
          r_0=anab-(cat1+cat2)
c
c      expected biomass with no toxic chemicals
c           z1=bp(ll,k)*exp(r_0*xiday(is))
c           B_t=amax1(0.0,z1)
c
c      expected biomass with exposure to toxic chemical 
c           B_x=bp(ll,k) + ((1.0-pmax(is))*(B_t - bp(ll,k))) 
c           B_x=amax1(B_x,0.000001*bp(ll,k)) !permits effects >100%, but constrains B_x to be >0
c                                           !to avoid taking log of zero 
c
c      calculate r_x: growth rate reduced by toxic chemicals
c           r_x=(log(B_x)-log(bp(ll,k)))/xiday(is)
            r_x=r_0*(1.0-pmax(is)) !Russ Erickson/USEPA changes
c
c
c      calculate TEF using GSS
           if(isyndrome.eq.0)xf=(anab-cat1-cat2-r_x)/(anab+cat2)		 
c      calculate TEF using PSS
           if(isyndrome.eq.1)xf=(anab-cat1-cat2-r_x)/(anab)
c
c
c     if xf > 1, adjust xf to shift necessary effect to catabolic losses
c                use the resulting factor in genp to set photosynthesis to zero
c                and correspondingly adjust other loss parameters by the factor
           if(xf.gt.1.0) xf=(-cat1-cat2-r_x)/(cat2)
c
           dtef(is,kday)=amin1(physmax,xf)
c
40        continue
	    enddo
	  enddo
c
c
c     CASM Consumer Populations
c     estimate expected % reduction using CASM probit function, GSS,
c     and CASM toxicity scenarios
       i1=nplants+1
       do i=i1,npops
         pmax(i)=0.
c
         a=xcept(i) !CASM slope
         b=slope(i) !CASM intercept
c
         probits=0.
         if(conc.gt.0)probits=b*alog10(conc)+a
c
         if(effc(i).gt.0.) call invprob(probits,y2)
         pmax(i)=y2
         if(pmax(i).lt.0.)   pmax(i)=0.
         if(pmax(i).ge.1.0)  pmax(i)=0.99  !avoids taking ln(0)...
c
       pmax_out(kday,i)=pmax(i)*100. !convert to percent
c
       enddo
c
c     Calculate effects factors for consumer populations
         do ll=1,nc
            kllm=nsc(ll)
            do k=1,kllm
	       is=is+1
           if(bc(ll,k).le.0.) goto 180
	       if(pmax(is).le.0.) goto 180  
c
           anab=0.
           cat1=0.
           cat2=0.
c      calculate r_0: base growth rate      
           anab= g(ll,k)
           cat1= g(ll,k)*rsda(ll,k)
           cat2= u(ll,k)+rs(ll,k)+cm(ll,k)
           r_0=anab-(cat1+cat2)
c
c           if(r_0.le.0.) r_0=0.000001
c
c      expected biomass with no toxic chemicals
           z2=bc(ll,k)*exp(r_0*xiday(is))
           B_t=amax1(0.0,z2)
c
c      expected biomass with exposure to chemicals
           B_x=bc(ll,k) +((1.0-pmax(is))*(B_t-bc(ll,k))) 
c
c      calculate r_x: growth rate reduced by toxic chemicals
           r_x=(log(B_x)-log(bc(ll,k)))/xiday(is)
c
c      calculate fact(is,j)
           xf=(anab-cat1-cat2-r_x)/(anab+cat2)
c
c
c     if xf > 1, adjust xf to shift necessary effect to catabolic losses
c                use the resulting factor in genp to set consumption to zero
c                and correspondingly adjust other loss parameters by the factor
           if(xf.gt.1.0) xf=(-cat1-cat2-r_x)/(cat2)
c
           dtef(is,kday)=amin1(physmax,xf)
c                      
180   continue
	    enddo
	  enddo
c
        endif !daily dtef() calculation
      enddo   !daily dtef() loop
c
      return
      end 
c
c
c
      Subroutine genp(iday)
      implicit real*4(a-h,o-z)
      character chmnm*16, chemnm*16
      real*4 slpfct,shpfct,alogkow,pKa,pH,wtod,frcdetr
      integer iab,ihydr,ik2,lethal
      common/csm4/npops
      common/res1/isyndrome,islope
      common/t/nsp(4),nsc(8)
      common/p/  bp(4,10),   !initial biomass
     .           ps(4,10),   !max photosyhnethetic rate
     .         pte1(4,10),pxk1(4,10),   !Thornton-Lessem temperature parameters   
     .         pte2(4,10),pxk2(4,10),   
     .         pte3(4,10),pxk3(4,10),   
     .         pte4(4,10),pxk4(4,10),   
     .           si(4,10),  !Light saturation intensity 
     .          snk(4,10),  !Sinking rate
     .          xkp(4,10),xkn(4,10),xks(4,10), !Michaelis-Menten constants for P, N, and Si 
     .          pm(4,10),   !Mortality rate
     .        resp(4,10),   !Photorespiration rate
     .        resd(4,10),   !Dark respiration rate
c
c             Habitat factors for salinity, current velocity, and depth
     .        psplt(4,10),pspol(4,10),pspou(4,10),psput(4,10),  !salinity
     .        ppvlt(4,10),ppvol(4,10),ppvou(4,10),ppvut(4,10),  !velocity
     .        ppdlt(4,10),ppdol(4,10),ppdou(4,10),ppdut(4,10),  !depth
c             Phenology         
     .         ddi(4,10),   !Degree-days required for growth initiation
     .         dds(4,10),   !Degree-days for onset of senescence
     .              xlk
c
      common/c/  bc(8,5),  !Initial biomass 
     .          cmx(8,5),  !max consumption rate
     .         cte1(8,5),cxk1(8,5),   !Thornton-Lessem temperature parameters   
     .         cte2(8,5),cxk2(8,5),   
     .         cte3(8,5),cxk3(8,5),   
     .         cte4(8,5),cxk4(8,5),   
     .         rsda(8,5),    rs(8,5),           !specific dynamic action rate, respiration rate
     .          tro(8,5),   trm(8,5),           !temperature dependence of respiration
     .            f(8,5),     u(8,5),  cm(8,5), !egestion, excretion, mortality rates
     .          dft(8,5),                       !invertebrate drift, 1/d 
c
c             Habitat factors for dissolved oxygen, salinity, current velocity, and depth
     .         DOlt(8,5), DOol(8,5), DOou(8,5), DOut(8,5),  !dissolved oxygen
     .        psclt(8,5),pscol(8,5),pscou(8,5),pscut(8,5),  !salinity
     .        ccvlt(8,5),ccvol(8,5),ccvou(8,5),ccvut(8,5),  !velocity
     .        ccdlt(8,5),ccdol(8,5),ccdou(8,5),ccdut(8,5),  !depth
     .             bxkn,      bxkp
c
c
      common/px/  bpx(4,10),   !initial biomass
     .           psx(4,10),   !max photosyhnethetic rate
     .         pte1x(4,10),pxk1x(4,10),   !Thornton-Lessem temperature parameters   
     .         pte2x(4,10),pxk2x(4,10),   
     .         pte3x(4,10),pxk3x(4,10),   
     .         pte4x(4,10),pxk4x(4,10),   
     .           six(4,10),  !Light saturation intensity 
     .          snkx(4,10),  !Sinking rate
     .          xkpx(4,10),xknx(4,10),xksx(4,10), !Michaelis-Menten constants for P, N, and Si 
     .          pmx(4,10),   !Mortality rate
     .        respx(4,10),   !Photorespiration rate
     .        resdx(4,10),   !Dark respiration rate
c
c             Habitat factors for salinity, current velocity, and depth
     .        pspltx(4,10),pspolx(4,10),pspoux(4,10),psputx(4,10),  !salinity
     .        ppvltx(4,10),ppvolx(4,10),ppvoux(4,10),ppvutx(4,10),  !velocity
     .        ppdltx(4,10),ppdolx(4,10),ppdoux(4,10),ppdutx(4,10),  !depth
c             Phenology         
     .         ddix(4,10),   !Degree-days required for growth initiation
     .         ddsx(4,10),   !Degree-days for onset of senescence
     .              xlkx
c

      common/cx/  bcx(8,5),  !Initial biomass 
     .          cmxx(8,5),  !max consumption rate
     .         cte1x(8,5),cxk1x(8,5),   !Thornton-Lessem temperature parameters   
     .         cte2x(8,5),cxk2x(8,5),   
     .         cte3x(8,5),cxk3x(8,5),   
     .         cte4x(8,5),cxk4x(8,5),   
     .         rsdax(8,5),    rsx(8,5),           !specific dynamic action rate, respiration rate
     .          trox(8,5),   trmx(8,5),           !temperature dependence of respiration
     .            fx(8,5),     ux(8,5),  cmz(8,5), !egestion, excretion, mortality rates
     .          dftx(8,5),                       !invertebrate drift, 1/d 
c
c             Habitat factors for dissolved oxygen, salinity, current velocity, and depth
     .         DOltx(8,5), DOolx(8,5), DOoux(8,5), DOutx(8,5),  !dissolved oxygen
     .        pscltx(8,5),pscolx(8,5),pscoux(8,5),pscutx(8,5),  !salinity
     .        ccvltx(8,5),ccvolx(8,5),ccvoux(8,5),ccvutx(8,5),  !velocity
     .        ccdltx(8,5),ccdolx(8,5),ccdoux(8,5),ccdutx(8,5),  !depth
     .             bxknx,      bxkpx
c
      common/micfac/cfact(80),cmort(80)
c
      common/blkmic/xiday(80),enpt(80),effc(80),pclc50(80),idetr(80),
     .         fracLip(80),elemr(80),wetwt(80),q10(80),rb(80),uptrt(80),
     .              xnoec(80),envc(365),kappa
      common/blkmict/slpfct,shpfct,alogkow,iab,ihydr,ik2,pKa,pH,wtod,
     .          frcdetr,lethal,ibarb
      common/toxpar/itoxmdl,chemnm
c
      common/tef_q1/tef(365,80),parm_mod(365,80,9) !output for TEF QA
      common/tef_q2/pmax_out(365,80) 
c
c     obtain the daily TEF values
      jday=iday
      call getTEF(jday)
c
      is=0
c
c     Producer population parameters
      do ll=1,4
        kllm=nsp(ll)
        do k=1,kllm                                             
          is=is+1
c
          fctr=cfact(is)
          tef(iday,is)=fctr
c
          if(isyndrome.eq.0)then
           si(ll,k)=   six(ll,k)           
           ps(ll,k)=   amax1(0.,(psx(ll,k)  *(1.0-fctr)))
           snk(ll,k)=  amin1(1.0,(snkx(ll,k)*(1.0+fctr)))
           xkp(ll,k)=  xkpx(ll,k)*(1.0+fctr)
           xkn(ll,k)=  xknx(ll,k)*(1.0+fctr)
           xks(ll,k)=  xksx(ll,k)*(1.0+fctr)
           pm(ll,k)=   amin1(1.0,(pmx(ll,k) *(1.0+fctr)))
           resp(ll,k)=respx(ll,k)
           resd(ll,k)=resdx(ll,k)           *(1.0+fctr)
          endif
c
          if(isyndrome.eq.1)then
           si(ll,k)=   six(ll,k)
           ps(ll,k)=   amax1(0.,(psx(ll,k)  *(1.0-fctr)))
           snk(ll,k)=  snkx(ll,k)
           xkp(ll,k)=  xkpx(ll,k)
           xkn(ll,k)=  xknx(ll,k)
           xks(ll,k)=  xksx(ll,k)
           if(fctr.lt.1.0) pm(ll,k)=pmx(ll,k) 
           if(fctr.ge.1.0) pm(ll,k)=pmx(ll,k)*(1.0+fctr) 
           resp(ll,k)=respx(ll,k)
           resd(ll,k)=resdx(ll,k)          
          endif
c
c       Parameter values modified by the TEF
        parm_mod(iday,is,1)=si(ll,k)
        parm_mod(iday,is,2)=ps(ll,k)
        parm_mod(iday,is,3)=snk(ll,k)
        parm_mod(iday,is,4)=xkp(ll,k)
        parm_mod(iday,is,5)=xkn(ll,k)
        parm_mod(iday,is,6)=xks(ll,k)
        parm_mod(iday,is,7)=pm(ll,k)
        parm_mod(iday,is,8)=resp(ll,k)
        parm_mod(iday,is,9)=resd(ll,k)
c
c     check application of TEF
c     baseline growth rate
      a0 = psx(ll,k)
      c0 = (psx(ll,k)*respx(ll,k))+resdx(ll,k)+snkx(ll,k)+pmx(ll,k)
      r0 = a0-c0
      Bt = bpx(ll,k)*exp(r0*xiday(is))
c	
c     TEF-based growth rate
      ae =  parm_mod(iday,is,2)
      ce = (psx(ll,k)*respx(ll,k))
     .     + parm_mod(iday,is,3) + parm_mod(iday,is,7)
     .     + parm_mod(iday,is,9)
      re = ae-ce
c
      pcnt_eff=abs((re-r0)/(r0)*100.)
c
      if(abs(pmax_out(iday,is)-pcnt_eff) .le. 0.2)goto 20
c
c     adjust mortality to make up difference
      print*,'In genp adjust producer loop'
c 
      xmt = 0.
      xmt=r0-re
c
      pm(ll,k)=pm(ll,k)+xmt
      parm_mod(iday,is,7)=parm_mod(iday,is,7)+xmt
c 
c
20    continue
        enddo
      enddo
c
c     Consumer population parameters
      do ll=1,8
        kllm=nsc(ll)
        do k=1,kllm
          is=is+1
c
          fctr=cfact(is)
          tef(iday,is)=fctr
c
          cmx(ll,k)=  amax1(0.,cmxx(ll,k) *(1.0-fctr))
          rs(ll,k)=   rsx(ll,k)           *(1.0+fctr)
          rsda(ll,k)= rsdax(ll,k)
          u(ll,k)=    amin1(1.0,ux(ll,k)  *(1.0+fctr))
          f(ll,k)=    amin1(1.0,fx(ll,k)  *(1.0+fctr))
          cm(ll,k)=   amin1(1.0,cmz(ll,k) *(1.0+fctr))
c       Parameter values modified by the TEF
        parm_mod(iday,is,1)=cmx(ll,k)
        parm_mod(iday,is,2)=rs(ll,k)
        parm_mod(iday,is,3)=rsda(ll,k)
        parm_mod(iday,is,4)=u(ll,k)
        parm_mod(iday,is,5)=f(ll,k)
        parm_mod(iday,is,6)=cm(ll,k)
        parm_mod(iday,is,7)=0.
        parm_mod(iday,is,8)=0.
c
c     check application of TEF
c     baseline growth, Bt
      a0 = cmxx(ll,k)
	c0 = (cmxx(ll,k)*rsdax(ll,k))+rsx(ll,k)+ux(ll,k)+cmz(ll,k)
      r0 = a0-c0
      Bt = bcx(ll,k)*exp(r0*xiday(is))
c	
c     TEF-based growth
      ae =  parm_mod(iday,is,1)
      ce = (cmxx(ll,k)*parm_mod(iday,is,3))
     .     + parm_mod(iday,is,2) + parm_mod(iday,is,4)
     .     + parm_mod(iday,is,6)
      re = ae-ce
      Be = bcx(ll,k)*exp(re*xiday(is))
c
      pcnt_eff=abs(((Be-Bt)/(Bt-bcx(ll,k)))*100.)
c
      if(abs(pmax_out(iday,is)-pcnt_eff) .le. 0.2)goto 40
c     adjust consumer mortality to make up difference
      xmt = 0.
      rz=0.
      Bn = (100.0-pmax_out(iday,is))/100.*(Bt)
	  if(Bt.gt.Bn)rz = -(log(Bn/Bt)/xiday(is))
      xmt=amax1(0.,rz) !avoid negative mortality rates
      cm(ll,k)=amin1(1.0,cm(ll,k)+xmt)
      parm_mod(iday,is,6)=parm_mod(iday,is,6)+xmt
c
40    continue
        enddo
      enddo
c     adjust bacterioplankton 1/2 sat-n and p
      bxkn=bxknx*(1.0+fctr)
      bxkp=bxkpx*(1.0+fctr)
c
      return
      end
c
c
      Subroutine TEF_QA
      implicit real*4(a-h,o-z)
      character*14 popnam,iguild(4)
      character chmnm*16, chemnm*16
      real*4 slpfct,shpfct,alogkow,pKa,pH,wtod,frcdetr
      integer iab,ihydr,ik2,lethal

      common/csm4/npops
      common/t/nsp(4),nsc(8)
      common/px/  bpx(4,10),   !initial biomass
     .           psx(4,10),   !max photosyhnethetic rate
     .         pte1x(4,10),pxk1x(4,10),   !Thornton-Lessem temperature parameters   
     .         pte2x(4,10),pxk2x(4,10),   
     .         pte3x(4,10),pxk3x(4,10),   
     .         pte4x(4,10),pxk4x(4,10),   
     .           six(4,10),  !Light saturation intensity 
     .          snkx(4,10),  !Sinking rate
     .          xkpx(4,10),xknx(4,10),xksx(4,10), !Michaelis-Menten constants for P, N, and Si 
     .          pmx(4,10),   !Mortality rate
     .        respx(4,10),   !Photorespiration rate
     .        resdx(4,10),   !Dark respiration rate
c
c             Habitat factors for salinity, current velocity, and depth
     .        pspltx(4,10),pspolx(4,10),pspoux(4,10),psputx(4,10),  !salinity
     .        ppvltx(4,10),ppvolx(4,10),ppvoux(4,10),ppvutx(4,10),  !velocity
     .        ppdltx(4,10),ppdolx(4,10),ppdoux(4,10),ppdutx(4,10),  !depth
c             Phenology         
     .         ddix(4,10),   !Degree-days required for growth initiation
     .         ddsx(4,10),   !Degree-days for onset of senescence
     .              xlkx
c
      common/cx/  bcx(8,5),  !Initial biomass 
     .          cmxx(8,5),  !max consumption rate
     .         cte1x(8,5),cxk1x(8,5),   !Thornton-Lessem temperature parameters   
     .         cte2x(8,5),cxk2x(8,5),   
     .         cte3x(8,5),cxk3x(8,5),   
     .         cte4x(8,5),cxk4x(8,5),   
     .         rsdax(8,5),    rsx(8,5),           !specific dynamic action rate, respiration rate
     .          trox(8,5),   trmx(8,5),           !temperature dependence of respiration
     .            fx(8,5),     ux(8,5),  cmz(8,5), !egestion, excretion, mortality rates
     .          dftx(8,5),                       !invertebrate drift, 1/d 
c
c             Habitat factors for dissolved oxygen, salinity, current velocity, and depth
     .         DOltx(8,5), DOolx(8,5), DOoux(8,5), DOutx(8,5),  !dissolved oxygen
     .        pscltx(8,5),pscolx(8,5),pscoux(8,5),pscutx(8,5),  !salinity
     .        ccvltx(8,5),ccvolx(8,5),ccvoux(8,5),ccvutx(8,5),  !velocity
     .        ccdltx(8,5),ccdolx(8,5),ccdoux(8,5),ccdutx(8,5),  !depth
     .             bxknx,      bxkpx
c
      common/blkmic/xiday(80),enpt(80),effc(80),pclc50(80),idetr(80),
     .         fracLip(80),elemr(80),wetwt(80),q10(80),rb(80),uptrt(80),
     .              xnoec(80),envc(365),
     .              kappa
      common/blkmict/slpfct,shpfct,alogkow,iab,ihydr,ik2,pKa,pH,wtod,
     .          frcdetr,lethal,ibarb
      common/toxpar/itoxmdl,chemnm
c
      common/res1/isyndrome,islope
      common/res2/slope(80),xcept(80)
      common/res3/nplants
c
      common/tef_q1/tef(365,80),parm_mod(365,80,9) !output for TEF QA
      common/tef_q2/pmax_out(365,80)                  !for use in TEF QA 
      common/tef_q3/knum                                 !knum=1, output producer TEFs only
      common/tef_q4/delp_r(4,10,365),delp_e(4,10,365)    !daily specific growth rates, %/day
c                                                        !knum=2, output producer and consumer TEFs
c
      common/stein/rmass(80,365), !daily biomass values for reference simulation
     .             tmass(80,365)  !daily biomass values for treatment simulations
      common/name/popnam(80)
c
      data iguild/'Periphyton 1  ',
     .            'Periphyton 2  ',
     .            'Macrophytes   ',
     .            'Emergents     '/
c
      do iday=1,365
c
c      if(envc(iday).le.0.)goto 100 !outputs only for days on non-zero exposure
c
      ipop=0
c
      write(14,*)' '
c  
c     Primary producers
      do i=1,3 !guild loop 
c
      write(14,4)iday,envc(iday)
4     format( /'Day of Year: ',i3,
     .         '  Exposure concentration (ppb): ',f6.1/
     .         'CASM Population')
c
      write(14,6)iguild(i)
6     format(a14,5x,'EC50    Eff    TEF    Ps     ResP   ResD   Snk',
     .'    Pm     Is     xKn    xKp    xKs    Bmass      DelP'/
     .19x,'ppb      %     ---    1/d    1/d    1/d    1/d    1/d    ',
     .'e/m2/d mg/L   mg/L   mg/L   gC/m2      1/d')
c     
      do j=1,nsp(i) !population loop
c
      ipop=ipop+1
c     reference population
      write(14,16)popnam(ipop),
     .            psx(i,j),respx(i,j),resdx(i,j),snkx(i,j),pmx(i,j), !baseline parameter values
     .            six(i,j),xknx(i,j),xkpx(i,j),xksx(i,j),
     .            rmass(ipop,iday),delp_r(i,j,iday)                  !reference biomass, growth rate
16    format(a14,26x,5(1x,f6.4),1x,f6.2,3(1x,f6.4),
     .       1x,e10.4,1x,e10.4,'  Ref   ')                           !reference biomass value
c     exposed population 
      write(14,18)effc(ipop),pmax_out(iday,ipop),tef(iday,ipop),
     .            parm_mod(iday,ipop,2), !modified Ps value
     .            parm_mod(iday,ipop,8), !modified Resp value
     .            parm_mod(iday,ipop,9), !modified Resd value
     .            parm_mod(iday,ipop,3), !modified Snk value
     .            parm_mod(iday,ipop,7), !modified Pm value
     .            parm_mod(iday,ipop,1), !modified Is value
     .            parm_mod(iday,ipop,5), !modified xKn value
     .            parm_mod(iday,ipop,4), !modified xKp value
     .            parm_mod(iday,ipop,6), !modified xKs value
     .            tmass(ipop,iday),      !modified population biomass
     .            delp_e(i,j,iday)
c
18    format('Calculated TEF     ',f6.1,1x,f5.1,2x,6(1x,f6.4),
     .       1x,f6.2,3(1x,f6.4),1x,e10.4,1x,e10.4,'  Eff   ')
c
c     check application of TEF
c     baseline growth, Bt
      a0 = psx(i,j)
      c0 = (psx(i,j)*respx(i,j))+resdx(i,j)+snkx(i,j)+pmx(i,j)
      r0 = a0-c0
c	
c     TEF-based growth
      ae =  parm_mod(iday,ipop,2)
c     ce = (parm_mod(iday,ipop,2)*parm_mod(iday,ipop,8))
      ce=(psx(i,j)*respx(i,j))
     .     + parm_mod(iday,ipop,3) + parm_mod(iday,ipop,7)
     .     + parm_mod(iday,ipop,9)
      re = ae-ce
c
      pcnt_eff=abs((re-r0)/(r0) *100.)
c	             
c     calculate and output percent difference between Bref and Beff
      pdif=0.
      if(rmass(ipop,iday).gt.0.)pdif=
     .100. * (tmass(ipop,iday)-rmass(ipop,iday))/rmass(ipop,iday)
c
c
      rdif=0.
      if(delp_r(i,j,iday).ne.0.)rdif=
     .100. * (delp_e(i,j,iday)-delp_r(i,j,iday))/delp_r(i,j,iday)
      write(14,20)pcnt_eff,pdif,rdif
20    format('TEF verified % effect:   ',
     .       f6.1,75x,f8.2,1x,e10.4,'  % Diff')
c
      write(14,*)' '
c
      enddo !producer population loop
	enddo !producer guild loop
c
c
      if(knum.eq.2)then !option to skip consumer TEF output
c
c     Consumer populations            
      do i=1,8 !guild loop 
c
      do j=1,nsc(i) !population loop
c
      ipop=ipop+1
c
      if(ipop.eq.27 .or. ipop.eq.34)
     .write(14,4)iday,envc(iday)
c
      if(ipop.eq.27 .or. ipop.eq.34) write(14,7)
7     format('Consumer pop.   ',3x,'EC50    Eff    TEF     Cs',
     .'      Resp     SDA     U      Cm     Bmass'/
     .19x,'ppb      %     ---     1/d     1/d      ---     1/d    ',
     .    '1/d     gC/m2')
c     
c     reference population
      write(14,17)popnam(ipop),
     .            cmxx(i,j),rsx(i,j),rsdax(i,j),ux(i,j),cmz(i,j), !baseline parameter values
     .            rmass(ipop,iday)                                !reference biomass value
17    format(a14,26x,5(2x,f6.4),2x,e10.4,'  Ref   ')             
c     exposed population 
      write(14,19)effc(ipop),pmax_out(iday,ipop),tef(iday,ipop),
     .            parm_mod(iday,ipop,1), !modified Cs value
     .            parm_mod(iday,ipop,2), !modified Resp value
     .            parm_mod(iday,ipop,3), !modified SDA value
     .            parm_mod(iday,ipop,4), !modified U value
     .            parm_mod(iday,ipop,6), !modified Cm value
     .            tmass(ipop,iday)       !modified population biomass
c
19    format('Calculated TEF    ',f7.0,2x,f5.1,6(2x,f6.4),
     .       2x,e10.4,'  Eff   ')             
c
c     check application of TEF
c     baseline growth, Bt
      a0 = cmxx(i,j)
      c0 = (cmxx(i,j)*rsdax(i,j))+rsx(i,j)+ux(i,j)+cmz(i,j)
      r0 = a0-c0
      Bt = bcx(i,j)*exp(r0*xiday(ipop))
c	
c     TEF-based growth
      ae =  parm_mod(iday,ipop,1)
      ce = (parm_mod(iday,ipop,1)*parm_mod(iday,ipop,3))
     .     + parm_mod(iday,ipop,2) + parm_mod(iday,ipop,4)
     .     + parm_mod(iday,ipop,6)
      re = ae-ce
      Be = bcx(i,j)*exp(re*xiday(ipop))
c
      pcnt_eff=abs( ((Be-Bt)/(Bt-bcx(i,j))) *100.)
c     calculate and output percent difference between Bref and Beff
      pdif=0.
      if(rmass(ipop,iday).gt.0.)pdif=
     .100. * (tmass(ipop,iday)-rmass(ipop,iday))/rmass(ipop,iday)
c
      write(14,21)pcnt_eff,pdif
21    format('TEF verified % effect:    ',f6.1,52x,f8.2,'  % Diff')
      write(14,*)' '
      enddo !consumer population loop
	  enddo !consumer guild loop
c
      endif !consumer section
c
      enddo !day of year loop
c
      return
      end
c
c
c     Inverse standard normal function
      Subroutine invprob(p,x2)
      implicit real*4(a-h,o-z)
      c1=2.506628
      c2=0.3193815
      c3=-0.3565638
      c4=1.7814779
      c5=-1.821256
      c6=1.3302744
c
      p = 1.281053 * (p - 3.72) / 1.28 - 1.281053
      if(p.ge.0.)then
	   w=1.0
	  else
	   w=-1.0
      endif
c
      yb=1.0/(1.0+0.2316419 * w * p)
c
      x2 = 0.5 + w * (0.5 - (exp(-p*p/2.) / c1) *
     .     (yb * (c2+yb*(c3+yb*(c4+yb*(c5+yb*c6)))))) 
c
      return
      end
c
c
      Subroutine simdex_rev
      implicit real*4(a-h,o-z)
      character*80 outf,outtox,outbio,outtef
      character*120 expfil,toxfil
c    
      common/t/nsp(4),nsc(8)
      common/res3/nplants
      common/lblout/outf(28),expfil,toxfil,outtef
      common/csm4/npops
c
c     arrays for Steinhaus similarity coefficient
      common/stein/rmass(80,365), !daily biomass values for reference simulation
     .             tmass(80,365)  !daily biomass values for treatment simulation
      common/trk/ied,led
c
      common/sim/idfexp   !DOY for first non-zero atrazine concentration
      common/roll1/sim(11,365),
     .             ideff(11) !DOY of first SI .ne. 1.000
      common/res1/isyndrome,islope
c
c
      dimension simin(11),pdiff(11),idoy(11),idat(11),ichk(11)
      dimension sumdev(11),si365(11), !SI value on last DOY
     .                    xm365(11), !averging periods of 365 days
     .                    xm260(11), !averaging period of 260 days (day 105 initial exposure)
     .                    xmexp(11), !averaging period from day of initial exposure thru day 365
     .                    xmeff(11),  !averaging period from day of initial SI .ne. 1.000
     .					sumexp(11) !Shyam added 10/18/2015
c
c     Initialize
      do m=1,11
	    do n=1,365
	     sim(m,n)=1.
        enddo
      enddo
c
      do i=1,11
       simin(i)=1.0
       pdiff(i)=0.
       idoy(i)=0
       idat(i)=0
      enddo
c
c
c     Loop over days
      do j=1,365
c
c     Create producer guild SI values
      do ii=1,4
c
       if(ii.eq.1 .and. nsp(ii).gt.0)then !Phytoplankton
          n1=1
	      n2=nsp(1)
          sn1=0.
	      sd1=0.
            do i=n1,n2
	           sn1=sn1+amin1(rmass(i,j),tmass(i,j))
               sd1=sd1+rmass(i,j)+tmass(i,j)
	        enddo
	        if(sd1.gt.0.)sim(1,j)=2.0*sn1/sd1
	   endif
c
       if(ii.eq.2 .and. nsp(ii).gt.0)then !Periphyton
            n1=1+nsp(1)
	        n2=  nsp(1)+nsp(2)
            sn2=0.
	        sd2=0.
              do i=n1,n2
	           sn2=sn2+amin1(rmass(i,j),tmass(i,j))
               sd2=sd2+rmass(i,j)+tmass(i,j)
	          enddo
	        if(sd2.gt.0.)sim(2,j)=2.0*sn2/sd2
	   endif
c
       if(ii.eq.3 .and. nsp(ii).gt.0)then !Macrophytes
            n1=1+nsp(1)+nsp(2)
	        n2=  nsp(1)+nsp(2)+nsp(3)
            sn3=0.
	        sd3=0.
              do i=n1,n2
	           sn3=sn3+amin1(rmass(i,j),tmass(i,j))
               sd3=sd3+rmass(i,j)+tmass(i,j)
	          enddo
	        if(sd3.gt.0.)sim(3,j)=2.0*sn3/sd3
	   endif
c
       if(ii.eq.4 .and. nsp(ii).gt.0)then !Emergent aquatic plants
            n1=1+nsp(1)+nsp(2)+nsp(3)
	        n2=  nsp(1)+nsp(2)+nsp(3)+nsp(4)
            sn4=0.
	        sd4=0.
              do i=n1,n2
	           sn4=sn4+amin1(rmass(i,j),tmass(i,j))
                 sd4=sd4+rmass(i,j)+tmass(i,j)
	          enddo
	        if(sd4.gt.0.)sim(4,j)=2.0*sn4/sd4
	   endif
c
      enddo !plant guild loop
c
c
      sn5=0.   !Total producer community 
	  sd5=0.
        do i=1,nplants
	       sn5=sn5+amin1(rmass(i,j),tmass(i,j))
           sd5=sd5+rmass(i,j)+tmass(i,j)
	    enddo
	    if(sd5.gt.0.)sim(5,j)=2.0*sn5/sd5
c
c
c     Create consumer population files
      do ii=1,8
c
       if(ii.eq.1 .and. nsc(ii).gt.0)then !Zooplankton
          n1=1+nplants
          n2=  nsc(1)+nplants
              sn6=0.    
              sd6=0.
            do i=n1,n2
	           sn6=sn6+amin1(rmass(i,j),tmass(i,j))
               sd6=sd6+rmass(i,j)+tmass(i,j)
	        enddo
              if(sd6.gt.0.)sim(6,j)=2.0*sn6/sd6
	   endif
c
       if(ii.eq.2 .and. nsc(ii).gt.0)then !Pelagic omnivorous fish
          n1=1+nsc(1)+nplants
          n2=  nsc(1)+nsc(2)+nplants
              sn7=0.    
              sd7=0.
            do i=n1,n2
	           sn7=sn7+amin1(rmass(i,j),tmass(i,j))
               sd7=sd7+rmass(i,j)+tmass(i,j)
	        enddo
            if(sd7.gt.0.)sim(7,j)=2.0*sn7/sd7
       endif
c
       if(ii.eq.3 .and. nsc(ii).gt.0)then !Pelagic piscivorous fish
          n1=1+nsc(1)+nsc(2)+nplants
          n2=  nsc(1)+nsc(2)+nsc(3)+nplants
              sn8=0.    
              sd8=0.
            do i=n1,n2
	           sn8=sn8+amin1(rmass(i,j),tmass(i,j))
               sd8=sd8+rmass(i,j)+tmass(i,j)
	        enddo
            if(sd8.gt.0.)sim(8,j)=2.0*sn8/sd8
       endif
c
       if(ii.eq.5 .and. nsc(ii).gt.0)then !Benthic invertebrates
          n1=1+nsc(1)+nsc(2)+nsc(3)+nsc(4)+nplants
          n2=  nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nplants
              sn9=0.    
              sd9=0.
            do i=n1,n2
	           sn9=sn9+amin1(rmass(i,j),tmass(i,j))
               sd9=sd9+rmass(i,j)+tmass(i,j)
	        enddo
            if(sd9.gt.0.)sim(9,j)=2.0*sn9/sd9
       endif
c
       if(ii.eq.6 .and. nsc(ii).gt.0)then !Benthic omnivorous fish
          n1=1+nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nplants
          n2=  nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nsc(6)+nplants
              sn10=0.    
              sd10=0.
            do i=n1,n2
	           sn10=sn10+amin1(rmass(i,j),tmass(i,j))
                 sd10=sd10+rmass(i,j)+tmass(i,j)
	        enddo
            if(sd10.gt.0.)sim(10,j)=2.0*sn10/sd10
       endif
c
       if(ii.eq.7 .and. nsc(ii).gt.0)then !Benthic piscivorous fish
          n1=1+nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nsc(6)+nplants
          n2=  nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nsc(6)+nsc(7)+nplants
              sn11=0.    
              sd11=0.
            do i=n1,n2
	           sn11=sn11+amin1(rmass(i,j),tmass(i,j))
               sd11=sd11+rmass(i,j)+tmass(i,j)
	        enddo
             if(sd11.gt.0.)sim(11,j)=2.0*sn11/sd11
       endif
c
c
      enddo !consumer community loop
c
      enddo !DOY loop
c
c
c     Output to casm_tox.out (unit 8)
      write(8,*)' '
      write(8,'(a120)') toxfil
      write(8,'(a120)') expfil
      write(8,10)
c
      do j=1,365
         jj=j-ied
       write(8,12) j,jj,(sim(i,j),i=1,11)
      enddo
10    format('Steinhaus Similarity Index values'/
     .'Day   DFT  Phyto   Perip   Macro   Emerg   TProd   Zoopl   POFsh',
     .'   PPFsh   BInvt   BOFsh   BPFsh')       
12    format(i3,2x,i4,2x,11(f5.3,3x))
c
c     fnd DOY of first SI value not equal to 1.000 for each group
      do i=1,11
c
        ichk(i)=0
c
        do j=1,365
           if(ichk(i).eq.1)goto 40 
           if(sim(i,j).lt..9995)ideff(i)=j
           if(sim(i,j).lt..9995) ichk(i)=1
40         continue
        enddo
      enddo
c
C
c     calculate maximum percent difference
      do i=1,11
      sumexp(i)=0.0 !Shyam added 10/18/2015
c 
        do j=1,365
	     jj=j-ied
	     if(sim(i,j).ge.simin(i))goto 14
	        simin(i)=sim(i,j)
	        idoy(i)=j
	        idat(i)=jj
14      continue
c
        sumdev(i)=sumdev(i)+(1.0-sim(i,j))
	    if (j.lt.led) then ! Shyam added 10/18/2015
		sumexp(i)=sumexp(i)+(1.0-sim(i,j))
	    endif
c
        enddo
c
       pdiff(i)=abs(((simin(i)-1.0)/1.0)*100.)
c
       si365(i)=sim(i,365)
c
       xm365(i)=sumdev(i)/365.
       xm260(i)=sumdev(i)/260.
c
       if(idfexp.eq.0)idfexp=1 !avoid problem if there is no atrazine exposure, Shyam corrected idexp to idfexp on this line
       xmexp(i)=sumexp(i)/(float(led) - float(idfexp)) !Shyam changed 10/18/2015
c
       if(ideff(i).eq.0)ideff(i)=1
       xmeff(i)=sumdev(i)/(365. - float(ideff(i)-1))
c
      enddo
c
      write(8,*)' '
      write(8,'(a120)') toxfil
      write(8,'(a120)') expfil
      write(8,16)
      write(*,*)' '
      write(*,'(a120)') toxfil
      write(*,'(a120)') expfil
      write(*,16)
16    format('Max % Difference in Steinhaus Similarity Index values'/
     .'          Phyto Perip Macro Emerg TProd Zoopl POFsh',       
     .' PPFsh BInvt BOFsh BPFsh')       
c
      write(8,17)(simin(i),i=1,11)
      write(8,18)(idoy (i),i=1,11) !DOY where min SI occurs
      write(8,20)(pdiff(i),i=1,11)
      write(8,21)(sumdev(i),i=1,11)
      write(8,22)(xm365(i),i=1,11)
      write(8,23)(xm260(i),i=1,11)
      write(8,24)(xmexp(i),i=1,11)
      write(8,25)(xmeff(i),i=1,11)
      write(8,26)(si365(i),i=1,11)
      write(8,27)(ideff(i),i=1,11)
      write(8,28)idfexp
c
      write(*,17)(simin(i),i=1,11)
      write(*,18)(idoy (i),i=1,11)
      write(*,20)(pdiff(i),i=1,11)
      write(*,21)(sumdev(i),i=1,11)
      write(*,22)(xm365(i),i=1,11)
      write(*,23)(xm260(i),i=1,11)
      write(*,24)(xmexp(i),i=1,11)
      write(*,25)(xmeff(i),i=1,11)
      write(*,26)(si365(i),i=1,11)
      write(*,27)(ideff(i),i=1,11)
      write(*,28)idfexp
      write(*,29)led !Shyam added 10/18/2015
c
17    format('SIMmin   ',1x,11(f5.3,1x))
18    format('DOY      ',1x,11(i3,3x))
20    format('% Diff   ',1x,11(f5.1,1x))
21    format('SumDev   ',1x,11(f5.1,1x))
22    format('AveDev365',1x,11(f5.3,1x))
23    format('AveDev260',1x,11(f5.3,1x))
24    format('AveDevExp',1x,11(f5.3,1x))
25    format('AveDevEff',1x,11(f5.3,1x))
26    format('SI365    ',1x,11(f5.3,1x))
27    format('DOYFstEff',1x,11(i3,3x))
28    format('DOYFstExp',1x,i3)
29    format('DOYLstExp',1x,i3) !Shyam added 10/18/2015
c 
      return
	  end
c
c
      Subroutine CASMday_prey(iunit,iday)
      implicit real*4(a-h,o-z)
c
      character*14 popnam,carnam(4),guild_nam(3),
     .             envnam(13)
      common/t/nsp(4),nsc(8)
      common/name/popnam(80)
      common/daypry1/dlyprey(84,365) !daily prey biomass values
      common/daypry2/dlyenv(13) 
c
      common/env/ein(365),te(365),th(365),xpe(365),xne(365),xse(365),
     .           depth(365),cvel(365),tis(365),POCe(365),wind(365),
     .           salinity(365)      
c
      common/ibd1/carnam,guild_nam
      common/ibd2/envnam
      common/ibm_adv/om11_adv,om12_adv,om21_adv
c
      dimension tot_guild(3)    !total daily biomass for phytoplankton, periphyton, and zooplankton
c
c      print *,'In CASMday_prey()'
c      print *, iday
c      pause
c
      do i=1,3
       tot_guild(i)=0.
      enddo
c
c     compute index values from modeled populations
      nplants=nsp(1)+nsp(2)+nsp(3)+nsp(4)
c     producer guild 1
      np11=1                               ! 01
      np12=nsp(1)                          ! 05
c
c     producer guild 2
      np21=1+nsp(1)                        ! 06
      np22=  nsp(1)+nsp(2)                 ! 08
c
c     producer guild 3
      np31=1+nsp(1)+nsp(2)                 ! 09
      np32=  nsp(1)+nsp(2)+nsp(3)          ! 11
c
c     producer guild 4
      np41=1+nsp(1)+nsp(2)+nsp(3)          ! 11
      np42=  nsp(1)+nsp(2)+nsp(3)+nsp(4)   ! 10
c
c     consumer guild 1
      nc11=1+nplants                       ! 11
      nc12=nsc(1)+nplants                  ! 15
c
c     consumer guild 2
      nc21=1+nsc(1)+nplants                ! 16
      nc22=  nsc(1)+nsc(2)+nplants         ! 20
c
c     consumer guild 3
      nc31=1+nsc(1)+nsc(2)+nplants         ! 21
      nc32=  nsc(1)+nsc(2)+nsc(3)+nplants  ! 25
c
c     consumer guild 4
      nc41=1+nsc(1)+nsc(2)+nsc(3)+nplants          !26
      nc42=  nsc(1)+nsc(2)+nsc(3)+nsc(4)+nplants   !26            
c
c     consumer guild 5
      nc51=1+nsc(1)+nsc(2)+nsc(3)+nsc(4)+nplants                      ! 27
      nc52=  nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nplants               ! 31
c
c     consumer guild 6
      nc61=1+nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nplants               ! 32
      nc62=  nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nsc(6)+nplants        ! 36
c
c     consumer guild 7
      nc71=1+nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nsc(6)+nplants        ! 37
      nc72=  nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nsc(6)+nsc(7)+nplants ! 40
c
c     consumer guild 8
      nc81=1+nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nsc(6)+nsc(7)+nplants ! 41
      nc82=nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nsc(6)+nsc(7)+nsc(8)    ! 41
     .           +nplants
c
      ncons=nsc(1)+nsc(2)+nsc(3)+nsc(4)+nsc(5)+nsc(6)+nsc(7)+nsc(8)
c
      npops=nplants+ncons
c
c     output daily prey biomass results to file
c
        if(iunit.eq.990 .and. iday.eq.1)
     .     write(iunit,10)
10         format('CASM-CMB reference biomass values:')
        if(iunit.eq.992 .and. iday.eq.1)
     .     write(iunit,11)
11         format('CASM-CMB treatment biomass values:')
c
           if(iday.eq.1)write(iunit,13)
13    format('Calculated daily prey biomass (g C/m2):')
c
           if(iday.eq.1)write(iunit,121)
     .              (popnam(k),k=np11,np12),
     .              (popnam(k),k=np21,np22),
     .              (popnam(k),k=np31,np32),
     .              (popnam(k),k=np41,np42),
c
     .              (popnam(k),k=nc11,nc12),
     .              (popnam(k),k=nc21,nc22),
     .              (popnam(k),k=nc31,nc32),
     .              (popnam(k),k=nc41,nc42),
     .              (popnam(k),k=nc51,nc52),
     .              (popnam(k),k=nc61,nc62),
     .              (popnam(k),k=nc71,nc72),
     .              (popnam(k),k=nc81,nc82),
     .              (guild_nam(k),k=1,3),
     .              (envnam(k),k=1,13)
c
c
12    format(' Yr  AccDay  Day  ',117(a14,2x))
121   format(             'Day  ',117(a14,2x)) 
c
c
c     calculate guild biomass totals
c     phytoplankton
      sumph=0.
        do nn=np11,np12
	    sumph=sumph+dlyprey(nn,iday)
        enddo
      tot_guild(1)=sumph
c      
c     periphyton
      sumpe=0.
        do nn=np21,np22
	    sumpe=sumpe+dlyprey(nn,iday)
        enddo
      tot_guild(2)=sumpe
c      
c     zooplankton
      sumzo=0.
        do nn=nc11,nc12
	    sumzo=sumzo+dlyprey(nn,iday)
        enddo
      tot_guild(3)=sumzo
c
      write(iunit,20)iday,
     .               (dlyprey(j,iday),j=1,npops), !PREY:
     .               (tot_guild(n),n=1,3),        !daily guild totals
     .               (dlyenv(j),j=1,13)           !environmental factors
c
20    format(i3,2x,117(e14.8,2x))
c
      return
      end
c
