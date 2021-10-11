@BocaLib.pro
@gaussian_simulation_functions.pro
@generate_ortho_vectors.pro
@generate_ortho_vectors_diagdom.pro

@prime_sequence.pro

PRO PRO_GAUSSIAN_SIMULATION_PHANTOM_PAIR_CHANGE_DETECTION__V3__withSegm, PATH_IMG, PATH_ROI, Phantom1, Phantom2, PATH_SAVE, PREFIX_SIM, PREFIX_PHT, PREFIX_SEGM, info, ParsGauss, distOpt, seed, randPerc, correlationWindow

  ;-----------------------------------------------------------------------------------
  ZetaIni = ParsGauss.ZetaIni
  ZetaEnd = ParsGauss.ZetaEnd
  PsiIni = ParsGauss.PsiIni
  PsiEnd = ParsGauss.PsiEnd
  ;-----------------------------------------------------------------------------------

;temp---remover
seed = 100

  ImageOriginal = READ_TIFF(PATH_IMG);OPEN_IMAGE(Path_img)
  IF distOpt THEN ImageDistorcida = PCA_DISTORTION(ImageOriginal, seed) ELSE ImageDistorcida = ImageOriginal

seed = 10

  Rois = ASCII_READ_ROI(Path_roi)
  
  reindexPhantom1 = Phantom1
  reindexPhantom2 = Phantom2

  dimsImage = GET_DIMENSIONS(ImageOriginal)
  dimsPhantom = GET_DIMENSIONS(Phantom1)

  simPhantom1 = FLTARR(dimsImage[0],dimsPhantom[1],dimsPhantom[2])
  simPhantom2 = FLTARR(dimsImage[0],dimsPhantom[1],dimsPhantom[2])

  nClass = N_ELEMENTS(Rois)
  primeSeq = PRIME_SEQUENCE(nClass*2)
  prime1 = primeSeq[0:nClass-1]
  prime2 = primeSeq[nClass:*]
  primePhantom1 = Phantom1
  primePhantom2 = Phantom2


  ;-----------------------------------------------
  SampleClassRef1 = GET_LABELED_INFO(ImageOriginal, Rois)
  muRef1 = GET_MU_VECTOR(SampleClassRef1)
  SigmaRef1 = GET_SIGMA_MATRIX(SampleClassRef1)
  EighVectorsRef1 = GET_EIGENVECTOR(SigmaRef1)
  PtrDiagEValuesRef1 = EV_DIAGONAL_MATRIX(EighVectorsRef1)
  ;-----------------------------------------------

  ;-----------------------------------------------
  SampleClassRef2 = GET_LABELED_INFO(ImageDistorcida, Rois)
  muRef2 = GET_MU_VECTOR(SampleClassRef2)
  SigmaRef2 = GET_SIGMA_MATRIX(SampleClassRef2)
  EighVectorsRef2 = GET_EIGENVECTOR(SigmaRef2)
  PtrDiagEValuesRef2 = EV_DIAGONAL_MATRIX(EighVectorsRef2)
  ;-----------------------------------------------


  minPhantoms = MIN([MIN(Phantom1),MIN(Phantom2)])
  maxPhantoms = MAX([MAX(Phantom1),MAX(Phantom2)])

  randIndex = LONG(RANDOMU(seed,maxPhantoms-minPhantoms+1)*100000L) MOD N_ELEMENTS(Rois)
  countIndex = 0L

  FOR ind = minPhantoms, maxPhantoms DO BEGIN
    pos1 = WHERE(Phantom1 EQ ind)
    pos2 = WHERE(Phantom2 EQ ind)

    ;IF (pos1[0] NE -1) AND (pos2[0] NE -1) THEN BEGIN

    IF (pos1[0] NE -1) THEN BEGIN
      
seed = ind
      
      ;----------SIMULATION PHANTOM 1 DATA--------------
      DiagEValuesRef1 = *PtrDiagEValuesRef1[randIndex[countIndex]]
      EVecRef1 = *EighVectorsRef1[randIndex[countIndex]]  &  EVecRef1 = EVecRef1.EigenVector

      mult_Evec_DiagEval_Ref1 = EVecRef1 ## SQRT(DiagEValuesRef1)
      targetMeanVectorRef1 = TRANSPOSE(muRef1[randIndex[countIndex],*])

      Z = RANDOMN(seed, dimsImage[0], N_ELEMENTS(pos1))
      X = Z*0 ;just to copy and clean all info
      Y = Z*0 ;just to copy and clean all info

      varCov = RANDOMU(seed, 1)*(ZetaEnd - ZetaIni) + ZetaIni
      varMean = RANDOMU(seed, 1)*(PsiEnd - PsiIni) + PsiIni

      FOR j = 0L, N_ELEMENTS(X[0,*])-1 DO X[*,j] = mult_Evec_DiagEval_Ref1 ## Z[*,j]
      FOR j = 0L, N_ELEMENTS(Y[0,*])-1 DO Y[*,j] = targetMeanVectorRef1

      FOR j = 0L, N_ELEMENTS(pos1)-1 DO BEGIN
        col = pos1[j] MOD N_ELEMENTS(Phantom1[*,0])
        lin = pos1[j] / N_ELEMENTS(Phantom1[*,0])
        ;simPhantom1[*,col,lin] = (CovPhantomFill[*,col,lin]*varCov[k-1]) + (MeanPhantomFill[*,col,lin]*varMean[k-1])
        simPhantom1[*,col,lin] = (X[*,j]*varCov[0]) + (Y[*,j]*varMean[0])
      ENDFOR
      ;-------------------------------------------------
      reindexPhantom1[pos1] = randIndex[countIndex]
      primePhantom1[pos1] = prime1[randIndex[countIndex]]
    ENDIF

    IF (pos2[0] NE -1) THEN BEGIN

seed = ind

      ;----------SIMULATION PHANTOM 2 DATA--------------
      DiagEValuesRef2 = *PtrDiagEValuesRef2[randIndex[countIndex]]
      EVecRef2 = *EighVectorsRef2[randIndex[countIndex]]  &  EVecRef2 = EVecRef2.EigenVector

      mult_Evec_DiagEval_Ref2 = EVecRef2 ## SQRT(DiagEValuesRef2)
      targetMeanVectorRef2 = TRANSPOSE(muRef2[randIndex[countIndex],*])

      Z = RANDOMN(seed, dimsImage[0], N_ELEMENTS(pos2))
      X = Z*0 ;just to copy and clean all info
      Y = Z*0 ;just to copy and clean all info

;Temporario.... descomentar!
      ;varCov = RANDOMU(seed, 1)*(ZetaEnd - ZetaIni) + ZetaIni
      ;varMean = RANDOMU(seed, 1)*(PsiEnd - PsiIni) + PsiIni

      FOR j = 0L, N_ELEMENTS(X[0,*])-1 DO X[*,j] = mult_Evec_DiagEval_Ref2 ## Z[*,j]
      FOR j = 0L, N_ELEMENTS(Y[0,*])-1 DO Y[*,j] = targetMeanVectorRef2

      FOR j = 0L, N_ELEMENTS(pos2)-1 DO BEGIN
        col = pos2[j] MOD N_ELEMENTS(Phantom2[*,0])
        lin = pos2[j] / N_ELEMENTS(Phantom2[*,0])
        simPhantom2[*,col,lin] = (X[*,j]*varCov[0]) + (Y[*,j]*varMean[0])
      ENDFOR
      ;-------------------------------------------------
      reindexPhantom2[pos2] = randIndex[countIndex]
      primePhantom2[pos2] = prime2[randIndex[countIndex]]
    ENDIF


    IF ((pos1[0] NE -1) OR (pos2[0] NE -1)) THEN countIndex++

  ENDFOR


  diffPostSim = ABS(reindexPhantom1 - reindexPhantom2) < 1


  ;save simulation -- Phantom...
  PATH_T1 = PATH_SAVE+PREFIX_PHT+info+'--T1.tif'
  PATH_T2 = PATH_SAVE+PREFIX_PHT+info+'--T2.tif'
  
  ;save simulation -- Segmentation...
  PATH_Seg1 = PATH_SAVE+PREFIX_SEGM+info+'--T1.tif'
  PATH_Seg2 = PATH_SAVE+PREFIX_SEGM+info+'--T2.tif'  
  
  ;save prime transition representation
  PATH_PRIME = PATH_SAVE+PREFIX_SIM+info+'--PrimeTrans.tif'  
  
  ;save simulation -- Phantom+Spectral...
  PATH_S1 = PATH_SAVE+PREFIX_SIM+info+'--T1.tif'
  PATH_S2 = PATH_SAVE+PREFIX_SIM+info+'--T2.tif'
  PATH_S1_corr = PATH_SAVE+PREFIX_SIM+info+'--T1_corr.tif'
  PATH_S2_corr = PATH_SAVE+PREFIX_SIM+info+'--T2_corr.tif'
  
  ;save ROIs + Diff
  PATH_DI = PATH_SAVE+PREFIX_PHT+info+'--Dif.tif'
  PATH_OUT = PATH_SAVE+PREFIX_PHT+info+'--ROI.txt'
  
  PATH_OUT_IND_T1 = PATH_SAVE+PREFIX_PHT+info+'--ROI_Point_T1.txt'
  PATH_OUT_IND_T2 = PATH_SAVE+PREFIX_PHT+info+'--ROI_Point_T2.txt'
  
  PATH_OUT_IND_T1_TEST = PATH_SAVE+PREFIX_PHT+info+'--ROI_T1_Complete.txt'
  PATH_OUT_IND_T2_TEST = PATH_SAVE+PREFIX_PHT+info+'--ROI_T2_Complete.txt'
  
  PATH_OUT_IND_CONT_T1 = PATH_SAVE+PREFIX_PHT+info+'--ROI_Poly_T1.txt'
  PATH_OUT_IND_CONT_T2 = PATH_SAVE+PREFIX_PHT+info+'--ROI_Poly_T2.txt'
  
  PATH_OUT_IND_T12 = PATH_SAVE+PREFIX_PHT+info+'--ROI_TransPrime.txt'
  PATH_OUT_IND_T12_COMPLETE = PATH_SAVE+PREFIX_PHT+info+'--ROI_Trans_Test.txt'
  
  ;writing images...
  WRITE_TIFF, PATH_PRIME, primePhantom1*primePhantom2, /LONG
  WRITE_TIFF, PATH_T1, reindexPhantom1
  WRITE_TIFF, PATH_T2, reindexPhantom2
  WRITE_TIFF, PATH_S1, simPhantom1, /FLOAT
  WRITE_TIFF, PATH_S2, simPhantom2, /FLOAT
  WRITE_TIFF, PATH_DI, diffPostSim
  
  WRITE_TIFF, PATH_Seg1, Phantom1, /LONG
  WRITE_TIFF, PATH_Seg2, Phantom2, /LONG
  
  ;MatFilter = FLTARR(correlationWindow,correlationWindow)
  MatFilter = [[0.024 , 0.10695 , 0.024] , [0.10695 , 0.4761 , 0.10695] , [0.024 , 0.10695 , 0.024]]
  simPhantomCorr1 = CORRELATION_FILTER(simPhantom1, MatFilter)
  simPhantomCorr2 = CORRELATION_FILTER(simPhantom2, MatFilter)
  WRITE_TIFF, PATH_S1_corr, simPhantomCorr1, /FLOAT
  WRITE_TIFF, PATH_S2_corr, simPhantomCorr2, /FLOAT
  
  ;writing samples...
  FUNCTION_BUILD_ASCII_ROI_FROM_MASK, PATH_DI, PATH_OUT, seed
  
  FUNCTION_BUILD_ASCII_ROI_FROM_PHANTOM, reindexPhantom1, PATH_OUT_IND_T1, randPerc, seed
  FUNCTION_BUILD_ASCII_ROI_FROM_PHANTOM, reindexPhantom2, PATH_OUT_IND_T2, randPerc, seed  
  
  FUNCTION_BUILD_ASCII_ROI_FROM_PHANTOM, reindexPhantom1, PATH_OUT_IND_T1_TEST, 1.0, seed
  FUNCTION_BUILD_ASCII_ROI_FROM_PHANTOM, reindexPhantom2, PATH_OUT_IND_T2_TEST, 1.0, seed
  
  FUNCTION_BUILD_ASCII_ROI_FROM_PHANTOM_COMPACT_REGION, Phantom1, reindexPhantom1, PATH_OUT_IND_CONT_T1, randPerc, seed
  FUNCTION_BUILD_ASCII_ROI_FROM_PHANTOM_COMPACT_REGION, Phantom2, reindexPhantom2, PATH_OUT_IND_CONT_T2, randPerc, seed
  
  FUNCTION_BUILD_ASCII_ROI_FROM_TRANSITION_TRAIN_TEST, reindexPhantom1, reindexPhantom2, PATH_OUT_IND_T12, PATH_OUT_IND_T12_COMPLETE, randPerc, seed, nClass
  
  print, '++simulation...'
END