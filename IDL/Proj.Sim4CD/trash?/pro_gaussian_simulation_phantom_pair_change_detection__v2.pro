@BocaLib.pro
@gaussian_simulation_functions.pro
@generate_ortho_vectors.pro
@generate_ortho_vectors_diagdom.pro

PRO PRO_GAUSSIAN_SIMULATION_PHANTOM_PAIR_CHANGE_DETECTION__V2, PATH_IMG, PATH_ROI, PATH_SIM, PREFIX, info, ParsGauss, distOpt, seed, $
                                                               Phantom1, Phantom2, supportClass, supportClassDist


  ;-----------------------------------------------------------------------------------
  ZetaIni = ParsGauss.ZetaIni
  ZetaEnd = ParsGauss.ZetaEnd
  PsiIni = ParsGauss.PsiIni
  PsiEnd = ParsGauss.PsiEnd
  ;-----------------------------------------------------------------------------------

  ImageOriginal = READ_TIFF(PATH_IMG);OPEN_IMAGE(Path_img)
  IF distOpt THEN ImageDistorcida = PCA_DISTORTION(ImageOriginal) ELSE ImageDistorcida = ImageOriginal

  Rois = ASCII_READ_ROI(Path_roi)

;  Phantom1 = READ_TIFF(PATH_T1)
;  Phantom2 = READ_TIFF(PATH_T2)

  reindexPhantom1 = Phantom1
  reindexPhantom2 = Phantom2

  dimsImage = GET_DIMENSIONS(ImageOriginal)
  dimsPhantom = GET_DIMENSIONS(Phantom1)

  simPhantom1 = FLTARR(dimsImage[0],dimsPhantom[1],dimsPhantom[2])
  simPhantom2 = FLTARR(dimsImage[0],dimsPhantom[1],dimsPhantom[2])

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
      ;----------SIMULATION PHANTOM 1 DATA--------------
      XXX = supportClass[pos1[0]] -1
      
      DiagEValuesRef1 = *PtrDiagEValuesRef1[XXX]
      EVecRef1 = *EighVectorsRef1[XXX]  &  EVecRef1 = EVecRef1.EigenVector

      mult_Evec_DiagEval_Ref1 = EVecRef1 ## SQRT(DiagEValuesRef1)
      targetMeanVectorRef1 = TRANSPOSE(muRef1[XXX,*])

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
      ;reindexPhantom1[pos1] = randIndex[countIndex]
    ENDIF

    IF (pos2[0] NE -1) THEN BEGIN
      ;----------SIMULATION PHANTOM 2 DATA--------------
      YYY = supportClassDist[pos2[0]] -1
      
      DiagEValuesRef2 = *PtrDiagEValuesRef2[YYY]
      EVecRef2 = *EighVectorsRef2[YYY]  &  EVecRef2 = EVecRef2.EigenVector

      mult_Evec_DiagEval_Ref2 = EVecRef2 ## SQRT(DiagEValuesRef2)
      targetMeanVectorRef2 = TRANSPOSE(muRef2[YYY,*])

      Z = RANDOMN(seed, dimsImage[0], N_ELEMENTS(pos2))
      X = Z*0 ;just to copy and clean all info
      Y = Z*0 ;just to copy and clean all info

      varCov = RANDOMU(seed, 1)*(ZetaEnd - ZetaIni) + ZetaIni
      varMean = RANDOMU(seed, 1)*(PsiEnd - PsiIni) + PsiIni

      FOR j = 0L, N_ELEMENTS(X[0,*])-1 DO X[*,j] = mult_Evec_DiagEval_Ref2 ## Z[*,j]
      FOR j = 0L, N_ELEMENTS(Y[0,*])-1 DO Y[*,j] = targetMeanVectorRef2

      FOR j = 0L, N_ELEMENTS(pos2)-1 DO BEGIN
        col = pos2[j] MOD N_ELEMENTS(Phantom2[*,0])
        lin = pos2[j] / N_ELEMENTS(Phantom2[*,0])
        simPhantom2[*,col,lin] = (X[*,j]*varCov[0]) + (Y[*,j]*varMean[0])
      ENDFOR
      ;-------------------------------------------------
      reindexPhantom2[pos2] = randIndex[countIndex]
    ENDIF


    IF ((pos1[0] NE -1) OR (pos2[0] NE -1)) THEN countIndex++

  ENDFOR

  ;diffOriginal = ABS(Phantom1 - Phantom2) < 1
  ;diffPostSim = ABS(reindexPhantom1 - reindexPhantom2) < 1

  ;writing simulation...
  PATH_S1 = PATH_SIM+PREFIX+info+'--T1.tif'
  PATH_S2 = PATH_SIM+PREFIX+info+'--T2.tif'
  WRITE_TIFF, PATH_S1, simPhantom1, /FLOAT
  WRITE_TIFF, PATH_S2, simPhantom2, /FLOAT


  print, '++simulation...'
END