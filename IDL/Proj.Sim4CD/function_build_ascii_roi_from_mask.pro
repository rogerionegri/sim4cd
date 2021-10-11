@RoiFunctions.pro

PRO FUNCTION_BUILD_ASCII_ROI_FROM_MASK, PATH_MASK, PATH_OUT, seed

mask = READ_TIFF(PATH_MASK)
dims = GET_DIMENSIONS(mask)

change = WHERE(mask EQ 1)
unchange = WHERE(mask EQ 0)

ptrChan = PTR_NEW({RoiName: 'change', RoiColor: [255,0,0], Roilex: change})
ptrUnchan = PTR_NEW({RoiName: 'unchange', RoiColor: [0,255,0], Roilex: unchange})

ptrROI = [ptrChan , ptrUnchan] ;<<<ordem importante!

ASCII_SAVE_ROI, ptrROI, PATH_OUT, dims
END


;###################################################
PRO FUNCTION_BUILD_ASCII_ROI_FROM_PHANTOM, phantom, PATH_OUT, randPerc, seed

dims = GET_DIMENSIONS(phantom)

minIndex = LONG(MIN(phantom))
maxIndex = LONG(MAX(phantom))

ptrRoi = PTR_NEW()
FOR i = minIndex, maxIndex DO BEGIN
  pos = WHERE(phantom EQ i)
  rand = SORT(RANDOMU(seed,N_ELEMENTS(pos)))
  
  IF FLOOR(0.001 + N_ELEMENTS(pos)*randPerc) GE N_ELEMENTS(pos) THEN  choosenPos = pos $
    ELSE choosenPos = pos[rand[0:FLOOR(0.001 + N_ELEMENTS(pos)*randPerc)]]
  
  color = TEKTRONIX(i)
  roi = PTR_NEW({RoiName: 'index_'+STRTRIM(STRING(i),1), RoiColor: color, Roilex: choosenPos})
  ptrRoi = [ptrRoi, roi]
ENDFOR
ptrRoi = ptrRoi[1:*]

ASCII_SAVE_ROI, ptrRoi, PATH_OUT, dims
END


;###################################################
PRO FUNCTION_BUILD_ASCII_ROI_FROM_PHANTOM_COMPACT_REGION, originalPhantom, reindexPhantom, PATH_OUT, randPerc, seed

  dims = GET_DIMENSIONS(reindexPhantom)

  minIndex = LONG(MIN(reindexPhantom))
  maxIndex = LONG(MAX(reindexPhantom))

  ptrRoi = PTR_NEW()
  FOR i = minIndex, maxIndex DO BEGIN
    pos = WHERE(reindexPhantom EQ i)
    
    regsOrig = originalPhantom[pos]
    indexRegOrig = regsOrig[UNIQ(regsOrig, SORT(regsOrig))]
    
    Q = N_ELEMENTS(pos)*randPerc
    R = N_ELEMENTS(indexRegOrig)
    lado = CEIL(SQRT((FLOAT(Q)/R)))
    
    currentRoiLex = -1L
    FOR numReg = 0, R-1 DO BEGIN
       pos2 = WHERE(originalPhantom EQ indexRegOrig[numReg])
       
       ok = 0
       largerListSample = -1L
       count = 0L
       WHILE ~ok DO BEGIN
          randPoint = LONG(RANDOMU(seed,1)*100000L) MOD N_ELEMENTS(pos2) 
          choosenPoint = pos2[randPoint]
          
          colChoosen = choosenPoint MOD dims[1]
          linChoosen = choosenPoint / dims[1]
        
          listSample = -1L
          FOR ii = -lado/2, lado/2 DO BEGIN
             FOR jj = -lado/2, lado/2 DO BEGIN
             
                ti = colChoosen + ii
                tj = linChoosen + jj
                
                IF ((ti GE 0) AND (ti LT dims[1]) AND (tj GE 0) AND (tj LT dims[2])) THEN BEGIN
                   IF (originalPhantom[ti,tj] EQ indexRegOrig[numReg]) THEN listSample = [listSample , (tj*dims[2] + ti)]
                ENDIF 
              
             ENDFOR
          ENDFOR
        
          IF N_ELEMENTS(listSample) GT 1 THEN $
             IF N_ELEMENTS(listSample[1:*]) GT (lado^2)*0.5 THEN ok=1
             
          IF N_ELEMENTS(listSample[1:*]) GT N_ELEMENTS(largerListSample) THEN largerListSample = listSample[1:*]
          
          count++
          
          IF count GT 100 THEN ok=1
          
       ENDWHILE
       
       currentRoiLex = [currentRoiLex , listSample[1:*]]
    ENDFOR

    color = TEKTRONIX(i)
    roi = PTR_NEW({RoiName: 'index_'+STRTRIM(STRING(i),1), RoiColor: color, Roilex: largerListSample})
    ptrRoi = [ptrRoi, roi]
  ENDFOR
  ptrRoi = ptrRoi[1:*]

  ASCII_SAVE_ROI, ptrRoi, PATH_OUT, dims
END



;###################################################
PRO FUNCTION_BUILD_ASCII_ROI_FROM_TRANSITION_TRAIN_TEST, phantom1, phantom2, PATH_TRAIN, PATH_TEST, randPerc, seed, nClass

  primeSeq = PRIME_SEQUENCE(nClass*2)
  prime1 = primeSeq[0:nClass-1]
  prime2 = primeSeq[nClass:*]

  dims = GET_DIMENSIONS(phantom1)
  minIndex = LONG(MIN(phantom1))
  maxIndex = LONG(MAX(phantom1))

  ptrRoi = PTR_NEW()
  ptrTest = PTR_NEW()
  FOR i = minIndex, maxIndex DO BEGIN
    FOR j = minIndex, maxIndex DO BEGIN
      
      posI = WHERE(phantom1 EQ i)
      posIJ = WHERE(phantom2[posI] EQ j)
      
      IF posIJ[0] NE -1 THEN BEGIN
        rand = SORT(RANDOMU(seed,N_ELEMENTS(posIJ)))
        choosenPos = posI[posIJ[rand[0:FLOOR(0.001 + N_ELEMENTS(posIJ)*randPerc)]]]

        color = TEKTRONIX(i)/(j+1)
        
        nameInd = STRTRIM(STRING(LONG(prime1[i])*prime2[j]),1)
        
        roi = PTR_NEW({RoiName: nameInd, RoiColor: color, Roilex: choosenPos})
        test = PTR_NEW({RoiName: nameInd, RoiColor: color, Roilex: posI[posIJ]})
        
        ptrRoi = [ptrRoi, roi]
        ptrTest = [ptrTest, test]
      ENDIF
    ENDFOR
  ENDFOR    
    
  ptrRoi = ptrRoi[1:*]
  ptrTest = ptrTest[1:*]

  ASCII_SAVE_ROI, ptrRoi, PATH_TRAIN, dims
  ASCII_SAVE_ROI, ptrTest, PATH_TEST, dims
END