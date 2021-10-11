PRO FUNCTION_SIM_4_CD, vecDefs, PATH_SAVE, PATH_IMG, PATH_ROI, PREFIX_PHT, PREFIX_SIM, METAFILE, seed, distOpt

;seed = 1234567L      ;semente usada no processo de simulação (global? >> fazer 'common')
;seed = SYSTIME(/second) MOD 1000

dictionarySize = 3   ;quantidade de objetos implementados no dicionario de formas geométricas

PUT_HEAD_METAFILE, METAFILE
FOR s = 0, N_ELEMENTS(vecDefs)-1 DO BEGIN

   ;------------------------------------------------------------
   supDims = vecDefs[s].supDims  ;dimensões do suporte
   densityFill = vecDefs[s].densityFill    ;densidade de objetos que combrem a área do suporte
   scaleFact = vecDefs[s].scaleFact     ;fator de escala relacionao aos objetos que compõem a imagem
   varSize = vecDefs[s].varSize         ;valor maximo que os objetos podem expandir com relação ao fator de escala inicial
   
   ParsGauss = {zetaIni: vecDefs[s].zetaIni, zetaEnd: vecDefs[s].zetaEnd, psiIni: vecDefs[s].psiIni, psiEnd: vecDefs[s].psiEnd}
   ;------------------------------------------------------------

   Support = DBLARR(supDims[0],supDims[1])
   supportSize = N_ELEMENTS(Support)*1.0
   supportFill = Support
   supportFillDist = Support

   freePos = WHERE(supportFill EQ 0)

   listObjects = [PTR_NEW()]
   listObjectsDist = [PTR_NEW()]
   indexObject = 0L

   window, 1, xsize = supDims[0], ysize = supDims[1]
   window, 2, xsize = supDims[0], ysize = supDims[1]
   WHILE((1.0-((N_ELEMENTS(freePos)*1.0)/supportSize)) LE densityFill) DO BEGIN

      indexObject++
      sorteio = FLOOR(RANDOMU(seed)*10L^5)
      objIndex = sorteio MOD dictionarySize   ;tipo do objeto
     
      posXY = GET_FREE_POS(supportFill,supDims,seed)
     
      newObject = GEOMETRIC_DICTIONARY(objIndex, posXY, supDims, scaleFact, varSize, seed)
      listObjects = [listObjects , newObject]
      INCLUDE_OBJECT, supportFill, supDims, newObject, indexObject
     
      wset, 1
      tvscl, supportFill
     
      newObjectDist = GEOMETRIC_DISTORTION(newObject, varSize, seed)
      listObjectsDist = [listObjectsDist , newObjectDist]
      INCLUDE_OBJECT, supportFillDist, supDims, newObjectDist, indexObject
     
      wset, 2
      tvscl, supportFillDist
     
      freePos = WHERE(supportFill EQ 0)
      print, (N_ELEMENTS(freePos)/supportSize)
   ENDWHILE

   window, 3, xsize = supDims[0], ysize = supDims[1]
   wset, 3
   diff = ABS(supportFill - supportFillDist) < 1
   tvscl, diff
   
   ;save simulation...
   PATH_T1 = PATH_SAVE+PREFIX_PHT+STRTRIM(STRING(s+1),1)+'--T1.tif'
   PATH_T2 = PATH_SAVE+PREFIX_PHT+STRTRIM(STRING(s+1),1)+'--T2.tif'
   PATH_DI = PATH_SAVE+PREFIX_PHT+STRTRIM(STRING(s+1),1)+'--Dif.tif'
   PATH_OUT = PATH_SAVE+PREFIX_PHT+STRTRIM(STRING(s+1),1)+'--ROI.txt'
   
   WRITE_TIFF, PATH_T1, supportFill
   WRITE_TIFF, PATH_T2, supportFillDist
   WRITE_TIFF, PATH_DI, diff 
   
   FUNCTION_BUILD_ASCII_ROI_FROM_MASK, PATH_DI, PATH_OUT
     
   PRO_GAUSSIAN_SIMULATION_PHANTOM_PAIR_CHANGE_DETECTION, PATH_IMG, PATH_ROI, PATH_T1, PATH_T2, PATH_SAVE, PREFIX_SIM, $
                                                          STRTRIM(STRING(s+1),1), ParsGauss, distOpt, seed
   
   PUT_INFO_METAFILE, METAFILE, supDims, densityFill, scaleFact, varSize, ParsGauss
ENDFOR

Print,'Finishing simulation process...'
END



;#############################
PRO PUT_HEAD_METAFILE, Meta

OPENW, Arq, Meta, /GET_LUN, /APPEND
PRINTF, Arq, 'Dimensions ; Density ; Scale ; Variation ; zetaIni ; zetaEnd ; psiIni ; psiEnd'
Close, Arq
FREE_LUN, Arq

END


;#############################
PRO PUT_INFO_METAFILE, Meta, supDims, densityFill, scaleFact, varSize, parGauss

OPENW, Arq, Meta, /GET_LUN, /APPEND
PRINTF, Arq, STRTRIM(STRING(supDims[0]),1)+' ; '+STRTRIM(STRING(supDims[1]),1)+' ; '+STRTRIM(STRING(densityFill),1)+' ; '+$
        STRTRIM(STRING(scaleFact),1)+' ; '+STRTRIM(STRING(varSize),1)+' ; '+$
        STRTRIM(STRING(parGauss.zetaIni),1)+' ; '+STRTRIM(STRING(parGauss.zetaEnd),1)+' ; '+$
        STRTRIM(STRING(parGauss.psiIni),1)+' ; '+STRTRIM(STRING(parGauss.psiEnd),1)

Close, Arq
FREE_LUN, Arq

END