PRO FUNCTION_SIM_4_CD__POLSAR, vecDefs, PATH_SAVE, PATH_IMG, PATH_ROI, PREFIX_PHT, PREFIX_SIM, METAFILE, seed, distOpt, randPerc

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

    ;alterar esse noma???
    ParsGauss = {zetaIni: vecDefs[s].zetaIni, zetaEnd: vecDefs[s].zetaEnd, psiIni: vecDefs[s].psiIni, psiEnd: vecDefs[s].psiEnd}


    correlationWindow = vecDefs[s].correlation
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

;    PRO_GAUSSIAN_SIMULATION_PHANTOM_PAIR_CHANGE_DETECTION__v3, PATH_IMG, PATH_ROI, supportFill, supportFillDist, PATH_SAVE, PREFIX_SIM, PREFIX_PHT, $
;      STRTRIM(STRING(s+1),1), ParsGauss, distOpt, seed, randPerc, correlationWindow
      
    PRO_GOODMAN_SIMULATION_PHANTOM_PAIR_CHANGE_DETECTION, PATH_IMG, PATH_ROI, supportFill, supportFillDist, PATH_SAVE, PREFIX_SIM, PREFIX_PHT, $
                                                          STRTRIM(STRING(s+1),1), ParsGauss, distOpt, seed, randPerc, correlationWindow      

    PUT_INFO_METAFILE, METAFILE, supDims, densityFill, scaleFact, varSize, ParsGauss
  ENDFOR

  Print,'Finishing simulation process...'
END