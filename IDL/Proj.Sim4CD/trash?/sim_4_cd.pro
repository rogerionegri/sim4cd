@geometric_dictionary.pro
@get_free_pos.pro
@include_object.pro
@geometric_distortion.pro


PRO SIM_4_CD

;seed = 1234567L      ;semente usada no processo de simulação (global? >> fazer 'common')
seed = SYSTIME(/second) MOD 1000
dictionarySize = 3   ;quantidade de objetos implementados no dicionario de formas geométricas

;------------------------------------------------------------
supDims = [500,500]  ;dimensões do suporte
densityFill = 0.75    ;densidade de objetos que combrem a área do suporte
scaleFact = 0.01     ;fator de escala relacionao aos objetos que compõem a imagem
varSize = 3         ;valor maximo que os objetos podem expandir com relação ao fator de escala inicial
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

stop
END