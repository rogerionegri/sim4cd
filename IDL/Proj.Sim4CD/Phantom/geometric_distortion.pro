;Esta função gera uma dada figura geométrica e respectivos atributos de representacao (rotacao, escala, parametros associados, etc)
FUNCTION GEOMETRIC_DISTORTION, PtrObj, var, seed

  obj = *PtrObj

  CASE obj.type OF
    0: ptrDistObj = RECTANGLE_DISTORTION(obj,var,seed)
    1: ptrDistObj = CIRCLE_DISTORTION(obj,var,seed)
    2: ptrDistObj = TRIANGLE_DISTORTION(obj,var,seed)
    ELSE: stop ;<<<caso inesperado...
  ENDCASE

  Return, ptrDistObj
END



;-------------------------------------------------
FUNCTION RECTANGLE_DISTORTION, obj,var,seed

  l1 = FLOOR(obj.parameters[0] * (1 + (RANDOMU(seed) - 0.5)))
  l2 = FLOOR(obj.parameters[1] * (1 + (RANDOMU(seed) - 0.5)))
  rotation = obj.rotation

  R = [ [cos(rotation) , sin(rotation)] , [-sin(rotation) , cos(rotation)] ]

  bbh = l1
  bbv = l2
  center = [bbh/2 , bbv/2]

  pair = 0L
  listPos = FLTARR(2,bbh*bbv)
  FOR i = 0L, bbh-1 DO BEGIN
    FOR j = 0L, bbv-1 DO BEGIN
      pos = [i - center[0] , j - center[1]]
      rotPos = R#pos
      listPos[*,pair] = rotPos
      pair++
    ENDFOR
  ENDFOR

  deltaH = FLOOR(MAX(listPos[0,*]) - MIN(listPos[0,*]))
  deltaV = FLOOR(MAX(listPos[1,*]) - MIN(listPos[1,*]))

  cornLU = [MIN(listPos[0,*]) , MIN(listPos[1,*])]

;  figure = INTARR(deltaH,deltaV)
;  FOR i = 0, pair-2 DO BEGIN
;    figure[ 0 > FLOOR(listPos[0,i] - cornLU[0]) < (deltaH-1) , 0 > FLOOR(listPos[1,i] - cornLU[1]) < (deltaV-1)  ] = 1
;  ENDFOR
;  figureReg = REGULARIZE_ORD1(figure)
  
  
  IF (deltaH GT 1) AND (deltaV GT 1) THEN BEGIN
    figure = INTARR(deltaH,deltaV)
    FOR i = 0L, pair-2 DO BEGIN
      figure[ 0 > FLOOR(listPos[0,i] - cornLU[0]) < (deltaH-1) , 0 > FLOOR(listPos[1,i] - cornLU[1]) < (deltaV-1)  ] = 1
      figureReg = REGULARIZE_ORD1(figure)
    ENDFOR
  ENDIF ELSE figureReg =  intarr(3,3) ;<<< qnd desaparece a figura
  

  Return, PTR_NEW( {type: 0, rotation: rotation, parameters: [l1,l2], figure: figureReg, position: obj.position} )
END



;-------------------------------------------------
FUNCTION CIRCLE_DISTORTION, obj,var,seed

  radius = FLOOR(obj.parameters[0] * (1 + (RANDOMU(seed) - 0.5)))
  center = [FLOOR((2*radius+1)/2) , FLOOR((2*radius+1)/2)]

  figure = INTARR(2*radius+1 , 2*radius+1)
  FOR i = 0, 2*radius DO BEGIN
    FOR j = 0, 2*radius DO BEGIN
      IF NORM( [i,j] - center ) LE radius THEN figure[i,j] = 1
    ENDFOR
  ENDFOR

  Return, PTR_NEW( {type: 1, rotation: 0, parameters: [radius], figure: figure, position: obj.position} )
END



;-------------------------------------------------
FUNCTION TRIANGLE_DISTORTION, obj,var,seed

  l1 = FLOOR(obj.parameters[0] * (1 + (RANDOMU(seed) - 0.5)))
  l2 = FLOOR(obj.parameters[1] * (1 + (RANDOMU(seed) - 0.5)))
  rotation = obj.rotation

  R = [ [cos(rotation) , sin(rotation)] , [-sin(rotation) , cos(rotation)] ]

  bbh = l1
  bbv = l2
  center = [bbh/2 , bbv/2]

  A = [0,0]
  B = [l1,0]

  minAngle = 45*(!PI/180)
  it = 0L
  WHILE 1 DO BEGIN
    CX = FLOOR(RANDOMU(seed)*10L^5) MOD l1
    CY = FLOOR(RANDOMU(seed)*10L^5) MOD l2
    C = [CX , CY]
    theta = ACOS( (TOTAL( (A-B)*(A-C) )) / ( NORM(A-B) * NORM(A-C) ))

    IF theta GT minAngle THEN BREAK

    IF it EQ 1000 THEN BEGIN ;<<< escapar de determinados casos... 
       theta = minAngle
       BREAK
    ENDIF

    it++
  ENDWHILE


  pair = 0L
  listPos = FLTARR(2,bbh*bbv)
  FOR i = 0L, bbh-1 DO BEGIN
    FOR j = 0L, bbv-1 DO BEGIN

      P = [i,j]

      thetaAPB = ACOS((TOTAL( (A-P) * (B-P) ))/( NORM(A-P) * NORM(B-P) )) > 0
      thetaAPC = ACOS((TOTAL( (A-P) * (C-P) ))/( NORM(A-P) * NORM(C-P) )) > 0
      thetaBPC = ACOS((TOTAL( (B-P) * (C-P) ))/( NORM(B-P) * NORM(C-P) )) > 0

      IF ABS( (thetaAPB + thetaAPC + thetaBPC) - 2*!PI) LE 0.00001 THEN BEGIN

        pos = [i , j]

        rotPos = R#pos
        listPos[*,pair] = rotPos
        pair++
      ENDIF
    ENDFOR
  ENDFOR

  deltaH = FLOOR(MAX(listPos[0,*]) - MIN(listPos[0,*]))
  deltaV = FLOOR(MAX(listPos[1,*]) - MIN(listPos[1,*]))

  cornLU = [MIN(listPos[0,*]) , MIN(listPos[1,*])]

  
  IF (deltaH GT 1) AND (deltaV GT 1) THEN BEGIN
     figure = INTARR(deltaH,deltaV)
     FOR i = 0, pair-2 DO BEGIN
        figure[ 0 > FLOOR(listPos[0,i] - cornLU[0]) < (deltaH-1) , 0 > FLOOR(listPos[1,i] - cornLU[1]) < (deltaV-1)  ] = 1
        figureReg = REGULARIZE_ORD1(figure)
     ENDFOR
  ENDIF ELSE figureReg = intarr(3,3) ;<<< qnd desaparece a figura

  Return, PTR_NEW( {type: 2, rotation: rotation, parameters: [l1,l2,C], figure: figureReg, position: obj.position} )
END