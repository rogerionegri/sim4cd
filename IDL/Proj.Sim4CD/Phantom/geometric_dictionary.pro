;Esta função gera uma dada figura geométrica e respectivos atributos de representacao (rotacao, escala, parametros associados, etc)
FUNCTION GEOMETRIC_DICTIONARY, objInd, posXY, supDims, scale, var, seed

CASE objInd OF
  0: ptrObj = RECTANGLE(scale,SupDims,posXY,var,seed)
  1: ptrObj = CIRCLE(scale,SupDims,posXY,var,seed)
  2: ptrObj = TRIANGLE(scale,SupDims,posXY,var,seed)
  ELSE: stop ;<<<caso inesperado...
ENDCASE

Return, ptrObj
END



;-------------------------------------------------
FUNCTION RECTANGLE, scale,SupDims,posXY,var,seed
   
   boundBox  = [FLOOR(scale*supDims[0]),FLOOR(scale*supDims[1])]
   
   l1 = FLOOR(MIN(boundBox) * (1 + RANDOMU(seed)*var))
   l2 = FLOOR(MAX(boundBox) * (1 + RANDOMU(seed)*var))
   rotation = RANDOMU(seed,1) * !PI
   
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
   
;   figure = INTARR(deltaH,deltaV)
;   FOR i = 0, pair-2 DO BEGIN
;      figure[ 0 > FLOOR(listPos[0,i] - cornLU[0]) < (deltaH-1) , 0 > FLOOR(listPos[1,i] - cornLU[1]) < (deltaV-1)  ] = 1
;   ENDFOR
;   figureReg = REGULARIZE_ORD1(figure)   
   
   IF (deltaH GT 1) AND (deltaV GT 1) THEN BEGIN
     figure = INTARR(deltaH,deltaV)
     FOR i = 0, pair-2 DO BEGIN
       figure[ 0 > FLOOR(listPos[0,i] - cornLU[0]) < (deltaH-1) , 0 > FLOOR(listPos[1,i] - cornLU[1]) < (deltaV-1)  ] = 1
       figureReg = REGULARIZE_ORD1(figure)
     ENDFOR
   ENDIF ELSE figureReg =  intarr(3,3) ;<<< qnd desaparece a figura
   
   
   
   
   Return, PTR_NEW( {type: 0, rotation: rotation, parameters: [l1,l2], figure: figureReg, position: posXY} )
END



;-------------------------------------------------
FUNCTION CIRCLE, scale,SupDims,posXY,var,seed

   radius = FLOOR((scale*supDims[0]) * (1 + RANDOMU(seed)*var))
   center = [FLOOR((2*radius+1)/2) , FLOOR((2*radius+1)/2)]
   
   figure = INTARR(2*radius+1 , 2*radius+1)
   FOR i = 0, 2*radius DO BEGIN
      FOR j = 0, 2*radius DO BEGIN
         IF NORM( [i,j] - center ) LE radius THEN figure[i,j] = 1
      ENDFOR
   ENDFOR

   Return, PTR_NEW( {type: 1, rotation: 0, parameters: [radius], figure: figure, position: posXY} )
END



;-------------------------------------------------
FUNCTION TRIANGLE, scale,SupDims,posXY,var,seed

  boundBox  = [FLOOR(scale*supDims[0]),FLOOR(scale*supDims[1])]

  l1 = FLOOR(MIN(boundBox) * (1 + RANDOMU(seed)*var))
  l2 = FLOOR(MAX(boundBox) * (1 + RANDOMU(seed)*var))

  rotation = RANDOMU(seed,1) * !PI

  R = [ [cos(rotation) , sin(rotation)] , [-sin(rotation) , cos(rotation)] ]

  bbh = l1
  bbv = l2
  center = [bbh/2 , bbv/2]
  
  A = [0,0]
  B = [l1,0]
  
  minAngle = 45*(!PI/180)
  WHILE 1 DO BEGIN
     CX = FLOOR(RANDOMU(seed)*10L^5) MOD l1
     CY = FLOOR(RANDOMU(seed)*10L^5) MOD l2
     C = [CX , CY]
     theta = ACOS( (TOTAL( (A-B)*(A-C) )) / ( NORM(A-B) * NORM(A-C) ))
     
     IF theta GT minAngle THEN BREAK
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

;  figure = INTARR(deltaH,deltaV)
;  FOR i = 0, pair-2 DO BEGIN
;    figure[ 0 > FLOOR(listPos[0,i] - cornLU[0]) < (deltaH-1) , 0 > FLOOR(listPos[1,i] - cornLU[1]) < (deltaV-1)  ] = 1
;  ENDFOR
;  figureReg = REGULARIZE_ORD1(figure)

   IF (deltaH GT 1) AND (deltaV GT 1) THEN BEGIN
      figure = INTARR(deltaH,deltaV)
      FOR i = 0, pair-2 DO BEGIN
         figure[ 0 > FLOOR(listPos[0,i] - cornLU[0]) < (deltaH-1) , 0 > FLOOR(listPos[1,i] - cornLU[1]) < (deltaV-1)  ] = 1
         figureReg = REGULARIZE_ORD1(figure)
      ENDFOR
   ENDIF ELSE figureReg =  intarr(3,3) ;<<< qnd desaparece a figura

  Return, PTR_NEW( {type: 2, rotation: rotation, parameters: [l1,l2,C], figure: figureReg, position: posXY} )
END




;-------------------------------------------------
;-------------------------------------------------
;-------------------------------------------------
FUNCTION REGULARIZE_ORD1, fig

dims = SIZE(fig,/DIMENSIONS)

regFig = fig
FOR i = 0, dims[0]-1 DO BEGIN
   FOR j = 0, dims[1]-1 DO BEGIN
     
      IF ~fig[i,j] THEN BEGIN
         IF ( ((i-1) GE 0) AND (i+1 LT dims[0]) AND ((j-1) GE 0) AND (j+1 LT dims[1]) ) THEN BEGIN
            count = 0
            IF fig[i-1,j] EQ 1 THEN count++
            IF fig[i+1,j] EQ 1 THEN count++
            IF fig[i,j-1] EQ 1 THEN count++
            IF fig[i,j+1] EQ 1 THEN count++
          
            IF count GE 2 THEN regFig[i,j] = 1 
         ENDIF
      ENDIF
    
   ENDFOR
ENDFOR

Return, regFig
END