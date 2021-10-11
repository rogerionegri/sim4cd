


FUNCTION GENERATE_ORTHO_VECTORS, n, seed

;WHILE 1 DO BEGIN
factor = 10*n

WHILE 1 DO BEGIN
  A = INVERT((RANDOMU(seed,n)*factor # TRANSPOSE(RANDOMU(seed,n))*factor)/factor, status, /DOUBLE) 
  IF status EQ 0 THEN BREAK
ENDWHILE

QRFAC, A, R, QMATRIX=q

;nn = WHERE(q LT 0)
;
;IF nn[0] EQ -1 THEN BREAK
;
;ENDWHILE

qq = DIAG_DOMINANTE(q)

Return, qq
END



;-------------------------------------------
FUNCTION DIAG_DOMINANTE, M

n = SIZE(M,/DIMENSIONS)
ddM = M
temp = MAKE_ARRAY(1,n[0], TYPE = SIZE(M,/TYPE))
FOR i = 0, n[0]-1 DO BEGIN
  FOR j = i+1, n[0]-1 DO BEGIN
    IF ABS(ddM[i,i]) LT ABS(ddM[j,i]) THEN BEGIN
      temp[*] = ddM[i,*]
      ddM[i,*] = ddM[j,*]
      ddM[j,*] = temp[*] 
    ENDIF
  ENDFOR
ENDFOR


Return, ddM 
END