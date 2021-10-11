FUNCTION GENERATE_ORTHO_VECTORS_DIAGDOM, n, seed

factor = 10*n
WHILE 1 DO BEGIN

   WHILE 1 DO BEGIN
      A = INVERT((RANDOMU(seed,n)*factor # TRANSPOSE(RANDOMU(seed,n))*factor)/factor, status, /DOUBLE)
      IF status EQ 0 THEN BREAK
   ENDWHILE

   QRFAC, A, R, QMATRIX=q1
   
   q2 = DIAG_DOMINANTE(q1)
   
   res = CHECK_DIAGDOM(q2)
   
   IF res THEN BREAK
   
ENDWHILE

  H = DBLARR(n,n)
  FOR i = 0, n-1 DO H[i,i] = 1.0/q2[i,i]
  q3 = q2 ## H

  Return, q3
END



;-------------------------------------------
FUNCTION DIAG_DOMINANTE__V2, M

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


;-------------------------------------------
FUNCTION CHECK_DIAGDOM, M
   n = SIZE(M,/DIMENSIONS)
   FOR i = 0, n[0]-1 DO BEGIN ;...caminha pela diagonal
      FOR j = 0, n[0]-1 DO BEGIN ;...caminha pelas colunas que o elemento da diagonal ocupa M[i,i]
         IF (i NE j) AND (ABS(M[i,i]) LE ABS(M[j,i])) THEN Return, 0       
      ENDFOR
      
      FOR k = 0, n[0]-1 DO BEGIN ;...caminha pelas linhas que o elemento da diagonal ocupa M[i,i]
         IF (i NE k) AND (ABS(M[i,i]) LE ABS(M[i,k])) THEN Return, 0
      ENDFOR
   ENDFOR
   Return, 1
END