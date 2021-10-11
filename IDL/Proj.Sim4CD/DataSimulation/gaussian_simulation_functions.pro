

PRO GAUSSIAN_SIMULATION_FUNCTIONS
   print, 'compiled...'
   
END

;------------------------------
;#################################################
FUNCTION EV_DIAGONAL_MATRIX, AutoV

PtrDiagEV = PTRARR(N_ELEMENTS(AutoV))

FOR i = 0, N_ELEMENTS(AutoV)-1 DO BEGIN
   Aux = *AutoV[i]
   Diag = DIAG_MATRIX(Aux.EigenValues)
   PtrDiagEV[i] = PTR_NEW(Diag)
ENDFOR

Return, PtrDiagEV
END



;#################################################
FUNCTION GET_EIGENVECTOR, Sigma

AutoV = PTR_NEW()
FOR i = 0, N_ELEMENTS(Sigma[0,0,*])-1 DO BEGIN 
   
   ;Evec contains the i-th eigenvector on i-th row! (in descending order)
   EVal = EIGENQL(Sigma[*,*,i], /DOUBLE, EIGENVECTORS = EVec)   
    
   Struct = {EigenVector: TRANSPOSE(EVec), EigenValues: EVal}
   AutoV = [AutoV , PTR_NEW(Struct)]
ENDFOR

Return, AutoV[1:*]
END  