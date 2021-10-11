FUNCTION SIM_SET_DEFINITION, Sims, supDims, Seed, corr

;Seed =  123456L 
;Sims = 100                   ;Number of images simulated
;supDims = [500,500]          ;Support/image dimension

;density >> [0.25 ~ 0.90]     ;Simulation density
;scale >> [0.01 ~ 0.1]        ;Objects scale factor regrading the support 
;var >> [1 ~ 3]               ;Objects maximum expansion value
;zeta,psi >> [0.75 ~ 1.25]    ;Spectral variation regarding the original spectral behavior

vecDefs = REPLICATE({SupDims: [0,0], densityFill: FLOAT(0.0), scaleFact: 0.0, varSize: 0.0, zetaIni: 0.0, zetaEnd: 0.0, psiIni: 0.0, psiEnd: 0.0, correlation: 0}, Sims)

FOR i = 0, Sims-1 DO BEGIN

   ;simulation configuration
   density = 0.1 + (randomu(Seed, 1)) * (0.9 - 0.1)
   scale = 0.10 + (randomu(Seed, 1)) * (0.2 - 0.1)
   var = 1 + (randomu(Seed, 1)) * (3 - 1)
   zeta = 0.75 + (randomu(Seed, 2)) * (1.25 - 0.75)
   psi = 0.75 + (randomu(Seed, 2)) * (1.25 - 0.75)  
   
   ;correlation window (3x3)
   corr = 3
   
   IF zeta[0] LT zeta[1] THEN BEGIN 
      zetaIni = zeta[0]  &  zetaEnd = zeta[1] 
   ENDIF ELSE BEGIN 
      zetaIni = zeta[1]  &  zetaEnd = zeta[0]
   ENDELSE
   IF psi[0] LT psi[1] THEN BEGIN 
      psiIni = psi[0]  &  psiEnd = psi[1]
   ENDIF ELSE BEGIN 
      psiIni = psi[1]  &  psiEnd = psi[0]
   ENDELSE 

   vecDefs[i] = {SupDims: SupDims, densityFill: density[0], scaleFact: scale[0], varSize: var[0], zetaIni: zetaIni, zetaEnd: zetaEnd, psiIni: psiIni, psiEnd: psiEnd, correlation: corr}
ENDFOR

Return, vecDefs
END