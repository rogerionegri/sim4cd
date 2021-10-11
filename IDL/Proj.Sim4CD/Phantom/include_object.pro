PRO INCLUDE_OBJECT, supportFill, supDims, PtrObject, indexObject

obj = *PtrObject
dimsFig = SIZE(obj.figure,/DIMENSIONS)
FOR i = 0, dimsFig[0]-1 DO BEGIN
   FOR j = 0, dimsFig[1]-1 DO BEGIN
      
      IF obj.figure[i,j] THEN BEGIN
         IF ((i + obj.position[0] - dimsFig[0]/2) GE 0) AND $
            ((i + obj.position[0] - dimsFig[0]/2) LT supDims[0]) AND $
            ((j + obj.position[1] - dimsFig[1]/2) GE 0) AND $
            ((j + obj.position[1] - dimsFig[1]/2) LT supDims[1]) THEN $
            supportFill[ i + obj.position[0] - dimsFig[0]/2   ,   j + obj.position[1] - dimsFig[1]/2  ] = indexObject
      ENDIF
      
   ENDFOR
ENDFOR

END