FUNCTION CORRELATION_FILTER, Image, Mat

MatDim = GET_DIMENSIONS(Mat)
Dims = GET_DIMENSIONS(Image)

Filtered = Image*0.0

FOR k = 0, Dims[0]-1 DO BEGIN
   FOR i = 0, Dims[1]-1 DO BEGIN
      FOR j = 0, Dims[2]-1 DO BEGIN
      
         sum = 0
         count = 0
         FOR xw = i-(1*MatDim[1]/2) , i+(MatDim[1]/2) DO BEGIN
            FOR yw = j-(MatDim[1]/2) , j+(MatDim[1]/2) DO BEGIN
               
               IF (xw GE 0)AND(xw LT Dims[1])AND(yw GE 0)AND(yw LT Dims[2]) THEN BEGIN
                  sum += Image[k,xw,yw]
                  count++
               ENDIF
            
            ENDFOR
         ENDFOR  
         
         Filtered[k,i,j] = sum/FLOAT(count)
      
      ENDFOR
   ENDFOR
ENDFOR

Return, Filtered
END