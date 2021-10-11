FUNCTION GET_FREE_POS, supportFill, supDims, seed

WHILE 1 DO BEGIN
  posX = FLOOR(RANDOMU(seed)*10L^5) MOD supDims[0]           ;posicao-coluna do centro que a figura vai ocupar
  posY = FLOOR(RANDOMU(seed)*10L^5) MOD supDims[1]           ;posicao-linha do centro que a figura vai ocupar
  IF supportFill[posX,posY] EQ 0 THEN Return, [PosX,PosY]
ENDWHILE

END