FUNCTION PRIME_SEQUENCE, n

  seq = [-1]

  num = 2
  WHILE N_ELEMENTS(seq) LE n DO BEGIN

    count = 0
    FOR i = 1, num DO $
      IF (num MOD i) EQ 0 THEN count++

    IF count EQ 2 THEN seq = [seq , num]

    num++
  ENDWHILE

  Return, seq[1:*]
END