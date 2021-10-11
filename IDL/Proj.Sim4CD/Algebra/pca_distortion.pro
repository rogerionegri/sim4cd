@BocaLib.pro
@gaussian_simulation_functions.pro

FUNCTION PCA_DISTORTION, Image, seed

  Dims = GET_DIMENSIONS(Image)
  m = DBLARR(Dims[0])
  FOR i = 0L, Dims[0]-1 DO $
    m[i] = TOTAL(Image[i,*,*])/(1.0*N_ELEMENTS(Image[i,*,*]))

  MM = transpose(m) ## (m)

  C = DBLARR(Dims[0],Dims[0])
  FOR i = 0L, Dims[1]-1 DO BEGIN
    FOR j = 0L, Dims[2]-1 DO BEGIN
      C += transpose(Image[*,i,j]) ## Image[*,i,j] - MM
    ENDFOR
  ENDFOR
  C = C/(N_ELEMENTS(Image[0,*,*])*1.0)


  eigenvalues = EIGENQL(C, EIGENVECTORS = evecs, RESIDUAL = residual)
  A = C*0.0D
  FOR i = 0L, N_ELEMENTS(eigenvalues)-1 DO A[*,i] = evecs[*,i]

  ;OV = GENERATE_ORTHO_VECTORS(Dims[0], seed)
  OV = GENERATE_ORTHO_VECTORS_DIAGDOM(Dims[0], seed)
  K = OV # A
  ;ml = m * (0.5 + 1.0*randomu(seed,Dims[0]))
  ml = m


  Y = Image*0.0
  FOR i = 0L, Dims[1]-1 DO BEGIN
    FOR j = 0L, Dims[2]-1 DO Y[*,i,j] = A ## Transpose(Image[*,i,j] - m)
  ENDFOR


  W = Image*0.0
  FOR i = 0L, Dims[1]-1 DO BEGIN
    FOR j = 0L, Dims[2]-1 DO W[*,i,j] = ml + Transpose(K) ## Y[*,i,j]
  ENDFOR


Return, W
END