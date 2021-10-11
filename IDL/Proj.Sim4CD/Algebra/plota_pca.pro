PRO PLOTA_PCA, Image, Y, Z, W, at1, at2


  window, 1
  plot, [0,0], xrange = [min([min(image[at1,*,*]),min(Y[at1,*,*])]), max([max(image[at1,*,*]),max(Y[at1,*,*])])], yrange = [min([min(image[at2,*,*]),min(Y[at2,*,*])]) , max([max(image[at2,*,*]),max(Y[at2,*,*])])]
  oplot, image[at1,*,*], image[at2,*,*], color = 200000L, psym = 1 & oplot, Y[at1,*,*], Y[at2,*,*], color = 600000L, psym = 1


  window, 2
  plot, [0,0], xrange = [min([min(image[at1,*,*]),min(Z[at1,*,*])]), max([max(image[at1,*,*]),max(Z[at1,*,*])])], yrange = [min([min(image[at2,*,*]),min(Z[at2,*,*])]) , max([max(image[at2,*,*]),max(Z[at2,*,*])])]
  oplot, image[at1,*,*], image[at2,*,*], color = 200000L, psym = 1 & oplot, Z[at1,*,*], Z[at2,*,*], color = 600000L, psym = 1

  window, 3
  plot, [0,0], xrange = [min([min(image[at1,*,*]),min(W[at1,*,*])]), max([max(image[at1,*,*]),max(W[at1,*,*])])], yrange = [min([min(image[at2,*,*]),min(W[at2,*,*])]) , max([max(image[at2,*,*]),max(W[at2,*,*])])]
  oplot, image[at1,*,*], image[at2,*,*], color = 200000L, psym = 1 & oplot, W[at1,*,*], W[at2,*,*], color = 900000L, psym = 1


END