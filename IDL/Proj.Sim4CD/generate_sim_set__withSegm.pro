@BocaLib.pro
@sim_set_definition.pro

@function_sim_4_cd.pro
@function_sim_4_cd__v2.pro

@function_sim_4_cd__v3__withSegGen.pro

@gaussian_simulation_functions.pro
@generate_ortho_vectors.pro
@generate_ortho_vectors_diagdom.pro
@prime_sequence.pro

@geometric_dictionary.pro
@get_free_pos.pro
@include_object.pro
@geometric_distortion.pro

@get_free_pos.pro
@pca_distortion.pro

@correlation_filter.pro

;2x propositally (IDL's issue)
@pro_gaussian_simulation_phantom_pair_change_detection__v3__withSegm.pro
@pro_gaussian_simulation_phantom_pair_change_detection__v3__withSegm.pro
  @function_build_ascii_roi_from_mask.pro



PRO GENERATE_SIM_SET__withSegm

Seed = 1234567L
Sims = 1
Dims = [300,300]
optDist = 0
randPerc = 0.1

PATH_SIMS = './Sim4CD_datasets/'
PREFIX_PHT = 'Phantom_'
PREFIX_SIM = 'Simulate_'
PREFIX_SEGM = 'Segmentation_'
PATH_IMG = './Sim4CD_datasets/example_input_data/LANDSAT5-TM_Tapajos_2009.tif'
PATH_ROI = './Sim4CD_datasets/example_input_data/roiSet_8classes.txt'
PATH_META = PATH_SIMS+'metafile.txt'

;definicao dos parametros das phantoms
vecDefs = SIM_SET_DEFINITION(Sims,Dims,Seed,corr)

;simulação das phantoms/espectral
FUNCTION_SIM_4_CD__V3__withSegGen, vecDefs, PATH_SIMS, PATH_IMG, PATH_ROI, PREFIX_PHT, PREFIX_SIM, PREFIX_SEGM, PATH_META, seed, optDist, randPerc


Print, 'fim...'
END
