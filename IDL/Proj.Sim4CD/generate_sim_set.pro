@BocaLib.pro
@sim_set_definition.pro
@function_build_ascii_roi_from_mask.pro

@PRO_GAUSSIAN_SIMULATION_PHANTOM_PAIR_CHANGE_DETECTION.pro
@function_sim_4_cd.pro

@PRO_GAUSSIAN_SIMULATION_PHANTOM_PAIR_CHANGE_DETECTION__v2.pro
@function_sim_4_cd__v2.pro

@PRO_GAUSSIAN_SIMULATION_PHANTOM_PAIR_CHANGE_DETECTION__v3.pro
@function_sim_4_cd__v3.pro

@geometric_dictionary.pro
@get_free_pos.pro
@include_object.pro
@geometric_distortion.pro

@get_free_pos.pro
@pca_distortion.pro

@correlation_filter.pro



PRO GENERATE_SIM_SET

Seed = 1234567L
Sims = 50
Dims = [300,300]
optDist = 1
randPerc = 0.1

PATH_SIMS = '/home/rogerio/Desktop/Sim4CD_datasets/yesDistortion/'
PREFIX_PHT = 'Phantom_'
PREFIX_SIM = 'Simulate_'
PATH_IMG = '/media/rogerio/Dados/DADOS.SR/TAPAJOS_LANDSAT5-TM__Sim4CD/LANDSAT5-TM_Tapajos_2009.tif'
PATH_ROI = '/media/rogerio/Dados/DADOS.SR/TAPAJOS_LANDSAT5-TM__Sim4CD/roiSet_5classes.txt'
PATH_META = PATH_SIMS+'metafile.txt'

;definicao dos parametros das phantoms
vecDefs = SIM_SET_DEFINITION(Sims,Dims,Seed,corr)

;simulação das phantoms/espectral
FUNCTION_SIM_4_CD__V3, vecDefs, PATH_SIMS, PATH_IMG, PATH_ROI, PREFIX_PHT, PREFIX_SIM, PATH_META, seed, optDist, randPerc


Print, 'fim...'
END