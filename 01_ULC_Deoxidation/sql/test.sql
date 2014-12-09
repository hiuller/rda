select
  a.num_corr_aci corrida,
  a.dt_prodc_obtd_aci data
from
  tb_dados_corrida_aciaria a
where
  a.dt_prodc_obtd_aci = to_date('23/06/2013', 'dd/mm/yyyy')