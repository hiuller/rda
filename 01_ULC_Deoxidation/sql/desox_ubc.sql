select
  a.num_corr_aci,
  a.num_pp,
  ano_mes,
  a.sigla_aco,
  a.cod_local_evento_aci,
  a.chave_resp_trat_aco_pan,
  a.tempo_inic_vacuo_rh,    
  a.dt_desox,
  a.liga,
  a.inst_prim_adic,
  a.peso_prim_adic,
  a.aluminio,
  a.silicio,
  a.celoxA,
  a.tempA,
  a.oppmA,
  a.celoxB,
  a.tempB,
  a.oppmB,
  a.t_liber,
  a.canr_apos_desox,
  sum(case when 
        b.dt_inic_sopro_oxig_obtd is null 
      then 
        0 
      else 
        case when 
          b.dt_inic_sopro_oxig_obtd >= a.dt_desox 
        then 
          1 
        else 
          0 
        end 
      end) n_blow,
      
  sum(case when 
        b.volume_oxigenio_obtd is null 
      then 
        0 
      else 
        case when 
          b.dt_inic_sopro_oxig_obtd >= a.dt_desox 
        then 
          b.volume_oxigenio_obtd
        else 
          0 
        end 
      end) vo2,
      
  sum(case when 
        b.peso_aluminio_obtd is null 
      then 
        0 
      else 
        case when 
          b.dt_inic_sopro_oxig_obtd >= a.dt_desox 
        then 
          b.peso_aluminio_obtd
        else 
          0 
        end         
      end) al_blow
from
  (
    select
      a.num_corr_aci,
      a.num_pp,
      to_char(a.dt_prodc_obtd_aci, 'yyyymm') ano_mes,
      a.sigla_aco,
      a.cod_local_evento_aci,
      a.chave_resp_trat_aco_pan,
      a.tempo_inic_vacuo_rh,    
      a.dt_desox,
      a.liga,
      a.inst_prim_adic,
      a.peso_prim_adic,
      a.aluminio,
      a.silicio,
      a.canr_apos_desox,
      max(a.celoxA)  celoxA,
      sum(a.tempA)   tempA,
      sum(a.oppmA)   oppmA,
      max(a.celoxB)  celoxB,
      sum(a.tempB)   tempB,
      sum(a.oppmB)   oppmB,
      max(a.t_liber) t_liber
    from
      (
        select
          a.*,

          max( case when b.dt_celox < dt_desox then b.dt_celox else null end ) over (partition by a.num_corr_aci) celoxA,  
          decode( b.dt_celox, 
                  max( case when b.dt_celox < dt_desox then b.dt_celox else null end ) over (partition by a.num_corr_aci),
                  b.temp_aco_celox,
                  0 ) tempA,
          decode( b.dt_celox, 
                  max( case when b.dt_celox < dt_desox then b.dt_celox else null end ) over (partition by a.num_corr_aci),
                  b.vol_o2_celox,
                  0 ) oppmA,
                  
          min( case when b.dt_celox > dt_desox then b.dt_celox else null end ) over (partition by a.num_corr_aci) celoxB,
          decode( b.dt_celox, 
                  min( case when b.dt_celox > dt_desox then b.dt_celox else null end ) over (partition by a.num_corr_aci),
                  b.temp_aco_celox,
                  0 ) tempB,
          decode( b.dt_celox, 
                  min( case when b.dt_celox > dt_desox then b.dt_celox else null end ) over (partition by a.num_corr_aci),
                  b.vol_o2_celox,
                  0 ) oppmB,
                  
          decode( b.dt_celox,
                  max(b.dt_celox) over (partition by a.num_corr_aci),
                  b.temp_aco_celox,
                  0 ) t_liber,
                  
          case when a.ult_canr >= a.inst_prim_adic then 1 else 0 end canr_apos_desox
        from
          (
            select
              a.num_corr_aci,
              a.num_pp,
              a.dt_prodc_obtd_aci,
              a.sigla_aco,
              a.cod_local_evento_aci,
              a.chave_resp_trat_aco_pan,
              a.tempo_inic_vacuo_rh,    
              (a.tempo_inic_vacuo_rh)+(sum(a.prim_adic)/86400) dt_desox,
              /*--https://forums.oracle.com/message/3581224
              -- MichaelS Guru MichaelS 25/06/2009 12:18*/
              rtrim(xmlagg(xmlelement(e, a.prim_liga, ' ')).extract('//text()').extract('//text()') ,',') liga,
              sum(a.prim_adic) inst_prim_adic,
              sum(a.prim_peso) peso_prim_adic,
              sum(a.al) aluminio,
              sum(a.si) silicio,
              max(a.inst_canivete) ult_canr
            from
              (
                select
                  a.num_corr_aci,
                  a.num_pp,
                  a.dt_prodc_obtd_aci,              
                  a.sigla_aco,
                  b.cod_local_evento_aci,
                  d.chave_resp_trat_aco_pan,
                  b.tempo_inic_vacuo_rh,
                  decode(c.DESCR_ABREV_FE_LIGA, 'CANR', c.INSTANTE_OBTD_ADIC_PAN, 0) inst_canivete,
                  decode(c.INSTANTE_OBTD_ADIC_PAN, min(c.INSTANTE_OBTD_ADIC_PAN) over (partition by a.num_corr_aci), c.INSTANTE_OBTD_ADIC_PAN, 0) prim_adic,
                  decode(c.INSTANTE_OBTD_ADIC_PAN, min(c.INSTANTE_OBTD_ADIC_PAN) over (partition by a.num_corr_aci), c.DESCR_ABREV_FE_LIGA, null) prim_liga,
                  decode(c.INSTANTE_OBTD_ADIC_PAN, min(c.INSTANTE_OBTD_ADIC_PAN) over (partition by a.num_corr_aci), c.PESO_ADIC_OBTD_PAN    , 0) prim_peso,
                  case when c.DESCR_ABREV_FE_LIGA in ('ALGO', 'ALGR', 'ALG2') then c.PESO_ADIC_OBTD_PAN else 0 end al,
                  case when c.DESCR_ABREV_FE_LIGA in ('SIBC', 'SIBR'        ) then c.PESO_ADIC_OBTD_PAN else 0 end si
                from
                  tb_dados_corrida_aciaria a
                    inner join tb_dados_corrida_rh b
                      on (a.num_corr_aci = b.num_corr_aci
                      and a.num_pp = b.num_pp)
                    inner join tb_dados_adicoes_ligas_obtd c
                      on (a.num_corr_aci = c.num_corr_aci
                      and b.cod_local_evento_aci = c.cod_local_evento_aci
                      and b.num_trat_aco_pan = c.num_trat_aco_pan)
                    inner join tb_dados_trat_aco_pan d
                      on (a.num_corr_aci = d.num_corr_aci
                      and b.cod_local_evento_aci = d.cod_local_evento_aci
                      and b.num_trat_aco_pan = d.num_trat_aco_pan)
                where
                  a.num_ld > 3
                  and substr(a.sigla_aco, 5, 2) = 'AA'
                  and b.NUM_TRAT_ACO_PAN = 1 -- apenas o primeiro tratamento
                  and a.dt_prodc_obtd_aci between
                    to_date('01/01/2013', 'dd/mm/yyyy')
                      and
                    to_date('19/12/2013', 'dd/mm/yyyy')
                  and to_number(substr(a.sigla_aco, 7, 6))/1000 < 1
                  and c.DESCR_ABREV_FE_LIGA in ('ALGO', 'ALGR', 'ALG2', 'SIBC', 'SIBR', 'CANR')
              ) a
            group by
              a.num_corr_aci,
              a.num_pp,
              dt_prodc_obtd_aci,
              a.sigla_aco,
              a.cod_local_evento_aci,
              a.chave_resp_trat_aco_pan,
              a.tempo_inic_vacuo_rh
          ) a
            inner join tb_dados_celox b
              on (a.num_corr_aci = b.num_corr_aci
            and b.cod_local_evento_aci = b.cod_local_evento_aci
            and b.num_trat_aco_pan = b.num_trat_aco_pan
            and to_char(a.tempo_inic_vacuo_rh, 'yyyy') = to_char(b.dt_celox, 'yyyy'))
        /*where
          --a.num_corr_aci = 488600*/
      ) a 
    group by
      a.num_corr_aci,
      a.num_pp,
      to_char(a.dt_prodc_obtd_aci, 'yyyymm'),
      a.sigla_aco,
      a.cod_local_evento_aci,
      a.chave_resp_trat_aco_pan,
      a.tempo_inic_vacuo_rh,    
      a.dt_desox,
      a.liga,
      a.inst_prim_adic,
      a.peso_prim_adic,
      a.aluminio,
      a.silicio,
      a.canr_apos_desox
  ) a    
  left join tb_dados_inj_oxig_obtd_rh b
      on (a.num_corr_aci = b.num_corr_aci
      and a.num_pp = b.num_pp
      and a.cod_local_evento_aci = b.cod_local_evento_aci
      and 1 = b.num_trat_aco_pan)
group by
  a.num_corr_aci,
  a.num_pp,
  a.ano_mes,
  a.sigla_aco,
  a.cod_local_evento_aci,
  a.chave_resp_trat_aco_pan,
  a.tempo_inic_vacuo_rh,    
  a.dt_desox,
  a.liga,
  a.inst_prim_adic,
  a.peso_prim_adic,
  a.aluminio,
  a.silicio,
  a.celoxA,
  a.tempA,
  a.oppmA,
  a.celoxB,
  a.tempB,
  a.oppmB,
  a.t_liber,
  a.canr_apos_desox
order by
  a.num_corr_aci 