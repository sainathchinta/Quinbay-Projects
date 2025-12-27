package com.gdn.partners.pcu.internal.client.model.response;


import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class HalalCertificationDetailResponse extends BPJPHListResponse{

  private static final long serialVersionUID = 8532459906131346612L;

  private Long id_reg_prod;
  private String reg_prod_name;
  private String foto_produk;
  private HalalBusinessDetails pelaku_usaha;
  private HalalCeritficationDetails sertifikat;
}
