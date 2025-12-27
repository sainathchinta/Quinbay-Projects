package com.gdn.mta.bulk.dto.product;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class FbbL5CreateDTO {

  private String itemSku;
  private String businessPartnerCode;
  private String ppCode;
  private boolean buyable;
  private boolean discoverable;

}
