package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.*;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class DeletedProductItems extends BaseRequest {

  private static final long serialVersionUID = 2600782403754246476L;
  private String itemSku;
  private String itemCode;
}