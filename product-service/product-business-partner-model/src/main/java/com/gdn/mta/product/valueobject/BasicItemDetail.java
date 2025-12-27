package com.gdn.mta.product.valueobject;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BasicItemDetail {

  private String generatedItemName;
  private String upcCode;
  private String productItemCode;
  private String productItemSku;
  private String merchantSku;

}
