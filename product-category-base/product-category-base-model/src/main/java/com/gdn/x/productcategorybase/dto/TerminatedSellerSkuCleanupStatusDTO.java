package com.gdn.x.productcategorybase.dto;

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
public class TerminatedSellerSkuCleanupStatusDTO {

  private String productCode;
  private String sellerCode;
  private String service;
  private String result;
  private boolean publishImageDeletionEvent;
}
