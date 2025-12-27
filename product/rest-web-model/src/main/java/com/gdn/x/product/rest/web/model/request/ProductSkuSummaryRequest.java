package com.gdn.x.product.rest.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class ProductSkuSummaryRequest {

  private String productSkuName;
  private List<String> productSkus;
  private List<String> categoryCodes;
  private List<String> brand;
  private String sortOrder = "desc";
  private Boolean isArchived;
}
