package com.gdn.partners.pcu.external.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
@JsonIgnoreProperties(ignoreUnknown = true)
public class UpdateItemsPriceStockImagesWebRequest {
  private boolean productEditable;
  private boolean synchronize;
  private List<ProductLevel3SummaryDetailsImageWebRequest> copyToAllVariantImages = new ArrayList();
  private List<ProductPriceStockAndImagesWebRequest> productItems = new ArrayList();
  private boolean needCorrection = false;
}
