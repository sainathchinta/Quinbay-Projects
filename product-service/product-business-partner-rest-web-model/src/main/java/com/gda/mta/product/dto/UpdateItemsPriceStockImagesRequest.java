package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.List;

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
public class UpdateItemsPriceStockImagesRequest {
  private boolean productEditable;
  private boolean synchronize;
  private List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages = new ArrayList<>();
  private List<ProductPriceStockAndImagesRequest> productItems = new ArrayList<>();
  private boolean needCorrection = false;
}
