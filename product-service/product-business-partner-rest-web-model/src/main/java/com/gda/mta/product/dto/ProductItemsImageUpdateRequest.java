package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductItemsImageUpdateRequest {

  List<ProductPriceStockAndImagesRequest> variantList;
  List<ProductLevel3SummaryDetailsImageRequest> copyToAllVariantImages;
  String productCode;
  String businessPartnerCode;
  boolean needCorrection;
}
