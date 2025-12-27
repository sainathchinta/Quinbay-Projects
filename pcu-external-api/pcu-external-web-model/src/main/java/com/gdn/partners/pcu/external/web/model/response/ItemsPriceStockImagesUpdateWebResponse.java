package com.gdn.partners.pcu.external.web.model.response;

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
public class ItemsPriceStockImagesUpdateWebResponse {
  private boolean productReview;
  private boolean postLive;
  private List<VariantsErrorListWebResponse> variantsErrorList = new ArrayList<>();
  private Long l3Version;

  public ItemsPriceStockImagesUpdateWebResponse(boolean productReview, boolean postLive,
      List<VariantsErrorListWebResponse> variantsErrorList) {
    this.productReview = productReview;
    this.postLive = postLive;
    this.variantsErrorList = variantsErrorList;
  }
}
