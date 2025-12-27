package com.gdn.x.product.model.vo;

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
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ReelProductListingRequestVo {
  private String merchantCode;
  private String keyword;
  private List<String> categoryCodes = new ArrayList<>();
  private Boolean inStock;
  private Boolean tradingProduct;
  private List<String> productSkuList = new ArrayList<>();
}
