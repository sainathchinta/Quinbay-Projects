package com.gdn.x.product.rest.web.model.request;

import java.util.List;

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
public class ProductSummaryRequestV2 {
  private String merchantCode;
  private String keyword;
  private List<String> productSkuList;
  private List<String> categoryCodes;
  private List<String> brands;
  private List<String> pickupPointCodes;
  private Double minPrice;
  private Double maxPrice;
  private Boolean isArchived;
  private Boolean off2OnChannelActive;
  private Boolean freeSample;
  private Boolean tradingProduct;
  private String sortOrder;
  private String sortField;
  private Boolean inStock;
}
