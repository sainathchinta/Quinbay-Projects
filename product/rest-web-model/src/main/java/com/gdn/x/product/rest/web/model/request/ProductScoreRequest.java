package com.gdn.x.product.rest.web.model.request;


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
public class ProductScoreRequest {

  private String productCode;
  private String name;
  private String brand;
  private String categoryCode;
  private byte[] description;
  private String uniqueSellingPoint;
  private List<AttributeScoreRequest> productAttributeRequests;
  private String url;
  private List<ItemScoreRequest> itemRequests;
  private boolean synchronised;
}
