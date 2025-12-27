package com.gdn.x.productcategorybase.dto.response;

import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductMasterDataResponse extends ProductResponse {
  private static final long serialVersionUID = 322779459590123186L;

  private Set<ProductItemResponse> productItemResponses;
  private List<ProductCategoryResponse> productCategoryResponses;
  private String sellerCode;
}
