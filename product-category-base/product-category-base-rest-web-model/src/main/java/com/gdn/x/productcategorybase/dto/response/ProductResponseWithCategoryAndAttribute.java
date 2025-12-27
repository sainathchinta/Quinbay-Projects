package com.gdn.x.productcategorybase.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
public class ProductResponseWithCategoryAndAttribute extends ProductResponse {

  private static final long serialVersionUID = -154436116840524571L;

  private List<SimpleProductCategoryResponse> productCategoryResponses;
  private List<ProductAttributeResponse> productAttributeResponses;
}
