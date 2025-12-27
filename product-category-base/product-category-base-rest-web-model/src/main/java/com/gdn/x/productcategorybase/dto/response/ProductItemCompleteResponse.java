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
public class ProductItemCompleteResponse extends ProductItemResponse{

  private static final long serialVersionUID = -2164668896705674483L;

  private ProductResponseWithCategoryAndAttribute productResponse;
  private List<ProductItemAttributeValueResponse> productItemAttributeValueResponses;
}
