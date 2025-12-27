package com.gdn.x.productcategorybase.dto.response;

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
public class SimpleProductCategoryResponse {

  private String catalogCode;
  private CategoryResponse categoryResponse;
}
