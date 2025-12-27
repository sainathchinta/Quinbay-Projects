package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
@JsonInclude(JsonInclude.Include.ALWAYS)
public class CategorySummaryResponse extends BaseResponse {
  private static final long serialVersionUID = 4505460373619054728L;
  private String categoryCode;
  private String categoryName;
  private String oldCategoryId;
  private String newCategoryId;
  private boolean newCategoryBopisEligible = true;
}
