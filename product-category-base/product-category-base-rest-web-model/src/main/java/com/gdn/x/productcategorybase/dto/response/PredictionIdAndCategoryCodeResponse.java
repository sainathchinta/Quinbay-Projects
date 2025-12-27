package com.gdn.x.productcategorybase.dto.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PredictionIdAndCategoryCodeResponse extends BaseResponse {
  private String predictionId;
  private List<CategoryCodeAndNameResponse> categoryCodeAndNameResponseList;
}
