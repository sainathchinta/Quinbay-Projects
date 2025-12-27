package com.gdn.x.productcategorybase.dto.response;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.Map;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CategoryEligibleForSizeChartResponse extends BaseResponse {
  Map<String, Boolean> categoryCodeAndEligibilityMap = new HashMap<>();
}
