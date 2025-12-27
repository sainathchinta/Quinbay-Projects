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
public class BasicSizeChartDetailMapResponse extends BaseResponse {
  private static final long serialVersionUID = 2799254067689971161L;
  Map<String, BasicSizeChartDetailResponse> basicSizeChartDetailResponseMap = new HashMap<>();
}
