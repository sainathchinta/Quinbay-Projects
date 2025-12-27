package com.gda.mta.product.dto.response;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class InProgressProductsBySizeChartCodeResponse extends BaseResponse {
  private String productSku;
  private String sizeChartCode;
}
