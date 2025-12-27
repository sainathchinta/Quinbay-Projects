package com.gdn.x.productcategorybase.dto.response;

import java.io.Serializable;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class BasicSizeChartDetailResponse extends BaseResponse implements Serializable {
  private static final long serialVersionUID = -286398709702877085L;
  private String sizeChartName;
  private String businessPartnerCode;
}
