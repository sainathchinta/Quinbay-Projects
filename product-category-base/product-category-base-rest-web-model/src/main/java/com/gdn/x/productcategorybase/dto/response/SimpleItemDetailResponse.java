package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonInclude
public class SimpleItemDetailResponse extends BaseResponse  {

  String itemCode;
  String productCode;
  String itemName;
}
