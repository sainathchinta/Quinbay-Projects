package com.gdn.x.productcategorybase.dto.response;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class SimpleListStringResponse extends BaseResponse {
  private List<String> values;
}
