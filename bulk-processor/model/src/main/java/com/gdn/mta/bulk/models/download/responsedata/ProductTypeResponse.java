package com.gdn.mta.bulk.models.download.responsedata;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ProductTypeResponse extends BaseResponse {
  private static final long serialVersionUID = 1368935027957617400L;

  private String name;
  private String description;
}