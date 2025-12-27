package com.gdn.x.productcategorybase.dto.brand;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(JsonInclude.Include.ALWAYS)
public class BrandWipHistoryResponse extends BaseResponse {

  private static final long serialVersionUID = 8028693541519059004L;

  private String brandRequestCode;
  private String brandCode;
  private String description;
  private String state;
}
