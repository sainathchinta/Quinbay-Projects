package com.gdn.x.productcategorybase.dto.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
@JsonIgnoreProperties(ignoreUnknown =  true)
public class MerchantConfigurationFilterResponse extends BaseResponse {

  String merchantCode;
  String merchantName;
  String categoryName;
  String reviewConfig;
  Date createdDate;
  String createdBy;
}
