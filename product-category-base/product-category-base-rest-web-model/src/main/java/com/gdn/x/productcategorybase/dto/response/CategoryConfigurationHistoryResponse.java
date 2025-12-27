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
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class CategoryConfigurationHistoryResponse extends BaseResponse {

  String categoryCode;
  String categoryName;
  String activity;
  String oldValue;
  String newValue;
  Date updatedDate;
  String updatedBy;
  Date createdDate;
  String createdBy;
}
