package com.gdn.x.product.rest.web.model.dto;

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
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
@Builder
public class ProductCenterHistoryResponse extends BaseResponse {

  private String productSku;
  private Date updatedDate;
  private String activity;
  private String description;
  private String updatedBy;
}
