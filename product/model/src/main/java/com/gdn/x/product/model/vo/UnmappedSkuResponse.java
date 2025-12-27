package com.gdn.x.product.model.vo;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class UnmappedSkuResponse extends BaseResponse {
  private String productSku;
  private String productName;
  private String masterCatalogCode;
  private String masterCategoryCode;
  private Date createdDate;
}
