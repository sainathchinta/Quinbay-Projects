package com.gdn.x.product.rest.web.model.response;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
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
public class ProductCenterDetailResponse extends BaseResponse {
  private static final long serialVersionUID = 2976428894113949168L;

  private String productName;
  private String status;
  private String imagePath;
  private String sellerCode;
  private String sellerName;
  private Date productCenterUpdatedDate;
  private boolean markForDelete;
}