package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductL3CommonImageResponse extends BaseResponse {

  private static final long serialVersionUID = -7391255382969622954L;
  private Boolean mainImage;
  private Integer sequence;
  private String locationPath;
  private Boolean activeLocation;
  private Boolean markForDelete;
}
