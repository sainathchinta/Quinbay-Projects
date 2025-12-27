package com.gdn.x.product.rest.web.model.response;

import org.springframework.data.domain.Page;

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
public class ItemBasicDetailResponse extends BaseResponse {

  private static final long serialVersionUID = -2090786920077721386L;

  private String merchantCode;
  private String itemSku;
  private Page<ItemPickupPointResponse> itemPickupPoints;
  private int productTypeCode;
  private String productTypeName;
  private boolean online;

}
