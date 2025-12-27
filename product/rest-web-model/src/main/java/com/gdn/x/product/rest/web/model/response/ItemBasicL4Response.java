package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@Builder
@JsonInclude
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class ItemBasicL4Response extends BaseResponse {
  private String productSku;
  private String itemSku;
  private String upcCode;
  private String itemName;
}
