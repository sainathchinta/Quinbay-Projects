package com.gdn.x.product.model.vo;

import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ItemPickupPointTransactionResponse extends BaseResponse {
  private String itemSku;
  private String itemCode;
  private ProductItemDetailVO itemDetail;
}
