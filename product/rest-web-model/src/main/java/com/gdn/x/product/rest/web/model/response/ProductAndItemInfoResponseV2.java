package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductAndItemInfoResponseV2 extends BaseResponse {

  private static final long serialVersionUID = 4752103171988261523L;

  private ProductInfoResponse product;
  private ItemInfoResponseV2 item;

  @Override
  public String toString() {
    return String.format("ProductAndItemsInfoResponse [product=%s, item=%s, toString()=%s]",
        product, item, super.toString());
  }

}
