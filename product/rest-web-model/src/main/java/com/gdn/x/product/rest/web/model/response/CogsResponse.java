package com.gdn.x.product.rest.web.model.response;

import java.io.Serializable;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CogsResponse extends BaseResponse implements Serializable {

  private static final long serialVersionUID = 1L;

  private String itemSku;
  private String pickupPointCode;
  private double insuredAmount;
}
