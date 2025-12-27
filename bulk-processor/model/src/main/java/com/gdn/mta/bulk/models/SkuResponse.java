package com.gdn.mta.bulk.models;

import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author akshay.n
 **/
@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class SkuResponse extends BaseResponse {

  private static final long serialVersionUID = -5396144099470838433L;
  private String itemPickupPointId;
}
