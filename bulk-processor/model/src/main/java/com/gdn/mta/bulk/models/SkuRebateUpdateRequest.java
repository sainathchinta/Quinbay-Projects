package com.gdn.mta.bulk.models;

import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author akshay.n
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class SkuRebateUpdateRequest extends BaseRequest {
  private static final long serialVersionUID = 7377546186200076075L;

  private String itemSku;
  private String pickupPointCode;
  private double rebate;
}
