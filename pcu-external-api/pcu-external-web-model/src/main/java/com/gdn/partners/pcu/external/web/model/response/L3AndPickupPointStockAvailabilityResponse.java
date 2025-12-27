package com.gdn.partners.pcu.external.web.model.response;


import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class L3AndPickupPointStockAvailabilityResponse extends BaseResponse
  implements Serializable {
  private static final long serialVersionUID = 7349962038228442288L;
  private String webProductSku;
  private String pickupPointCode;
  private boolean webStockAvailable;
  private boolean warehouseAvailable;
}
