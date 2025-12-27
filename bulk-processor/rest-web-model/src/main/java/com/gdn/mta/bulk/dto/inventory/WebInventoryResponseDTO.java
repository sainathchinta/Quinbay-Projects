package com.gdn.mta.bulk.dto.inventory;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class WebInventoryResponseDTO extends BaseResponse {
  private static final long serialVersionUID = -284071249804838914L;
  private String webItemSku;
  private String warehouseItemSku;
  private String webMerchantCode;
  private String warehouseMerchantCode;
  private Integer originalStock;
  private Integer availableStock;
  private int minimumStockAlert;
  private boolean syncStock;
  private String pickupPointCode;
  private boolean notReallocable;
  private String oosSchedulerMarker;
  private String webProductSku;
  private Boolean cnc;
  private Boolean oos;
  private Date oosTimeStamp;
  private Date inStockTimeStamp;
  private String warehouseCode;
  private Boolean fbbPP;
  private String shippingOrigin;
}