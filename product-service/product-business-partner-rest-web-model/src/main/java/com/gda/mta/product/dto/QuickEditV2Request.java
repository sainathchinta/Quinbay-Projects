package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.mta.product.enums.ProductLevel3Status;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@Builder
public class QuickEditV2Request {

  private String itemSku;
  private ProductLevel3PriceRequest price;
  private Integer deltaStock;
  private ProductLevel3Status status;
  private ProductLevel3Status cncStatus;
  private Boolean wholeSaleActivated;
  private Boolean off2OnActiveFlag;
  private String sellerSku;
  private String pickupPointCode;
  private Boolean useWarehouseStock;
  private Long version;
  private Boolean cncActive;
  private String itemPickupPointId;
  private boolean fbbActivated;
  private B2bFieldsRequest b2bFieldsRequest;
  private boolean scheduleUpdate;
}
