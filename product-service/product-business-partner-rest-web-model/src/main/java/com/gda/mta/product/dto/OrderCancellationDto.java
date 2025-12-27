package com.gda.mta.product.dto;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class OrderCancellationDto implements Serializable {
  private static final long serialVersionUID = -113966991325306999L;
  private String storeId;
  private String orderItemStatus;
  private String itemGdnSku;
  private String pickupPointCode;
  private String productGdnSku;
  private String cancellationActor;
  private String statusOFFUpdatedBy;
  private String mtaApiPartner;
}
