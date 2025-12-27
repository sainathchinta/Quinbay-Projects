package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class FbbCreatePickupPointRequest extends BaseRequest implements Serializable {

  private static final long serialVersionUID = 6039697496761640558L;
  private String businessPartnerCode;
  private String pickupPointId;
  private boolean defaultWarehouse;
  private String itemSku;
  private String ppCode;
  private int stock;
  private boolean buyable;
  private boolean discoverable;
  private Boolean synchronizeStock = true;
}
