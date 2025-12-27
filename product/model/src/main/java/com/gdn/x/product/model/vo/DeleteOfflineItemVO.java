package com.gdn.x.product.model.vo;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DeleteOfflineItemVO implements Serializable {

  private static final long serialVersionUID = 3235274269764278464L;

  private String itemSku;
  private String pickupPointCode;
  private String productSku;
  private String itemName;
  private boolean cncUpdated;
  private boolean buyableUpdated;
  private boolean discoverableUpdated;
  private boolean success;
  private String errorMessage;

}
