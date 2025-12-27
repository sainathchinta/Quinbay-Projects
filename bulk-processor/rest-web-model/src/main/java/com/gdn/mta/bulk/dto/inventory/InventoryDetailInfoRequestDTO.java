package com.gdn.mta.bulk.dto.inventory;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class InventoryDetailInfoRequestDTO extends InventoryRequest implements Serializable {
  private static final long serialVersionUID = 1068863932549198859L;
  private String webItemSku;
  private String webMerchantCode;
  private String pickupPointCode;
}