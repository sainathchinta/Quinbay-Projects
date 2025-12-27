package com.gdn.partners.pbp.dto.offlineitem;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class SuccessOfflineItemResponse implements Serializable {

  private static final long serialVersionUID = -7196700669349250347L;
  private String itemSku;
  private String merchantSku;
  private String pickupPointCode;
  private String externalPickupPointCode;
  private Double listPrice;
  private Double price;
}
