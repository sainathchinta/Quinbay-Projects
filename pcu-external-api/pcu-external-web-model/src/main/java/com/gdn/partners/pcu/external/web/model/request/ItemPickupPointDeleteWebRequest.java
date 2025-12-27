package com.gdn.partners.pcu.external.web.model.request;


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
public class ItemPickupPointDeleteWebRequest implements Serializable {

  private static final long serialVersionUID = -4407784130782329478L;
  private String pickupPointId;
  private String itemSku;
}
