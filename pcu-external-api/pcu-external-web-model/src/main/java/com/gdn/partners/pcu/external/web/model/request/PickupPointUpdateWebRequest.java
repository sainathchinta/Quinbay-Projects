package com.gdn.partners.pcu.external.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointUpdateWebRequest {
  private String productSku;
  private String businessPartnerCode;
  private boolean differentLocation;
  private boolean markDefaultAddress;
  private String defaultPickupPointCode;
  private List<PickupPointUpdateItemsWebRequest> itemsPickupPoint = new ArrayList<>();
  private boolean needCorrection = false;
  private Boolean fbbActivated;
}
