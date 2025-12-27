package com.gda.mta.product.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PickupPointUpdateRequest extends BaseRequest {

  private static final long serialVersionUID = -8611389870882810163L;

  private String productSku;
  private String businessPartnerCode;
  private boolean differentLocation;
  private boolean markDefaultAddress;
  private String defaultPickupPointCode;
  private List<PickupPointRequest> itemsPickupPoint = new ArrayList<>();
  private boolean needCorrection = false;
  private Boolean fbbActivated;
  private boolean isFbbMigration;
}
