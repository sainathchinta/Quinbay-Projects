package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.model.entity.B2bFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleItemPickupPointResponseDTO implements Serializable {

  private static final long serialVersionUID = 3467817319442640378L;
  private String uniqueId;
  private String pickupPointCode;
  private String itemSku;
  private String productSku;
  private Set<PriceDTO> prices;
  private Set<ItemViewConfigDTO> itemViewConfigs;
  private Set<String> activePromoBundlings;
  private boolean promoBundling;
  private boolean cncActive;
  private boolean fbbActivated;
  private boolean archived;
  private boolean suspended;
  private boolean markForDelete;
  private boolean off2OnActive;
  private B2bFields b2bFields;
}
