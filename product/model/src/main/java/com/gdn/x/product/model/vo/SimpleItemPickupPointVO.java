package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.model.entity.B2bFields;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class SimpleItemPickupPointVO implements Serializable {

  private static final long serialVersionUID = -2841153116990930595L;
  private String uniqueId;
  private String pickupPointCode;
  private String itemSku;
  private String productSku;
  private Set<Price> prices;
  private Set<ItemViewConfig> itemViewConfigs;
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
