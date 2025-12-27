package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.model.entity.B2bFields;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointDataChangeEventModel extends ProductBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -6391831695666874911L;

  private String merchantCode;
  private String itemSku;
  private String productSku;
  private String merchantSku;
  private Set<Price> price = new HashSet<Price>();
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<ItemViewConfig>();
  private String pickupPointCode;
  private String externalPickupPointCode;
  private boolean cncActive;
  private boolean fbbActivated;
  private boolean wholesalePriceExists;
  private boolean merchantPromoDiscount;
  private boolean isFlashSaleActive;
  private boolean promoBundling;
  private boolean markForDelete;
  private List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes =
      new ArrayList<ItemPickupPointChangeEventType>();
  private boolean pureCNCStatusChange;
  private Set<String> activePromoBundlings;
  private Boolean newData;
  private String offlineItemId;
  private boolean forceReview;
  private List<String> sellerChannel = new ArrayList<>();
  private B2bFields b2bFields;
  private List<String> itemPickupPointChangeEventTypesV2 = new ArrayList<>();
  private boolean distribution;
}
