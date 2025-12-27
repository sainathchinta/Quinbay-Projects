package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemPickupPointViewConfigChangeEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 860513259229334206L;

  private String itemSku;
  private String merchantCode;
  private String storeId;
  private boolean markForDelete;
  private Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
  private String pickupPointCode;

}
