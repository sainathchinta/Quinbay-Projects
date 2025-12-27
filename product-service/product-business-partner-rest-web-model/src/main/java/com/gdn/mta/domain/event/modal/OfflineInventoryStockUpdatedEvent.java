package com.gdn.mta.domain.event.modal;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineInventoryStockUpdatedEvent extends GdnBaseDomainEventModel
    implements Serializable {
  private static final long serialVersionUID = 8004714301362286837L;

  private String storeId;
  private String requestId;
  private Date updatedDate;
  private String username;
  private String clientId;
  private String merchantCode;
  private String itemSku;
  private String pickupPointCode;
  private String offlineInventoryId;
  private Integer oldOriginalStock;
  private Integer originalStock;
}
