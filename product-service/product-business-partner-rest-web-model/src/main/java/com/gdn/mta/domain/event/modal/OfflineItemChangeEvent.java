package com.gdn.mta.domain.event.modal;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfflineItemChangeEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 2713873349737019384L;

  private String storeId;
  private String clientId;
  private Date updatedDate;
  private String username;
  private String requestId;
  private String uniqueId;
  private String merchantCode;
  private String itemSku;
  private String pickupPointCode;
  private Double listPrice;
  private Double offerPrice;
  private Boolean newData;
  private Double oldListPrice;
  private Double oldOfferPrice;
  private Boolean syncPriceAction;

}
