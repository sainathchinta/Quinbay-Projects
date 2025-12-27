package com.gdn.mta.bulk.models;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
@Builder
public class DormantSellerItemDetail extends GdnBaseDomainEventModel {

  private String itemSku;
  private String itemStatus;
  private String processType;
  private String businessPartnerCode;
  private String dormantSellerEventId;
}
