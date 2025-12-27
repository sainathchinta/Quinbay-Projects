package com.gdn.mta.bulk.entity;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class FbbL5ItemEventModel extends GdnBaseDomainEventModel {

  private String itemSku;
  private String ppCode;
  private boolean buyable;
  private boolean discoverable;
  private String businessPartnerCode;
  private String internalProcessCode;
}
