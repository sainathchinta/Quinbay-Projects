package com.gdn.mta.domain.event.modal;

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
public class ProductFbbMigrationEventModel extends GdnBaseDomainEventModel {

  private String identifier;
  private String pickupPointCode;
  private String storeId;
  private String productState;
}
