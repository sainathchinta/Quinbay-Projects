package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class VatUpdateHistoryDomainEventModel extends GdnBaseDomainEventModel {
  private String requestId;
  private String storeId;
  private String productItemId;
  private String itemCode;
  private String itemName;
  private String updatedBy;
  private String oldValue;
  private String newValue;
}
