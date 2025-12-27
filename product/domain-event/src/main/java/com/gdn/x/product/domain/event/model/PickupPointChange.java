package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.domain.event.enums.PickupPointChangeFields;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@NoArgsConstructor
@AllArgsConstructor
public class PickupPointChange {
  private String code;
  private String businessPartnerCode;
  private String storeId;
  private Set<PickupPointChangeFields> changedFields;
  private boolean cncActivated;
  private boolean archived;
  private String updatedBy;
  private long timestamp;
  private BusinessPartnerFlags flags;
  private Set<String> actionTypes;
}
