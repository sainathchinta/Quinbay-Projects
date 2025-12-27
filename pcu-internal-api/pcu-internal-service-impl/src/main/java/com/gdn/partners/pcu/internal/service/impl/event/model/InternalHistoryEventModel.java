package com.gdn.partners.pcu.internal.service.impl.event.model;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class InternalHistoryEventModel implements Serializable {

  private static final long serialVersionUID = 2590140528476182373L;
  private String storeId;
  private String productCode;
  private String username;
  private String activity;
  private String notes;
  private long timestamp;
}
