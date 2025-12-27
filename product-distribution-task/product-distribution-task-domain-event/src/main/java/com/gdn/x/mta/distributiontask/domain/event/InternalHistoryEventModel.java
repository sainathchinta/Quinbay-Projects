package com.gdn.x.mta.distributiontask.domain.event;

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
public class InternalHistoryEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 8568683873389920832L;
  private String storeId;
  private String productCode;
  private String username;
  private String activity;
  private String notes;
}
