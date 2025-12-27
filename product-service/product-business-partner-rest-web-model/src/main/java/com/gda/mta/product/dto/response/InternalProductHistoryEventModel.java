package com.gda.mta.product.dto.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class InternalProductHistoryEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 8270847653230474729L;
  private String storeId;
  private String productCode;
  private String username;
  private String activity;
  private String notes;
}
