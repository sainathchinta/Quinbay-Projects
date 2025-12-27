package com.gdn.x.productcategorybase.domain.event.model;

import java.io.Serial;
import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@AllArgsConstructor
@NoArgsConstructor
public class InternalProductHistoryEventModel extends GdnBaseDomainEventModel implements Serializable {

  @Serial
  private static final long serialVersionUID = -5389621626072863413L;

  private String storeId;
  private String productCode;
  private String username;
  private String activity;
  private String notes;
}
