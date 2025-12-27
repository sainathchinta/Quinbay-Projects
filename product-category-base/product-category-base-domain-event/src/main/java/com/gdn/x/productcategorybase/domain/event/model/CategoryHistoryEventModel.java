package com.gdn.x.productcategorybase.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@EqualsAndHashCode(callSuper = false)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryHistoryEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 8509843868368637383L;
  private String storeId;
  private String userName;
  private String categoryCode;
  private String activity;
  private String oldStatus;
  private String newStatus;
}
