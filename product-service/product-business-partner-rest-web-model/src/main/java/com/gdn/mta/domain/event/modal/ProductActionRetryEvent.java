package com.gdn.mta.domain.event.modal;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductActionRetryEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 7277827007019295110L;
  private String storeId;
  private String productCode;
  private String action;
  private String data;
}