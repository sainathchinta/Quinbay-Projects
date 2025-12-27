package com.gdn.x.product.domain.event.model;

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
public class WholesalePriceActivatedOrDeactivatedEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 1597163108491343402L;
  private String itemSku;
  private boolean wholesalePriceActivated;
  private String merchantCode;
}
