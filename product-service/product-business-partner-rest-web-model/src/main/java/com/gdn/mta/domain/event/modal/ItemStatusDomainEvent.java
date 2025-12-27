package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.mta.product.enums.ProductStatus;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemStatusDomainEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 7054318082855617809L;
  private ProductStatus productStatus;
  private Set<String> itemSkus;
  private String storeId;

}
