package com.gdn.x.product.domain.event.model;

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
public class ExternalSearchReindexToSolrEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 2931174367877492362L;
  private String productSku;
  private String storeId;
  private boolean skipInventoryCallForAtomicUpdate = true;
}
