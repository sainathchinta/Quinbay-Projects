package com.gdn.partners.pcu.external.web.model.response;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude
public class XProductL3SolrAutoHealEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -7281944439134732133L;
  private String productSku;
  private String storeId;
  private boolean skipInventoryCallForAtomicUpdate = true;

  public XProductL3SolrAutoHealEventModel(String productSku, String storeId, boolean skipInventoryCallForAtomicUpdate,
      long timestamp) {
    this.setTimestamp(timestamp);
    this.productSku = productSku;
    this.storeId = storeId;
    this.skipInventoryCallForAtomicUpdate = skipInventoryCallForAtomicUpdate;
  }
}
