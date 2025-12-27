package com.gdn.x.productcategorybase.domain.event.model;

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
public class TerminatedSellerSkuImageCleanupEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 4011371726476015906L;
  private String productCode;
  private String sellerCode;
}
