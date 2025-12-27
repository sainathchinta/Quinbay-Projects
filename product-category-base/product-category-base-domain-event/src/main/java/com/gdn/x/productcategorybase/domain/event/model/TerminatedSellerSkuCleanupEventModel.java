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
public class TerminatedSellerSkuCleanupEventModel extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -8697373661162903365L;
  private String productCode;
  private String sellerCode;
}
