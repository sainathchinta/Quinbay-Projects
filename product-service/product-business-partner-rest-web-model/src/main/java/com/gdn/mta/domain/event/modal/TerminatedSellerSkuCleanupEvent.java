package com.gdn.mta.domain.event.modal;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
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
@JsonInclude
public class TerminatedSellerSkuCleanupEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 5363823851142595198L;
  private String productCode;
  private String sellerCode;
}
