package com.gdn.mta.domain.event.modal;

import java.io.Serializable;
import java.util.List;

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
public class ProductQCRetryEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = 836334375552085562L;
  private String storeId;
  private String username;
  private String productCode;
}