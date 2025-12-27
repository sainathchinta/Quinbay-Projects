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
public class ProductMigrationRequestEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -6325544696961232136L;
  private List<String> productCodes;
  private List<String> productSkuList;
  private int threadCount;
}
