package com.gdn.x.productcategorybase.domain.event.model;

import java.io.Serializable;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ImagePathUpdateDomainEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = 3453420199722473173L;
  private String storeId;
  private String productCode;
  private Set<OldAndNewPathDomainEventModel> imageUpdatedPath;
}
