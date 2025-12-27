package com.gdn.mta.domain.event.modal;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class PBPAttributeMigrationEventModel extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -5976953565602206748L;
  private String productCode;
  private String attributeCode;
  private String attributeName;
  private String attributeValue;
  private String attributeId;
  private boolean skuValue;
}
