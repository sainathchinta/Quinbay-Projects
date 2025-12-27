package com.gdn.x.productcategorybase.domain.event.model;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PBPProductAttributeBackFillingEventModel extends GdnBaseDomainEventModel
    implements Serializable {
  private static final long serialVersionUID = -1907619910495156814L;
  private String productCode;
  private String attributeCode;
  private String attributeName;
  private String attributeId;
  private String attributeValue;
  private boolean skuValue;
}
