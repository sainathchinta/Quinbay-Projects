package com.gdn.mta.bulk.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ProductBrandUpdateRequest implements Serializable {

  private static final long serialVersionUID = 8682743206906533071L;

  private String productCode;
  private String newBrandName;
  private String oldBrandName;
  private String newBrandCode;
  private String oldBrandCode;
  private boolean onlyBrandNameUpdate;
  private boolean brandLevelUpdateRequired;
}
