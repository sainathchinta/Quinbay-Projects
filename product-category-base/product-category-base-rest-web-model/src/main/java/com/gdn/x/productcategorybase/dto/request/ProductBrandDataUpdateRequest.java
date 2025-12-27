package com.gdn.x.productcategorybase.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductBrandDataUpdateRequest extends BaseRequest implements Serializable {

  private String productCode;
  private String newBrandName;
  private String oldBrandName;
  private String newBrandCode;
  private String oldBrandCode;
  private boolean onlyBrandNameUpdate;
  private boolean brandLevelUpdateRequired;
  private Set<String> businessPartnerCodes = new HashSet<>();
}