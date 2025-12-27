package com.gdn.mta.bulk.entity;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

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
public class ProductCodeAndCategoryCodeEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -3663637649597555754L;
  private String productCode;
  private String categoryCode;
  private String recatRequestCode;
  private String id;
}