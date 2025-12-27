package com.gdn.x.productcategorybase.domain.event.model;

import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.productcategorybase.CategoryChangeEventType;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryDomainEventModel extends GdnBaseDomainEventModel {

  private String id;
  private String name;
  private String categoryCode;
  private Integer sequence;
  private byte[] description;
  private boolean display;
  private Integer logisticAdjustment;
  private boolean warranty;
  private boolean needIdentity;
  private boolean activated;
  private boolean viewable;
  private CatalogDomainEventModel catalog;
  private String parentCategoryId;
  private int dangerousGoodsLevel;
  private boolean umkm;
  private boolean genericTemplateEligible;
  private boolean halalCategory;
  private List<CategoryChangeEventType> categoryChangeTypes;
  private Set<String> categoryChangeTypesV2;
  private OSCDomainEventModel originalSalesCategory;
  private boolean newData;
  private boolean b2bExclusive;
  private List<String> attributeCodes;
}
