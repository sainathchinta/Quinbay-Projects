package com.gdn.partners.pcu.master.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryWebResponse {
  private String id;
  private String name;
  private String categoryCode;
  private Integer sequence;
  private String shortDescription;
  private byte[] description;
  private byte[] defaultDescription;
  private String state;
  private boolean display;
  private Integer logisticAdjustment;
  private boolean warranty;
  private boolean needIdentity;
  private boolean activated;
  private boolean viewable;
  private CatalogDetailResponse catalog;
  private String parentCategoryId;
  private Integer internalActivationInterval;
  private String nameEnglish;
  private byte[] descriptionEnglish;
  private long childCount;
  private boolean umkm;
  private int dangerousGoodsLevel;
  private boolean wholesalePriceConfigEnabled;
  private boolean genericTemplateEligible;
  private String documentType;
  private OriginalSalesCategoryResponse originalSalesCategory;
  private boolean b2bExclusive;
  private boolean halalCategory;
  private boolean bopisEligible;

  public CategoryWebResponse(String id, String name, String categoryCode, Integer sequence, String shortDescription,
      byte[] description, byte[] defaultDescription, String state, boolean display, Integer logisticAdjustment,
      boolean warranty, boolean needIdentity, boolean activated, boolean viewable, CatalogDetailResponse catalog,
      String parentCategoryId, Integer internalActivationInterval, String nameEnglish, byte[] descriptionEnglish,
      long childCount) {
    this.id = id;
    this.name = name;
    this.categoryCode = categoryCode;
    this.sequence = sequence;
    this.shortDescription = shortDescription;
    this.description = description;
    this.defaultDescription = defaultDescription;
    this.state = state;
    this.display = display;
    this.logisticAdjustment = logisticAdjustment;
    this.warranty = warranty;
    this.needIdentity = needIdentity;
    this.activated = activated;
    this.viewable = viewable;
    this.catalog = catalog;
    this.parentCategoryId = parentCategoryId;
    this.internalActivationInterval = internalActivationInterval;
    this.nameEnglish = nameEnglish;
    this.descriptionEnglish = descriptionEnglish;
    this.childCount = childCount;
  }

  public CategoryWebResponse(String id, String name, String categoryCode, Integer sequence, String shortDescription,
      byte[] description, byte[] defaultDescription, String state, boolean display, Integer logisticAdjustment,
      boolean warranty, boolean needIdentity, boolean activated, boolean viewable, CatalogDetailResponse catalog,
      String parentCategoryId, Integer internalActivationInterval, String nameEnglish, byte[] descriptionEnglish,
      long childCount, int dangerousGoodsLevel, boolean wholesalePriceConfigEnabled) {
    this.id = id;
    this.name = name;
    this.categoryCode = categoryCode;
    this.sequence = sequence;
    this.shortDescription = shortDescription;
    this.description = description;
    this.defaultDescription = defaultDescription;
    this.state = state;
    this.display = display;
    this.logisticAdjustment = logisticAdjustment;
    this.warranty = warranty;
    this.needIdentity = needIdentity;
    this.activated = activated;
    this.viewable = viewable;
    this.catalog = catalog;
    this.parentCategoryId = parentCategoryId;
    this.internalActivationInterval = internalActivationInterval;
    this.nameEnglish = nameEnglish;
    this.descriptionEnglish = descriptionEnglish;
    this.childCount = childCount;
    this.dangerousGoodsLevel = dangerousGoodsLevel;
    this.wholesalePriceConfigEnabled = wholesalePriceConfigEnabled;
  }
}
