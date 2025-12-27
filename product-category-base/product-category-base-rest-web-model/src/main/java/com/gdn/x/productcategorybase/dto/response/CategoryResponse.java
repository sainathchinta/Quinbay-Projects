package com.gdn.x.productcategorybase.dto.response;

import java.util.Date;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@ToString
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryResponse extends BaseDTOResponse {

  private static final long serialVersionUID = 6243678682799331788L;

  private String name;
  private String nameEnglish;
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
  private boolean activated = false;
  private boolean viewable = false;
  private CatalogResponse catalog;
  private String parentCategoryId;
  private Integer internalActivationInterval;
  private int dangerousGoodsLevel;
  private boolean umkm;
  private long childCount;
  private boolean genericTemplateEligible;
  private boolean wholesalePriceConfigEnabled;
  private String documentType;
  private String oscId;
  private Date oscUpdatedDate;
  private String oscUpdatedBy;
  private boolean b2bExclusive;
  private boolean halalCategory;
  private OriginalSalesCategoryResponse originalSalesCategoryResponse;

  private boolean bopisEligible;

  public CategoryResponse(String name, String categoryCode, Integer sequence,
      String shortDescription, byte[] description, byte[] defaultDescription, String state,
      boolean display, Integer logisticAdjustment, boolean warranty, boolean needIdentity,
      boolean viewable, CatalogResponse catalog, String parentCategory) {
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
    this.viewable = viewable;
    this.parentCategoryId = parentCategory;
  }

  public CategoryResponse(String name, String nameEnglish, String categoryCode, Integer sequence,
      String shortDescription, byte[] description, byte[] defaultDescription, String state,
      boolean display, Integer logisticAdjustment, boolean warranty, boolean needIdentity,
      boolean viewable, CatalogResponse catalog, String parentCategory) {
    this(name, categoryCode, sequence, shortDescription, description, defaultDescription, state,
        display, logisticAdjustment, warranty, needIdentity, viewable, catalog, parentCategory);
    this.nameEnglish = nameEnglish;
  }

  public CategoryResponse(String name, String categoryCode, Integer sequence,
      String shortDescription, byte[] description, byte[] defaultDescription, String state,
      boolean display, Integer logisticAdjustment, boolean warranty, boolean needIdentity,
      boolean viewable, CatalogResponse catalog, String parentCategory,
      Integer internalActivationInterval) {
    this(name, categoryCode, sequence, shortDescription, description, defaultDescription, state,
        display, logisticAdjustment, warranty, needIdentity, viewable, catalog, parentCategory);
    this.internalActivationInterval = internalActivationInterval;
  }

  public CategoryResponse(String name, String categoryCode, Integer sequence,
      String shortDescription, byte[] description, byte[] defaultDescription, String state,
      boolean display, Integer logisticAdjustment, boolean warranty, boolean needIdentity,
      boolean viewable, CatalogResponse catalog, String parentCategory,
      Integer internalActivationInterval, int dangerousGoodsLevel) {
    this(name, categoryCode, sequence, shortDescription, description, defaultDescription, state,
        display, logisticAdjustment, warranty, needIdentity, viewable, catalog, parentCategory,
        internalActivationInterval);
    this.dangerousGoodsLevel = dangerousGoodsLevel;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

}
