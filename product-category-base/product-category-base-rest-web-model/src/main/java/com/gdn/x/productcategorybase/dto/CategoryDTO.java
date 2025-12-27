package com.gdn.x.productcategorybase.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;

/**
 * @author febryo.lesmana
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryDTO extends CategoryResponse {

  private static final long serialVersionUID = 1L;

  private long childCount;
  private String nameEnglish;
  private byte[] descriptionEnglish;

  public CategoryDTO() {
    super();
  }

  public CategoryDTO(CategoryResponse categoryResponse, long childCount) {
    super(categoryResponse.getName(), categoryResponse.getCategoryCode(), categoryResponse.getSequence(),
        categoryResponse.getShortDescription(), categoryResponse.getDescription(), categoryResponse.getDefaultDescription(),
        categoryResponse.getState(), categoryResponse.isDisplay(),
        categoryResponse.getLogisticAdjustment(), categoryResponse.isWarranty(), categoryResponse.isNeedIdentity(),
        categoryResponse.isViewable(), categoryResponse.getCatalog(), categoryResponse.getParentCategoryId(),
        categoryResponse.getInternalActivationInterval());
  }

  public CategoryDTO(long childCount, String nameEnglish, byte[] descriptionEnglish) {
    this.childCount = childCount;
    this.nameEnglish = nameEnglish;
    this.descriptionEnglish = descriptionEnglish;
  }

  public CategoryDTO(String name, String categoryCode, Integer sequence, String shortDescription,
      byte[] description, byte[] defaultDescription, String state, boolean display,
      Integer logisticAdjustment, boolean warranty, boolean needIdentity, boolean viewable,
      CatalogResponse catalog, String parentCategory, Integer internalActivationInterval,
      boolean bopisEligible, long childCount, String nameEnglish, byte[] descriptionEnglish) {
    super(name, categoryCode, sequence, shortDescription, description, defaultDescription, state, display,
        logisticAdjustment, warranty, needIdentity, viewable, catalog, parentCategory, internalActivationInterval);
    this.childCount = childCount;
    this.nameEnglish = nameEnglish;
    this.descriptionEnglish = descriptionEnglish;
  }

  public long getChildCount() {
    return this.childCount;
  }

  public void setChildCount(long childCount) {
    this.childCount = childCount;
  }

  public String getNameEnglish() {
    return nameEnglish;
  }

  public void setNameEnglish(String nameEnglish) {
    this.nameEnglish = nameEnglish;
  }

  public byte[] getDescriptionEnglish() {
    return descriptionEnglish;
  }

  public void setDescriptionEnglish(byte[] descriptionEnglish) {
    this.descriptionEnglish = descriptionEnglish;
  }
}
