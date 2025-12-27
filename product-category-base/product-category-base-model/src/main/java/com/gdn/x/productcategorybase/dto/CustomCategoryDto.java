package com.gdn.x.productcategorybase.dto;

import java.io.Serializable;

import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Created by virajjasani on 11/07/16.
 */
public class CustomCategoryDto implements Serializable {

  private static final long serialVersionUID = 4645452390883382291L;

  private String id;
  private String categoryCode;
  private String name;
  private String parentCategoryId;
  private Integer sequence;
  private String catalogId;
  private boolean markForDelete;
  private boolean activated;


  public CustomCategoryDto(String id, String categoryCode, String name,
      String parentCategoryId, Integer sequence) {
    this.id = id;
    this.categoryCode = categoryCode;
    this.name = name;
    this.parentCategoryId = parentCategoryId;
    this.sequence = sequence;
  }

  public CustomCategoryDto() {
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getParentCategoryId() {
    return parentCategoryId;
  }

  public void setParentCategoryId(String parentCategoryId) {
    this.parentCategoryId = parentCategoryId;
  }

  public Integer getSequence() {
    return sequence;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public String getCatalogId() {
    return catalogId;
  }

  public void setCatalogId(String catalogId) {
    this.catalogId = catalogId;
  }

  public boolean isMarkForDelete() {
    return markForDelete;
  }

  public void setMarkForDelete(boolean markForDelete) {
    this.markForDelete = markForDelete;
  }

  public boolean isActivated() {
    return activated;
  }

  public void setActivated(boolean activated) {
    this.activated = activated;
  }


  @Override
  public String toString() {
    return new ToStringBuilder(this).append("id", id).append("categoryCode", categoryCode)
        .append("name", name).append("parentCategoryId", parentCategoryId)
        .append("sequence", sequence).append("catalogId", catalogId)
        .append("markForDelete", markForDelete).append("activated", activated).toString();
  }
}
