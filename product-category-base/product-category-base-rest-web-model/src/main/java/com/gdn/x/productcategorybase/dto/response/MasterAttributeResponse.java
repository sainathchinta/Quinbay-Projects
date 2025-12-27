package com.gdn.x.productcategorybase.dto.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.request.AttributeSortTypeRequest;

import java.util.Arrays;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterAttributeResponse extends AttributeResponse {

  private static final long serialVersionUID = -6145674210926310627L;
  private String nameEnglish;
  private String dsAttributeName;
  private byte[] descriptionEnglish;
  private AttributeSortTypeRequest sortType;
  private String example;

  public MasterAttributeResponse() {
  }

  public MasterAttributeResponse(String nameEnglish, byte[] descriptionEnglish, AttributeSortTypeRequest sortType,
      String example) {
    this.nameEnglish = nameEnglish;
    this.descriptionEnglish = descriptionEnglish;
    this.sortType = sortType;
    this.example = example;
  }

  public MasterAttributeResponse(String name, String attributeCode, String attributeType, boolean searchAble,
      boolean isBasicView, String nameEnglish, byte[] descriptionEnglish, AttributeSortTypeRequest sortType,
      String example) {
    super(name, attributeCode, attributeType, searchAble, isBasicView);
    this.nameEnglish = nameEnglish;
    this.descriptionEnglish = descriptionEnglish;
    this.sortType = sortType;
    this.example = example;
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

  public Enum getSortType() {
    return sortType;
  }

  public void setSortType(AttributeSortTypeRequest sortType) {
    this.sortType = sortType;
  }

  public String getExample() {
    return example;
  }

  public void setExample(String example) {
    this.example = example;
  }

  public String getDsAttributeName() {
    return dsAttributeName;
  }

  public void setDsAttributeName(String dsAttributeName) {
    this.dsAttributeName = dsAttributeName;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("MasterAttributeResponse{");
    sb.append("nameEnglish='").append(nameEnglish).append('\'');
    sb.append(", descriptionEnglish=").append(Arrays.toString(descriptionEnglish));
    sb.append(", sortType=").append(sortType);
    sb.append(", example='").append(example).append('\'');
    sb.append(", dsAttributeName=").append(dsAttributeName).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
