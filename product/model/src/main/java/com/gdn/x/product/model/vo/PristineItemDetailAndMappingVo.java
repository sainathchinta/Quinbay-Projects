package com.gdn.x.product.model.vo;

import com.gdn.common.web.base.BaseResponse;

import java.util.List;
import java.util.Map;

/**
 * Created by keshashah on 18/12/17.
 */
public class PristineItemDetailAndMappingVo extends BaseResponse {

  private static final long serialVersionUID = 4934173956116906764L;

  private String id;
  private String name;
  private String brand;
  private Map<String, String> attributes;
  private String imageUrl;
  private List<PristineSimilarItemVo> otherPristineItems;

  public PristineItemDetailAndMappingVo(String id, String name, String brand,
      Map<String, String> attributes, String imageUrl,
      List<PristineSimilarItemVo> otherPristineItems) {
    this.id = id;
    this.name = name;
    this.brand = brand;
    this.attributes = attributes;
    this.imageUrl = imageUrl;
    this.otherPristineItems = otherPristineItems;
  }

  public PristineItemDetailAndMappingVo() {
    // No Implementation Needed
  }

  public String getId() {
    return id;
  }

  public void setId(String id) {
    this.id = id;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public String getBrand() {
    return brand;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public Map<String, String> getAttributes() {
    return attributes;
  }

  public void setAttributes(Map<String, String> attributes) {
    this.attributes = attributes;
  }

  public List<PristineSimilarItemVo> getOtherPristineItems() {
    return otherPristineItems;
  }

  public void setOtherPristineItems(List<PristineSimilarItemVo> otherPristineItems) {
    this.otherPristineItems = otherPristineItems;
  }

  public String getImageUrl() {
    return imageUrl;
  }

  public void setImageUrl(String imageUrl) {
    this.imageUrl = imageUrl;
  }
}
