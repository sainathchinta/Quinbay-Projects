package com.gdn.x.product.rest.web.model.response;

import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.gdn.common.web.base.BaseResponse;

/**
 * Created by keshashah on 18/12/17.
 */
public class PristineItemDetailAndMapping extends BaseResponse {

  private static final long serialVersionUID = 4934173956116906764L;

  private String id;
  private String name;
  private String brand;
  private Map<String, String> attributes;
  private List<PristineSimilarItem> otherPristineItems;
  private String imageUrl;

  public PristineItemDetailAndMapping(String id, String name, String brand,
      Map<String, String> attributes, List<PristineSimilarItem> otherPristineItems,
      String imageUrl) {
    this.id = id;
    this.name = name;
    this.brand = brand;
    this.attributes = attributes;
    this.otherPristineItems = otherPristineItems;
    this.imageUrl = imageUrl;
  }

  public PristineItemDetailAndMapping() {
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

  public List<PristineSimilarItem> getOtherPristineItems() {
    return otherPristineItems;
  }

  public void setOtherPristineItems(List<PristineSimilarItem> otherPristineItems) {
    this.otherPristineItems = otherPristineItems;
  }

  public String getImageUrl() {
    return imageUrl;
  }

  public void setImageUrl(String imageUrl) {
    this.imageUrl = imageUrl;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("id", id).append("name", name).append("brand", brand)
        .append("attributes", attributes).append("otherPristineItems", otherPristineItems)
        .append("imageUrl", imageUrl).toString();
  }
}
