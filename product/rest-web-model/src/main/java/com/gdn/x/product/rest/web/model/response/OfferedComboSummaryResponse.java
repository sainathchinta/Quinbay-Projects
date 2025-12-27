package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.List;
import java.util.Map;

/**
 * Created by w.william on 2/14/2018.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class OfferedComboSummaryResponse extends BaseResponse {
  private static final long serialVersionUID = 1L;

  private String name;
  private String brand;
  private String itemSku;
  private Map<String, String> attributes;
  private String imageUrl;
  private List<Attribute> otherAttributes;

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

  public String getImageUrl() {
    return imageUrl;
  }

  public void setImageUrl(String imageUrl) {
    this.imageUrl = imageUrl;
  }

  public String getItemSku() {
    return itemSku;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  public List<Attribute> getOtherAttributes() {
    return otherAttributes;
  }

  public void setOtherAttributes(
      List<Attribute> otherAttributes) {
    this.otherAttributes = otherAttributes;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("name", name).append("brand", brand)
        .append("itemSku", itemSku).append("attributes", attributes).append("imageUrl", imageUrl)
        .append("otherAttributes", otherAttributes).toString();
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }
}
