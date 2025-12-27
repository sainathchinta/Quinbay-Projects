package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

import java.util.List;
import java.util.Map;

/**
 * Created by w.william on 2/21/2018.
 */
public class OfferedSummaryVo extends BaseResponse {

  private static final long serialVersionUID = 1L;

  private String id;
  private String name;
  private String brand;
  private String itemSku;
  private Map<String, String> attributes;
  private String imageUrl;
  private List<AttributeVo> otherAttributes;

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public Map<String, String> getAttributes() {
    return attributes;
  }

  public String getBrand() {
    return brand;
  }

  @Override
  public String getId() {
    return id;
  }

  public String getImageUrl() {
    return imageUrl;
  }

  public String getItemSku() {
    return itemSku;
  }

  public String getName() {
    return name;
  }

  public List<AttributeVo> getOtherAttributes() {
    return otherAttributes;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setAttributes(Map<String, String> attributes) {
    this.attributes = attributes;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  @Override
  public void setId(String id) {
    this.id = id;
  }

  public void setImageUrl(String imageUrl) {
    this.imageUrl = imageUrl;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setOtherAttributes(List<AttributeVo> otherAttributes) {
    this.otherAttributes = otherAttributes;
  }

  public void setItemSku(String itemSku) {
    this.itemSku = itemSku;
  }

  @Override
  public String toString() {
    return String.format(
        "OfferedSummaryVo [id=%s, name=%s, brand=%s, itemSku=%s, attributes=%s, imageUrl=%s, " +
            "otherAttributes=%s]", this.id, this.name, this.brand, this.itemSku, this.attributes,
        this.imageUrl, this.otherAttributes);
  }
}
