package com.gdn.partners.pcu.master.client.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ShippingResponse;

@JsonIgnoreProperties(ignoreUnknown = true)
public class CategoryDetailAndShippingResponse extends CategoryDetailResponse {

  private static final long serialVersionUID = 9172763720314867151L;
  private List<ShippingResponse> shippingResponses;
  private String nameEnglish;
  private byte[] descriptionEnglish;

  public  CategoryDetailAndShippingResponse() {
  }

  public CategoryDetailAndShippingResponse(List<ShippingResponse> shippingResponses, String nameEnglish,
      byte[] descriptionEnglish) {
    this.shippingResponses = shippingResponses;
    this.nameEnglish = nameEnglish;
    this.descriptionEnglish = descriptionEnglish;
  }

  public CategoryDetailAndShippingResponse(CategoryResponse response, List<ShippingResponse> shippingResponses,
      String nameEnglish, byte[] descriptionEnglish) {
    super(response);
    this.shippingResponses = shippingResponses;
    this.nameEnglish = nameEnglish;
    this.descriptionEnglish = descriptionEnglish;
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

  public List<ShippingResponse> getShippingResponses() {
    return shippingResponses;
  }

  public void setShippingResponses(List<ShippingResponse> shippingResponses) {
    this.shippingResponses = shippingResponses;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("CategoryDetailAndShippingResponse{");
    sb.append("shippingResponses=").append(shippingResponses);
    sb.append(", nameEnglish='").append(nameEnglish).append('\'');
    sb.append(", descriptionEnglish='").append(descriptionEnglish).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
