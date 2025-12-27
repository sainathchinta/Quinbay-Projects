package com.gdn.partners.pbp.dto.productlevel1;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel1WipDeleteRequest implements Serializable {

  private static final long serialVersionUID = 4861390767696558105L;

  private List<String> productCodes;
  private String notes;

  public ProductLevel1WipDeleteRequest() {}

  public List<String> getProductCodes() {
    return productCodes;
  }

  public void setProductCodes(List<String> productCodes) {
    this.productCodes = productCodes;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  @Override
  public String toString() {
    StringBuilder builder = new StringBuilder();
    builder.append("ProductLevel1WipDeleteRequest [productCodes=");
    builder.append(productCodes);
    builder.append(", notes=");
    builder.append(notes);
    builder.append("]");
    return builder.toString();
  }

}
