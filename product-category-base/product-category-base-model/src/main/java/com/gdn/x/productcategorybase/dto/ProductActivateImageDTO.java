package com.gdn.x.productcategorybase.dto;

import java.io.Serializable;
import java.util.Set;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by Vishal on 12/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductActivateImageDTO implements Serializable{
  private static final long serialVersionUID = -4830453346840850724L;

  private String productCode;
  private Set<ActivateImageDTO> imageRequests;

  public ProductActivateImageDTO() {
  }

  public ProductActivateImageDTO(String productCode, Set<ActivateImageDTO> imageRequests) {
    this.productCode = productCode;
    this.imageRequests = imageRequests;
  }

  public Set<ActivateImageDTO> getImageRequests() {
    return imageRequests;
  }

  public void setImageRequests(Set<ActivateImageDTO> imageRequests) {
    this.imageRequests = imageRequests;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCode", productCode)
        .append("imageRequests", imageRequests).toString();
  }
}
