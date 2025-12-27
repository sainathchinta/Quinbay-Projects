package com.gdn.x.productcategorybase.dto;

import java.util.Set;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

/**
 * Created by Vishal on 11/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductActivateImageRequest extends BaseRequest{
  private static final long serialVersionUID = 6257043547796927423L;
  private String productCode;
  private Set<ActivateImageRequest> imageRequests;

  public ProductActivateImageRequest() {
  }

  public ProductActivateImageRequest(String productCode, Set<ActivateImageRequest> imageRequests) {
    this.productCode = productCode;
    this.imageRequests = imageRequests;
  }

  public Set<ActivateImageRequest> getImageRequests() {
    return imageRequests;
  }

  public void setImageRequests(Set<ActivateImageRequest> imageRequests) {
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
