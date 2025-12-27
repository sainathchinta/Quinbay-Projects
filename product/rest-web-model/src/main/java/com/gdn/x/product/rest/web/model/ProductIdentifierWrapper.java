package com.gdn.x.product.rest.web.model;

import java.io.Serializable;
import java.util.Set;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

/**
 * Created by govind on 12/10/2017 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductIdentifierWrapper implements Serializable{

  private static final long serialVersionUID = -5855359945185919117L;
  private Set<String> productCodes;
  private Set<String> productSkus;

  public Set<String> getProductCodes() {
    return productCodes;
  }

  public void setProductCodes(Set<String> productCodes) {
    this.productCodes = productCodes;
  }

  public Set<String> getProductSkus() {
    return productSkus;
  }

  public void setProductSkus(Set<String> productSkus) {
    this.productSkus = productSkus;
  }


  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCodes", productCodes)
        .append("productSkus", productSkus).toString();
  }
}
