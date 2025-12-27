package com.gdn.x.product.rest.web.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Set;

/**
 * Created by govind on 14/09/2017 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductWrapper  extends ProductIdentifierWrapper {

  private Set<String> pristineIds;

  public Set<String> getPristineIds() {
    return pristineIds;
  }

  public void setPristineIds(Set<String> pristineIds) {
    this.pristineIds = pristineIds;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productCodes", super.getProductCodes())
        .append("productSkus", super.getProductSkus()).append("pristineIds", pristineIds).toString();
  }
}
