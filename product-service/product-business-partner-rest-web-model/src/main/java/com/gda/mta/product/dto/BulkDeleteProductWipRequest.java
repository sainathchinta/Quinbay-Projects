package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.Set;
import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Created by Vishal on 14/05/18.
 */
public class BulkDeleteProductWipRequest implements Serializable {

  private static final long serialVersionUID = 1485798926276128671L;
  Set<String> productLevel1Ids;

  public BulkDeleteProductWipRequest(Set<String> productLevel1Ids) {
    this.productLevel1Ids = productLevel1Ids;
  }

  public BulkDeleteProductWipRequest() {
  }

  public Set<String> getProductLevel1Ids() {
    return this.productLevel1Ids;
  }

  public void setProductLevel1Ids(Set<String> productLevel1Ids) {
    this.productLevel1Ids = productLevel1Ids;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productSkus", this.productLevel1Ids).toString();
  }
}

