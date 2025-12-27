package com.gdn.x.productcategorybase.entity.solr;

import java.io.Serializable;

import lombok.AllArgsConstructor;
import lombok.Builder;

@Builder
@AllArgsConstructor
public class SolrProductResponse implements Serializable {

  private static final long serialVersionUID = -4929347130497673346L;

  private String productCode;
  private String productName;

  public SolrProductResponse() {
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  @Override
  public String toString() {
    return "SolrProductResponse [productCode=" + productCode + ", productName=" + productName + "]";
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    if (!super.equals(o)) return false;

    SolrProductResponse that = (SolrProductResponse) o;

    if (productCode != null ? !productCode.equals(that.productCode) : that.productCode != null)
      return false;
    return productName != null ? productName.equals(that.productName) : that.productName == null;

  }

  @Override
  public int hashCode() {
    int result = super.hashCode();
    result = 31 * result + (productCode != null ? productCode.hashCode() : 0);
    result = 31 * result + (productName != null ? productName.hashCode() : 0);
    return result;
  }
}

