package com.gdn.partners.pbp.dto.productlevel3;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3GdnSkuListRequest implements Serializable {
  
  private static final long serialVersionUID = 7096062844407714649L;
  
  private List<String> gdnSkus;

  public List<String> getGdnSkus() {
    return gdnSkus;
  }

  public void setGdnSkus(List<String> gdnSkus) {
    this.gdnSkus = gdnSkus;
  }

  @Override
  public String toString() {
    return "ProductLevel3GdnSkuListRequest [gdnSkus=" + gdnSkus + "]";
  }
  
}
