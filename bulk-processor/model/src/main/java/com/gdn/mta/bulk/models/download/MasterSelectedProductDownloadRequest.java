package com.gdn.mta.bulk.models.download;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterSelectedProductDownloadRequest extends BulkDownloadRequest implements Serializable {
  
  private static final long serialVersionUID = -8186342200080283891L;
  private List<String> productCodes;
  private Integer productSize;

  public MasterSelectedProductDownloadRequest() {
  }

  public Integer getProductSize() {
    return productSize;
  }

  public void setProductSize(Integer productsSize) {
    this.productSize = productsSize;
  }

  public List<String> getProductCodes() {
    return productCodes;
  }

  public void setProductCodes(List<String> productcodes) {
    this.productCodes = productcodes;
  }

  public static class MasterSelectedProductBuilder extends BulkDownloadRequest.BulkRequestBuilder {
    private List<String> productCodes;
    private Integer productSize;

    public MasterSelectedProductBuilder productSize(Integer productSize) {
      this.productSize = productSize;
      return this;
    }

    public MasterSelectedProductBuilder productCodes(List<String> productCodes){
      this.productCodes = productCodes;
      return this;
    }

    public MasterSelectedProductDownloadRequest build() {
      return new MasterSelectedProductDownloadRequest(this);
    }
  }

  protected MasterSelectedProductDownloadRequest(MasterSelectedProductBuilder masterSelectedProductBuilder) {
    super(masterSelectedProductBuilder);
    productCodes = masterSelectedProductBuilder.productCodes;
    productSize = masterSelectedProductBuilder.productSize;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("MasterSelectedProductDownloadRequest{");
    sb.append("productCodes=").append(productCodes);
    sb.append(", productSize=").append(productSize);
    sb.append('}');
    return sb.toString();
  }
}
