package com.gdn.mta.bulk.models.download;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.mta.distributiontask.request.DistributionTaskMultipleFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductVendorDownloadRequest extends BulkDownloadRequest {

  private static final long serialVersionUID = 269665370894299715L;
  private ProductListRequest productListRequest;
  private String vendorCode;
  private Integer productSize;

  public ProductVendorDownloadRequest() {}

  public ProductListRequest getProductListRequest() {
    return productListRequest;
  }

  public String getVendorCode() {
    return vendorCode;
  }

  public Integer getProductSize() {
    return productSize;
  }

  public static class ProductVendorDownloadBuilder extends BulkDownloadRequest.BulkRequestBuilder {
    private ProductListRequest productListRequest;
    private String vendorCode;
    private Integer productSize;

    public ProductVendorDownloadBuilder() {}

    public ProductVendorDownloadBuilder productListRequest(ProductListRequest productListRequest) {
      this.productListRequest = productListRequest;
      return this;
    }

    public ProductVendorDownloadBuilder vendorCode(String vendorCode) {
      this.vendorCode = vendorCode;
      return this;
    }

    public ProductVendorDownloadBuilder productSize(Integer productSize) {
      this.productSize = productSize;
      return this;
    }

    public ProductVendorDownloadRequest build() {
      return new ProductVendorDownloadRequest(this);
    }

  }

  public ProductVendorDownloadRequest(ProductVendorDownloadBuilder builder) {
    super(builder);
    productListRequest = builder.productListRequest;
    vendorCode = builder.vendorCode;
    productSize = builder.productSize;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("productListRequest", productListRequest)
        .append("vendorCode", vendorCode).append("productSize", productSize)
        .append("requestId", getRequestId()).append("downloadType", getDownloadType())
        .append("fileType", getFileType()).append("bulkProcessEntity", getBulkProcessEntity())
        .append("emailCc", getEmailCc()).append("emailTo", getEmailTo())
        .append("filename", getFilename()).append("username", getUsername())
        .append("merchantId", getMerchantId()).append("language", getLanguage())
        .append("exceptionMsg", getExceptionMsg()).append("directDownload", isDirectDownload())
        .toString();
  }
}
