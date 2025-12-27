package com.gdn.partners.pcu.external.streaming.model.bulk;

import java.io.Serial;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;

import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
public class ProductDownloadEANRequest extends BulkDownloadRequest {

  @Serial
  private static final long serialVersionUID = 250L;
  private Integer productSize;
  private ProductLevel3SummaryRequest request;
  private ProductSummaryRequest productSummaryRequest;

  public ProductDownloadEANRequest() {
  }

  public static class ProductBuilder extends BulkDownloadRequest.BulkRequestBuilder {
    private Integer productSize;
    private ProductLevel3SummaryRequest request;
    private ProductSummaryRequest productSummaryRequest;

    public ProductBuilder productSize(Integer productSize) {
      this.productSize = productSize;
      return this;
    }

    public ProductBuilder productSummaryRequest(ProductSummaryRequest productSummaryRequest) {
      this.productSummaryRequest = productSummaryRequest;
      return this;
    }

    public ProductDownloadEANRequest build() {
      return new ProductDownloadEANRequest(this);
    }
  }

  protected ProductDownloadEANRequest(ProductBuilder productBuilder) {
    super(productBuilder);
    productSize = productBuilder.productSize;
    request = productBuilder.request;
    productSummaryRequest = productBuilder.productSummaryRequest;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this)
        .append("productSize", productSize)
        .append("requestId", getRequestId()).append("downloadType", getDownloadType())
        .append("fileType", getFileType()).append("bulkProcessEntity", getBulkProcessEntity())
        .append("emailCc", getEmailCc()).append("emailTo", getEmailTo()).append("request", request)
        .append("productSummaryRequest", productSummaryRequest)
        .append("filename", getFilename()).append("username", getUsername())
        .append("merchantId", getMerchantId()).toString();
  }
}