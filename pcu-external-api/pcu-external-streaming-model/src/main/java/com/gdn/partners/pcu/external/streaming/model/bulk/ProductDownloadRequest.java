package com.gdn.partners.pcu.external.streaming.model.bulk;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import org.apache.commons.lang3.builder.ToStringBuilder;

import java.util.Map;
import java.util.Set;


public class ProductDownloadRequest extends BulkDownloadRequest {

  private static final long serialVersionUID = -1126351309933076209L;
  private Map<String, Boolean> privilegedMap;
  private Integer productSize;
  private ProductLevel3SummaryRequest request;
  private ProductSummaryRequest productSummaryRequest;
  private Set<String> accessiblePickupPoints;

  public Map<String, Boolean> getPrivilegedMap() {
    return privilegedMap;
  }

  public Integer getProductSize() {
    return productSize;
  }

  public ProductLevel3SummaryRequest getRequest() {
    return request;
  }

  public ProductSummaryRequest getProductSummaryRequest() {
    return productSummaryRequest;
  }

  public Set<String> getAccessiblePickupPoints() {
    return accessiblePickupPoints;
  }

  public void setAccessiblePickupPoints(Set<String> accessiblePickupPoints) {
    this.accessiblePickupPoints = accessiblePickupPoints;
  }

  public ProductDownloadRequest() {
  }


  public static class ProductBuilder extends BulkDownloadRequest.BulkRequestBuilder {
    private Map<String, Boolean> privilegedMap;
    private Integer productSize;
    private ProductLevel3SummaryRequest request;
    private ProductSummaryRequest productSummaryRequest;
    private Set<String> accessiblePickupPoints;


    public ProductBuilder privilegedMap(Map<String, Boolean> privilegedMap) {
      this.privilegedMap = privilegedMap;
      return this;
    }

    public ProductBuilder productSize(Integer productSize) {
      this.productSize = productSize;
      return this;
    }

    public ProductBuilder productRequest(ProductLevel3SummaryRequest request){
      this.request = request;
      return this;
    }

    public ProductBuilder productSummaryRequest(ProductSummaryRequest productSummaryRequest) {
      this.productSummaryRequest = productSummaryRequest;
      return this;
    }

    public ProductBuilder accessiblePickupPoints(Set<String> accessiblePickupPoints) {
      this.accessiblePickupPoints = accessiblePickupPoints;
      return this;
    }

    public ProductDownloadRequest build() {
      return new ProductDownloadRequest(this);
    }
  }

  protected ProductDownloadRequest(ProductBuilder productBuilder) {
    super(productBuilder);
    privilegedMap = productBuilder.privilegedMap;
    productSize = productBuilder.productSize;
    request = productBuilder.request;
    productSummaryRequest = productBuilder.productSummaryRequest;
    accessiblePickupPoints = productBuilder.accessiblePickupPoints;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("privilegedMap", privilegedMap)
        .append("productSize", productSize)
        .append("requestId", getRequestId()).append("downloadType", getDownloadType())
        .append("fileType", getFileType()).append("bulkProcessEntity", getBulkProcessEntity())
        .append("emailCc", getEmailCc()).append("emailTo", getEmailTo()).append("request", request)
        .append("productSummaryRequest", productSummaryRequest)
        .append("filename", getFilename()).append("username", getUsername())
        .append("merchantId", getMerchantId()).toString();
  }
}
