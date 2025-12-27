package com.gdn.mta.bulk.entity;

import java.io.Serializable;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

/**
 * Created by virajjasani on 30/08/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkDownloadQueue extends GdnBaseDomainEventModel implements Serializable {
  private static final long serialVersionUID = -7784797834495965218L;

  private String businessPartnerCode;
  private String requestId;
  private Map<String, Boolean> privilegedMap;
  private Integer productSize;
  private ProductLevel3SummaryRequest request;
  private String emailCc;
  private String emailBcc;
  private String emailTo;

  public BulkDownloadQueue() {
  }

  public BulkDownloadQueue(String businessPartnerCode, String requestId,
      Map<String, Boolean> privilegedMap, Integer productSize, ProductLevel3SummaryRequest request) {
    this.businessPartnerCode = businessPartnerCode;
    this.requestId = requestId;
    this.privilegedMap = privilegedMap;
    this.productSize = productSize;
    this.request = request;
  }

  public BulkDownloadQueue(String businessPartnerCode, String requestId,
      Map<String, Boolean> privilegedMap, Integer productSize,ProductLevel3SummaryRequest request, String emailCc,
      String emailBcc, String emailTo) {
    this.businessPartnerCode = businessPartnerCode;
    this.requestId = requestId;
    this.privilegedMap = privilegedMap;
    this.productSize = productSize;
    this.emailCc = emailCc;
    this.emailBcc = emailBcc;
    this.emailTo = emailTo;
    this.request = request;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public String getRequestId() {
    return requestId;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  public Map<String, Boolean> getPrivilegedMap() {
    return privilegedMap;
  }

  public void setPrivilegedMap(Map<String, Boolean> privilegedMap) {
    this.privilegedMap = privilegedMap;
  }

  public Integer getProductSize() {
    return productSize;
  }

  public void setProductSize(Integer productSize) {
    this.productSize = productSize;
  }

  public String getEmailCc() {
    return emailCc;
  }

  public void setEmailCc(String emailCc) {
    this.emailCc = emailCc;
  }

  public String getEmailBcc() {
    return emailBcc;
  }

  public void setEmailBcc(String emailBcc) {
    this.emailBcc = emailBcc;
  }

  public String getEmailTo() {
    return emailTo;
  }

  public void setEmailTo(String emailTo) {
    this.emailTo = emailTo;
  }

  public ProductLevel3SummaryRequest getRequest() {
    return request;
  }

  public void setRequest(ProductLevel3SummaryRequest request) {
    this.request = request;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("BulkDownloadQueue{");
    sb.append("businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", requestId='").append(requestId).append('\'');
    sb.append(", privilegedMap=").append(privilegedMap);
    sb.append(", productSize=").append(productSize);
    sb.append(", request=").append(request);
    sb.append(", emailCc='").append(emailCc).append('\'');
    sb.append(", emailBcc='").append(emailBcc).append('\'');
    sb.append(", emailTo='").append(emailTo).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
