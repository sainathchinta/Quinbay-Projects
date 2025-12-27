package com.gdn.mta.bulk.dto;

import java.io.Serializable;
import java.util.Map;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;

/**
 * Created by virajjasani on 30/08/16.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkDownloadProductRequest implements Serializable {

  private static final long serialVersionUID = 2855922926625854734L;

  private String businessPartnerCode;
  private Map<String, Boolean> privilegedMap;
  private Integer productSize;
  private ProductLevel3SummaryRequest request;
  private String emailCc;
  private String emailTo;

  public BulkDownloadProductRequest(String businessPartnerCode,
      Map<String, Boolean> privilegedMap, Integer productSize, ProductLevel3SummaryRequest request, String emailCc,
      String emailTo) {
    this.businessPartnerCode = businessPartnerCode;
    this.privilegedMap = privilegedMap;
    this.productSize = productSize;
    this.request = request;
    this.emailCc = emailCc;
    this.emailTo = emailTo;
  }

  public BulkDownloadProductRequest() {
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
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
    final StringBuilder sb = new StringBuilder("BulkDownloadProductRequest{");
    sb.append("businessPartnerCode='").append(businessPartnerCode).append('\'');
    sb.append(", privilegedMap=").append(privilegedMap);
    sb.append(", productSize=").append(productSize);
    sb.append(", request=").append(request);
    sb.append(", emailCc='").append(emailCc).append('\'');
    sb.append(", emailTo='").append(emailTo).append('\'');
    sb.append('}');
    return sb.toString();
  }
}
