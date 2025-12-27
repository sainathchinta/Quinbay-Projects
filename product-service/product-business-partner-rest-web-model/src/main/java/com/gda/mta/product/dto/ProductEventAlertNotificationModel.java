package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.data.domain.Page;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;

/**
 * Created by Vishal on 19/06/18.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductEventAlertNotificationModel implements Serializable{


  private static final long serialVersionUID = -460237458816037094L;
  private String notificationType;

  private String notificationMode;

  private String productName;

  private String notes;

  private String productCreatedBy;

  private ProductDetailResponse product;

  private List<ProfileResponse> profileList;

  private Map<String, List<ProductCollectionElement>> productCollectionElementListMap;

  private Page<ProductBusinessPartnerResponse> productBusinessPartnerPage;

  private Map<String, List<List<String>>> productData ;

  public String getNotificationType() {
    return notificationType;
  }

  public void setNotificationType(String notificationType) {
    this.notificationType = notificationType;
  }

  public String getNotificationMode() {
    return notificationMode;
  }

  public void setNotificationMode(String notificationMode) {
    this.notificationMode = notificationMode;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public String getNotes() {
    return notes;
  }

  public void setNotes(String notes) {
    this.notes = notes;
  }

  public String getProductCreatedBy() {
    return productCreatedBy;
  }

  public void setProductCreatedBy(String productCreatedBy) {
    this.productCreatedBy = productCreatedBy;
  }

  public ProductDetailResponse getProduct() {
    return product;
  }

  public void setProduct(ProductDetailResponse product) {
    this.product = product;
  }

  public List<ProfileResponse> getProfileList() {
    return profileList;
  }

  public void setProfileList(List<ProfileResponse> profileList) {
    this.profileList = profileList;
  }

  public Map<String, List<ProductCollectionElement>> getProductCollectionElementListMap() {
    return productCollectionElementListMap;
  }

  public void setProductCollectionElementListMap(
      Map<String, List<ProductCollectionElement>> productCollectionElementListMap) {
    this.productCollectionElementListMap = productCollectionElementListMap;
  }

  public Page<ProductBusinessPartnerResponse> getProductBusinessPartnerPage() {
    return productBusinessPartnerPage;
  }

  public void setProductBusinessPartnerPage(
      Page<ProductBusinessPartnerResponse> productBusinessPartnerPage) {
    this.productBusinessPartnerPage = productBusinessPartnerPage;
  }

  public Map<String, List<List<String>>> getProductData() {
    return productData;
  }

  public void setProductData(Map<String, List<List<String>>> productData) {
    this.productData = productData;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("notificationType", notificationType)
        .append("notificationMode", notificationMode).append("productName", productName)
        .append("notes", notes).append("productCreatedBy", productCreatedBy)
        .append("product", product).append("profileList", profileList)
        .append("productCollectionElementListMap", productCollectionElementListMap)
        .append("productBusinessPartnerPage", productBusinessPartnerPage)
        .append("productData", productData).toString();
  }
}
