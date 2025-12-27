package com.gdn.mta.bulk.dto.product;

import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductLevel3Request extends BaseRequest {

  private static final long serialVersionUID = -8611382808482810163L;
  private String productSku;
  private String productCode;
  private String businessPartnerCode;
  private Boolean synchronize;
  private String productName;
  private Integer productType;
  private String categoryCode;
  private String categoryName;
  private String categoryHierarchy;
  private String brand;
  private String description;
  private String specificationDetail;
  private String uniqueSellingPoint;
  private String productStory;
  private List<ProductItemLevel3Request> items;
  private List<ProductLevel3AttributeRequest> attributes;
  private List<ProductLevel3ImageRequest> images;
  private String url;
  private Boolean installationRequired;

  public ProductLevel3Request() {}

  public ProductLevel3Request(String productSku, String productCode, String businessPartnerCode,
      Boolean synchronize, String productName, Integer productType, String categoryCode,
      String categoryName, String categoryHierarchy, String brand, String description,
      String specificationDetail, String uniqueSellingPoint, String productStory,
      List<ProductItemLevel3Request> items, List<ProductLevel3AttributeRequest> attributes,
      List<ProductLevel3ImageRequest> images) {
    super();
    this.productSku = productSku;
    this.productCode = productCode;
    this.businessPartnerCode = businessPartnerCode;
    this.synchronize = synchronize;
    this.productName = productName;
    this.productType = productType;
    this.categoryCode = categoryCode;
    this.categoryName = categoryName;
    this.categoryHierarchy = categoryHierarchy;
    this.brand = brand;
    this.description = description;
    this.specificationDetail = specificationDetail;
    this.uniqueSellingPoint = uniqueSellingPoint;
    this.productStory = productStory;
    this.items = items;
    this.attributes = attributes;
    this.images = images;
  }

  public ProductLevel3Request(String productSku, String productCode, String businessPartnerCode,
      Boolean synchronize, String productName, Integer productType, String categoryCode,
      String categoryName, String categoryHierarchy, String brand, String description,
      String specificationDetail, String uniqueSellingPoint, String productStory,
      List<ProductItemLevel3Request> items, List<ProductLevel3AttributeRequest> attributes,
      List<ProductLevel3ImageRequest> images, String url) {
    this(productSku, productCode, businessPartnerCode, synchronize, productName, productType,
        categoryCode, categoryName, categoryHierarchy, brand, description, specificationDetail,
        uniqueSellingPoint, productStory, items, attributes, images);
    this.url = url;
  }

  public ProductLevel3Request(String productSku, String productCode, String businessPartnerCode,
      Boolean synchronize, String productName, Integer productType, String categoryCode,
      String categoryName, String categoryHierarchy, String brand, String description,
      String specificationDetail, String uniqueSellingPoint, String productStory,
      List<ProductItemLevel3Request> items, List<ProductLevel3AttributeRequest> attributes,
      List<ProductLevel3ImageRequest> images, String url, Boolean installationRequired) {
    this(productSku, productCode, businessPartnerCode, synchronize, productName, productType,
        categoryCode, categoryName, categoryHierarchy, brand, description, specificationDetail,
        uniqueSellingPoint, productStory, items, attributes, images, url);
    this.installationRequired = installationRequired;
  }

  public String getProductSku() {
    return productSku;
  }

  public void setProductSku(String productSku) {
    this.productSku = productSku;
  }

  public String getProductCode() {
    return productCode;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public String getBusinessPartnerCode() {
    return businessPartnerCode;
  }

  public void setBusinessPartnerCode(String businessPartnerCode) {
    this.businessPartnerCode = businessPartnerCode;
  }

  public Boolean getSynchronize() {
    return synchronize;
  }

  public void setSynchronize(Boolean synchronize) {
    this.synchronize = synchronize;
  }

  public String getProductName() {
    return productName;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public Integer getProductType() {
    return productType;
  }

  public void setProductType(Integer productType) {
    this.productType = productType;
  }

  public String getCategoryCode() {
    return categoryCode;
  }

  public void setCategoryCode(String categoryCode) {
    this.categoryCode = categoryCode;
  }

  public String getCategoryName() {
    return categoryName;
  }

  public void setCategoryName(String categoryName) {
    this.categoryName = categoryName;
  }

  public String getCategoryHierarchy() {
    return categoryHierarchy;
  }

  public void setCategoryHierarchy(String categoryHierarchy) {
    this.categoryHierarchy = categoryHierarchy;
  }

  public String getBrand() {
    return brand;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public String getDescription() {
    return description;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public String getSpecificationDetail() {
    return specificationDetail;
  }

  public void setSpecificationDetail(String specificationDetail) {
    this.specificationDetail = specificationDetail;
  }

  public String getUniqueSellingPoint() {
    return uniqueSellingPoint;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
  }

  public String getProductStory() {
    return productStory;
  }

  public void setProductStory(String productStory) {
    this.productStory = productStory;
  }

  public List<ProductItemLevel3Request> getItems() {
    return items;
  }

  public void setItems(List<ProductItemLevel3Request> items) {
    this.items = items;
  }

  public List<ProductLevel3AttributeRequest> getAttributes() {
    return attributes;
  }

  public void setAttributes(List<ProductLevel3AttributeRequest> attributes) {
    this.attributes = attributes;
  }

  public List<ProductLevel3ImageRequest> getImages() {
    return images;
  }

  public void setImages(List<ProductLevel3ImageRequest> images) {
    this.images = images;
  }

  public static long getSerialversionuid() {
    return serialVersionUID;
  }

  public String getUrl() {
    return url;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  public Boolean getInstallationRequired() {
    return installationRequired;
  }

  public void setInstallationRequired(Boolean installationRequired) {
    this.installationRequired = installationRequired;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductLevel3Request [productSku=%s, productCode=%s, businessPartnerCode=%s, synchronize=%s, productName=%s, productType=%s, categoryCode=%s, categoryName=%s, categoryHierarchy=%s, brand=%s, description=%s, specificationDetail=%s, uniqueSellingPoint=%s, productStory=%s, items=%s, attributes=%s, images=%s, url=%s, installationRequired=%s, getProductSku()=%s, getProductCode()=%s, getBusinessPartnerCode()=%s, getSynchronize()=%s, getProductName()=%s, getProductType()=%s, getCategoryCode()=%s, getCategoryName()=%s, getCategoryHierarchy()=%s, getBrand()=%s, getDescription()=%s, getSpecificationDetail()=%s, getUniqueSellingPoint()=%s, getProductStory()=%s, getItems()=%s, getAttributes()=%s, getImages()=%s, getUrl()=%s, getInstallationRequired()=%s]",
            productSku, productCode, businessPartnerCode, synchronize, productName, productType,
            categoryCode, categoryName, categoryHierarchy, brand, description, specificationDetail,
            uniqueSellingPoint, productStory, items, attributes, images, url, installationRequired,
            getProductSku(), getProductCode(), getBusinessPartnerCode(), getSynchronize(),
            getProductName(), getProductType(), getCategoryCode(), getCategoryName(),
            getCategoryHierarchy(), getBrand(), getDescription(), getSpecificationDetail(),
            getUniqueSellingPoint(), getProductStory(), getItems(), getAttributes(), getImages(),
            getUrl(), getInstallationRequired());
  }
  
}
