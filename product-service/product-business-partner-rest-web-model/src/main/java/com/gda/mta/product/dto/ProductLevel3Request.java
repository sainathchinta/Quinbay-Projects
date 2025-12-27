package com.gda.mta.product.dto;

import java.util.List;

import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.partners.pbp.commons.constants.Constants;

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
  private String accessChannel;
  private Long version;
  private List<ProductLevel3LogisticsRequest> productLevel3LogisticsRequest;
  private String categoryId;
  private boolean productEditable;
  private boolean off2OnChannelActive;
  private boolean needCorrection;
  private boolean freeSample;

  public ProductLevel3Request() {}

  public ProductLevel3Request(String productSku, String productCode, String businessPartnerCode,
      Boolean synchronize, String productName, Integer productType, String categoryCode,
      String categoryName, String categoryHierarchy, String brand, String description,
      String specificationDetail, String uniqueSellingPoint, String productStory,
      List<ProductItemLevel3Request> items, List<ProductLevel3AttributeRequest> attributes,
      List<ProductLevel3ImageRequest> images,
      List<ProductLevel3LogisticsRequest> productLevel3LogisticsRequest) {
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
    this.productLevel3LogisticsRequest = productLevel3LogisticsRequest;
  }

  public ProductLevel3Request(String productSku, String productCode, String businessPartnerCode,
      Boolean synchronize, String productName, Integer productType, String categoryCode,
      String categoryName, String categoryHierarchy, String brand, String description,
      String specificationDetail, String uniqueSellingPoint, String productStory,
      List<ProductItemLevel3Request> items, List<ProductLevel3AttributeRequest> attributes,
      List<ProductLevel3ImageRequest> images, String url,
      List<ProductLevel3LogisticsRequest> productLevel3LogisticsRequest) {
    this(productSku, productCode, businessPartnerCode, synchronize, productName, productType,
        categoryCode, categoryName, categoryHierarchy, brand, description, specificationDetail,
        uniqueSellingPoint, productStory, items, attributes, images, productLevel3LogisticsRequest);
    this.url = url;
  }

  public ProductLevel3Request(String productSku, String productCode, String businessPartnerCode,
      Boolean synchronize, String productName, Integer productType, String categoryCode,
      String categoryName, String categoryHierarchy, String brand, String description,
      String specificationDetail, String uniqueSellingPoint, String productStory,
      List<ProductItemLevel3Request> items, List<ProductLevel3AttributeRequest> attributes,
      List<ProductLevel3ImageRequest> images, String url, Boolean installationRequired,
      List<ProductLevel3LogisticsRequest> productLevel3LogisticsRequest) {
    this(productSku, productCode, businessPartnerCode, synchronize, productName, productType,
        categoryCode, categoryName, categoryHierarchy, brand, description, specificationDetail,
        uniqueSellingPoint, productStory, items, attributes, images, url, productLevel3LogisticsRequest);
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

  public String getAccessChannel() {
    return accessChannel;
  }

  public void setAccessChannel(String accessChannel) {
    this.accessChannel = accessChannel;
  }

  public void setInstallationRequired(Boolean installationRequired) {
    this.installationRequired = installationRequired;
  }

  public Long getVersion() {
    return version;
  }

  public void setVersion(Long version) {
    this.version = version;
  }

  public List<ProductLevel3LogisticsRequest> getProductLevel3LogisticsRequest() {
    return productLevel3LogisticsRequest;
  }

  public void setProductLevel3LogisticsRequest(
      List<ProductLevel3LogisticsRequest> productLevel3LogisticsRequest) {
    this.productLevel3LogisticsRequest = productLevel3LogisticsRequest;
  }

  public String getCategoryId() {
    return categoryId;
  }

  public void setCategoryId(String categoryId) {
    this.categoryId = categoryId;
  }

  public boolean isProductEditable() {
    return productEditable;
  }

  public void setProductEditable(boolean productEditable) {
    this.productEditable = productEditable;
  }

  public boolean isOff2OnChannelActive() {
    return off2OnChannelActive;
  }

  public void setOff2OnChannelActive(boolean off2OnChannelActive) {
    this.off2OnChannelActive = off2OnChannelActive;
  }

  public boolean isNeedCorrection() {
    return needCorrection;
  }

  public void setNeedCorrection(boolean needCorrection) {
    this.needCorrection = needCorrection;
  }

  public boolean isFreeSample() {
    return freeSample;
  }

  public void setFreeSample(boolean freeSample) {
    this.freeSample = freeSample;
  }

  @Override
  public String toString() {
    return String
        .format(
            "ProductLevel3Request [productSku=%s, productCode=%s, businessPartnerCode=%s, synchronize=%s, productName=%s, productType=%s, categoryCode=%s, categoryName=%s, categoryHierarchy=%s, brand=%s, description=%s, specificationDetail=%s, uniqueSellingPoint=%s, productStory=%s, items=%s, attributes=%s, images=%s, url=%s, installationRequired=%s, productLevel3LogisticsRequest=%s, getProductSku()=%s, getProductCode()=%s, getBusinessPartnerCode()=%s, getSynchronize()=%s, getProductName()=%s, getProductType()=%s, getCategoryCode()=%s, getCategoryName()=%s, getCategoryHierarchy()=%s, getBrand()=%s, getDescription()=%s, getSpecificationDetail()=%s, getUniqueSellingPoint()=%s, getProductStory()=%s, getItems()=%s, getAttributes()=%s, getImages()=%s, getUrl()=%s, getInstallationRequired()=%s, getVersion()=%s, getProductLevel3LogisticsRequest()=%s, getCategoryId()=%s, isProductEditable()=%s, isOff2OnChannelActive()=%s, isNeedCorrection()=%s]",
            productSku, productCode, businessPartnerCode, synchronize, productName, productType,
            categoryCode, categoryName, categoryHierarchy, brand, description, specificationDetail,
            uniqueSellingPoint, productStory, items, attributes, images, url, installationRequired, 
            productLevel3LogisticsRequest,
            getProductSku(), getProductCode(), getBusinessPartnerCode(), getSynchronize(),
            getProductName(), getProductType(), getCategoryCode(), getCategoryName(),
            getCategoryHierarchy(), getBrand(), getDescription(), getSpecificationDetail(),
            getUniqueSellingPoint(), getProductStory(), getItems(), getAttributes(), getImages(),
            getUrl(), getInstallationRequired(), getVersion(), getProductLevel3LogisticsRequest(), getCategoryId(),
            isProductEditable(), isOff2OnChannelActive(), isNeedCorrection());
  }

  public boolean validateRequest() {
    if (StringUtils.isNotEmpty(this.description) && this.description.toString().toLowerCase()
        .contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.uniqueSellingPoint) && this.uniqueSellingPoint.toLowerCase()
        .contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.productStory) && this.productStory.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.specificationDetail) && this.specificationDetail.toLowerCase()
        .contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.url) && this.url.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.productName) && this.productName.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.productSku) && this.productSku.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.productCode) && this.productCode.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.accessChannel) && this.accessChannel.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    if (StringUtils.isNotEmpty(this.brand) && this.brand.toLowerCase().contains(Constants.SCRIPT)) {
      return false;
    }
    return true;
  }
}
