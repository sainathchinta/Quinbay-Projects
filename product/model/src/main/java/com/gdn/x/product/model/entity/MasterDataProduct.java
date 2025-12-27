package com.gdn.x.product.model.entity;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.x.product.model.vo.AiGeneratedFieldsResponse;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.Field;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.vo.SortedDefiningAttribute;
import lombok.Getter;
import lombok.Setter;

public class MasterDataProduct implements GdnBaseEmbedded {

  private static final long serialVersionUID = 1L;
  private static final Logger LOG = LoggerFactory.getLogger(MasterDataProduct.class);

  @Field(value = ProductFieldNames.BRAND)
  private String brand;

  @Field(value = ProductFieldNames.SHIPPING_WEIGHT)
  private double shippingWeight;

  @Field(value = ProductFieldNames.SPECIFICATION_DETAIL)
  private String specificationDetail;

  @Field(value = ProductFieldNames.PRODUCT_NAME)
  private String productName;

  @Field(value = ProductFieldNames.DESCRIPTION)
  private String description;

  @Field(value = ProductFieldNames.LONG_DESCRIPTION)
  private String longDescription;

  @Field(value = ProductFieldNames.UNIQUE_SELLING_POINT)
  private String uniqueSellingPoint;

  @Field(value = ProductFieldNames.IS_ACTIVATED)
  private boolean isActivated;

  @Field(value = ProductFieldNames.IS_VIEWABLE)
  private boolean isViewable;

  @Field(value = ProductFieldNames.PRODUCT_STORY)
  private String productStory;

  @Field(value = ProductFieldNames.UOM)
  private String uom;

  @Field(value = ProductFieldNames.MASTER_DATA_PRODUCT_IMAGES)
  private List<MasterDataProductImage> masterDataProductImages;

  @Field(value = ProductFieldNames.MASTER_DATA_PRODUCT_ATTRIBUTES)
  private List<MasterDataProductAttribute> masterDataProductAttributes =
      new ArrayList<MasterDataProductAttribute>();

  @Field(value = ProductFieldNames.URL)
  private String url;

  @Field(value = ProductFieldNames.LENGTH)
  private double length;

  @Field(value = ProductFieldNames.WIDTH)
  private double width;

  @Field(value = ProductFieldNames.HEIGHT)
  private double height;

  @Field(value = ProductFieldNames.WEIGHT)
  private double weight;

  @Field(value = ProductFieldNames.MASTER_CATALOG)
  private MasterCatalog masterCatalog;
  
  @Field(value = ProductFieldNames.BRAND_LOGO_URL)
  private String brandLogoUrl;

  @Transient
  private List<SortedDefiningAttribute> sortedDefiningAttributes;

  @Transient
  private boolean sortedDefining;

  @Transient
  private boolean reviewPending;

  @Transient
  private String sizeAttributeCode;

  @Transient
  @Getter
  @Setter
  private String distributionInfo;

  @Transient
  @Getter
  @Setter
  private AiGeneratedFieldsResponse aiGeneratedFieldsResponse;

  public MasterDataProduct() {

  }

  public MasterDataProduct(String brand, double shippingWeight, String specificationDetail,
      String productName, String description, String longDescription, String uniqueSellingPoint,
      boolean isActivated, boolean isViewable, String productStory, String uom,
      List<MasterDataProductImage> masterDataProductImages,
      List<MasterDataProductAttribute> masterDataProductAttributes, String url, double length,
      double width, double height, double weight, MasterCatalog masterCatalog,
      String brandLogoUrl) {
    super();
    this.brand = brand;
    this.shippingWeight = shippingWeight;
    this.specificationDetail = specificationDetail;
    this.productName = productName;
    this.description = description;
    this.longDescription = longDescription;
    this.uniqueSellingPoint = uniqueSellingPoint;
    this.isActivated = isActivated;
    this.isViewable = isViewable;
    this.productStory = productStory;
    this.uom = uom;
    this.masterDataProductImages = masterDataProductImages;
    this.masterDataProductAttributes = masterDataProductAttributes;
    this.url = url;
    this.length = length;
    this.width = width;
    this.height = height;
    this.weight = weight;
    this.masterCatalog = masterCatalog;
    this.brandLogoUrl = brandLogoUrl;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(this, obj);
  }

  public String getBrand() {
    return this.brand;
  }

  public String getDescription() {
    return this.description;
  }

  public double getHeight() {
    return this.height;
  }

  public double getLength() {
    return this.length;
  }

  public String getLongDescription() {
    return this.longDescription;
  }

  public MasterCatalog getMasterCatalog() {
    return this.masterCatalog;
  }
  
  public String getBrandLogoUrl() {
    return this.brandLogoUrl;
  }

  public List<MasterDataProductAttribute> getMasterDataProductAttributes() {
    return this.masterDataProductAttributes;
  }

  public List<MasterDataProductImage> getMasterDataProductImages() {
    return this.masterDataProductImages;
  }

  public String getProductName() {
    return this.productName;
  }

  public String getProductStory() {
    return this.productStory;
  }

  public double getShippingWeight() {
    return this.shippingWeight;
  }

  public List<SortedDefiningAttribute> getSortedDefiningAttributes() {
    if (!sortedDefining) {
      try {
        sortedDefiningAttributes = masterDataProductAttributes.stream()
            .filter(this::isDefiningAttribute)
            .map(this::getSortedDefiningAttributeFromMasterDataProductAttribute)
            .collect(Collectors.toList());
        sortedDefining = true;
      } catch (Exception e) {
        LOG.error("failed to sort definingAttributes with error = ", e);
      }
    }
    return sortedDefiningAttributes;
  }

  private SortedDefiningAttribute getSortedDefiningAttributeFromMasterDataProductAttribute(
      MasterDataProductAttribute masterDataProductAttribute) {
    List<String> attributeValues =
        masterDataProductAttribute.getMasterDataProductAttributeValues()
            .stream()
            .map(this::getDecriptiveValueOrAllowedAttributeValue)
            .filter(Objects::nonNull)
            .map(this::setMaxValueToSequenceIfNotPresent)
            .sorted(Comparator.comparing(MasterDataAllowedAttributeValue::getSequence))
            .map(MasterDataAllowedAttributeValue::getValue)
            .collect(Collectors.toList());
    SortedDefiningAttribute sortedDefiningAttribute = new SortedDefiningAttribute();
    sortedDefiningAttribute.setAttributeName(masterDataProductAttribute.getMasterDataAttribute().getAttributeName());
    sortedDefiningAttribute.setDefiningAttributes(attributeValues);
    return sortedDefiningAttribute;
  }

  private MasterDataAllowedAttributeValue setMaxValueToSequenceIfNotPresent(
      MasterDataAllowedAttributeValue attributeValue) {
    if(Objects.isNull(attributeValue.getSequence())){
      attributeValue.setSequence(Integer.MAX_VALUE);
    }
    return attributeValue;
  }

  private boolean isDefiningAttribute(MasterDataProductAttribute masterDataProductAttribute) {
    LOG.info("Values for defining and variant creation {} {}",
      Optional.ofNullable(masterDataProductAttribute.getMasterDataAttribute())
        .map(MasterDataAttribute::getAttributeType).orElse(null),
      Optional.ofNullable(masterDataProductAttribute.getMasterDataAttribute())
        .map(MasterDataAttribute::isVariantCreation).orElse(false));
    return Objects.nonNull(masterDataProductAttribute.getMasterDataAttribute()) &&
        MasterDataAttributeType.DEFINING_ATTRIBUTE.equals(
            masterDataProductAttribute.getMasterDataAttribute().getAttributeType()) ||
        masterDataProductAttribute.getMasterDataAttribute().isVariantCreation();
  }

  private MasterDataAllowedAttributeValue getDecriptiveValueOrAllowedAttributeValue(
      MasterDataProductAttributeValue masterDataProductAttributeValue) {
    if (Objects.isNull(masterDataProductAttributeValue.getAllowedAttributeValue()) ) {
      MasterDataAllowedAttributeValue masterDataAllowedAttributeValue = new MasterDataAllowedAttributeValue();
      masterDataAllowedAttributeValue.setValue(masterDataProductAttributeValue.getDescriptiveAttributeValue());
      return masterDataAllowedAttributeValue;
    }
    return masterDataProductAttributeValue.getAllowedAttributeValue();
  }

  public String getSpecificationDetail() {
    return this.specificationDetail;
  }

  public String getUniqueSellingPoint() {
    return this.uniqueSellingPoint;
  }

  public String getUom() {
    return this.uom;
  }

  public String getUrl() {
    return this.url;
  }

  public double getWeight() {
    return this.weight;
  }

  public double getWidth() {
    return this.width;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public boolean isActivated() {
    return this.isActivated;
  }

  public boolean isSortedDefining() {
    return sortedDefining;
  }

  public boolean isViewable() {
    return this.isViewable;
  }

  public void setActivated(boolean isActivated) {
    this.isActivated = isActivated;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setHeight(double height) {
    this.height = height;
  }

  public void setHeightIfNotZero(double height) {
    if (height != 0.0)
      this.height = height;
  }

  public void setLength(double length) {
    this.length = length;
  }

  public void setLengthIfNotZero(double length) {
    if (length != 0.0)
      this.length = length;
  }

  public void setLongDescription(String longDescription) {
    this.longDescription = longDescription;
  }

  public void setMasterCatalog(MasterCatalog masterCatalog) {
    this.masterCatalog = masterCatalog;
  }
  
  public void setBrandLogoUrl(String brandLogoUrl) {
    this.brandLogoUrl = brandLogoUrl;
  }

  public void setMasterDataProductAttributes(
      List<MasterDataProductAttribute> masterDataProductAttributes) {
    this.masterDataProductAttributes = masterDataProductAttributes;
  }

  public void setMasterDataProductImages(List<MasterDataProductImage> masterDataProductImages) {
    this.masterDataProductImages = masterDataProductImages;
  }

  public void setProductName(String productName) {
    this.productName = productName;
  }

  public void setProductStory(String productStory) {
    this.productStory = productStory;
  }

  public void setShippingWeight(double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public void setShippingWeightIfNotZero(double shippingWeight) {
    if (shippingWeight != 0.0)
      this.shippingWeight = shippingWeight;
  }

  public void setSortedDefining(boolean sortedDefining) {
    this.sortedDefining = sortedDefining;
  }

  public void setSortedDefiningAttributes(List<SortedDefiningAttribute> sortedDefiningAttributes) {
    this.sortedDefiningAttributes = sortedDefiningAttributes;
  }

  public void setSpecificationDetail(String specificationDetail) {
    this.specificationDetail = specificationDetail;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
  }

  public void setUom(String uom) {
    this.uom = uom;
  }

  public void setUrl(String url) {
    this.url = url;
  }

  public void setViewable(boolean isViewable) {
    this.isViewable = isViewable;
  }

  public void setWeight(double weight) {
    this.weight = weight;
  }

  public void setWeightIfNotZero(double weight) {
    if (weight != 0.0)
      this.weight = weight;
  }

  public void setWidth(double width) {
    this.width = width;
  }

  public void setWidthIfNotZero(double width) {
    if (width != 0.0)
      this.width = width;
  }

  public boolean isReviewPending() {
    return reviewPending;
  }

  public void setReviewPending(boolean reviewPending) {
    this.reviewPending = reviewPending;
  }

  @Override
  public String toString() {
    return String.format(
        "MasterDataProduct [brand=%s, shippingWeight=%s, specificationDetail=%s, productName=%s, description=%s, longDescription=%s, uniqueSellingPoint=%s, isActivated=%s, isViewable=%s, productStory=%s, uom=%s, masterDataProductImages=%s, masterDataProductAttributes=%s, url=%s, length=%s, width=%s, height=%s, weight=%s, masterCatalog=%s, brandLogoUrl=%s, toString()=%s]",
        this.brand, this.shippingWeight, this.specificationDetail, this.productName,
        this.description, this.longDescription, this.uniqueSellingPoint, this.isActivated,
        this.isViewable, this.productStory, this.uom, this.masterDataProductImages,
        this.masterDataProductAttributes, this.url, this.length, this.width, this.height,
        this.weight, this.masterCatalog, this.brandLogoUrl, super.toString());
  }

  public String getSizeAttributeCode() {
    return sizeAttributeCode;
  }

  public void setSizeAttributeCode(String sizeAttributeCode) {
    this.sizeAttributeCode = sizeAttributeCode;
  }
}
