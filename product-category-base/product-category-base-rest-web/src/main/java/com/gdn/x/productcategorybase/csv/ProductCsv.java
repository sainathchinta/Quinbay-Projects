package com.gdn.x.productcategorybase.csv;

import org.supercsv.cellprocessor.ConvertNullTo;
import org.supercsv.cellprocessor.Optional;
import org.supercsv.cellprocessor.ParseDouble;
import org.supercsv.cellprocessor.ParseInt;
import org.supercsv.cellprocessor.constraint.NotNull;
import org.supercsv.cellprocessor.ift.CellProcessor;

import com.gdn.common.base.GdnObjects;


public class ProductCsv {

  public static final String[] INPUT_HEADER = new String[] {"storeId", "brand", "name", "productCode", "description",
      "longDescription", "width", "height", "weight", "length", "shippingWeight", "uniqueSellingPoint", "UOM",
      "categoryId", "attributeId", "productAttributeName", "sequence", "allowedAttributeId",
      "descriptiveAttributeValueType", "descriptiveAttributeValue"};

  public static final CellProcessor[] INPUT_PROCESSORS = new CellProcessor[] {new NotNull(), new NotNull(),
      new NotNull(), new NotNull(), new NotNull(), new NotNull(), new ParseDouble(), new ParseDouble(),
      new ParseDouble(), new ParseDouble(), new ParseDouble(), new NotNull(), new NotNull(), new NotNull(),
      new NotNull(), new NotNull(), new ParseInt(), new Optional(), new Optional(), new Optional()};

  public static final String[] ACTUAL_HEADER =
      new String[] {"Store Id", "Product Id", "Product Item ID", "Product Item Name", "SKU Code", "UPC Code"};

  public static final String[] OUTPUT_HEADER =
      new String[] {"storeId", "productId", "id", "generatedItemName", "skuCode", null};

  public static final CellProcessor[] OUTPUT_PROCESSORS = new CellProcessor[] {new NotNull(), new NotNull(),
      new NotNull(), new NotNull(), new NotNull(), new ConvertNullTo("")};

  private String storeId;
  private String productCode;
  private String name;
  private Double length;
  private Double width;
  private Double height;
  private Double weight;
  private Double shippingWeight;
  private String description;
  private String longDescription;
  private String brand;
  private String uniqueSellingPoint;
  private String uom;
  private Integer sequence;
  private boolean activated;
  private String attributeId;
  private String productAttributeName;
  private String allowedAttributeId;
  private String descriptiveAttributeValueType;
  private String descriptiveAttributeValue;
  private String categoryId;

  public ProductCsv() {
    // nothing to do here
  }

  public ProductCsv(String productCode, String name, Double length, Double width, Double weight, Double height,
      Double shippingWeight, String description, String brand, String uniqueSellingPoint, String uom, String storeId) {
    this.productCode = productCode;
    this.name = name;
    this.length = length;
    this.width = width;
    this.weight = weight;
    this.height = height;
    this.shippingWeight = shippingWeight;
    this.description = description;
    this.brand = brand;
    this.uniqueSellingPoint = uniqueSellingPoint;
    this.uom = uom;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }
  
  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (this.getClass() != obj.getClass()) {
      return false;
    }
    ProductCsv other = (ProductCsv) obj;
    if (this.productCode == null) {
      if (other.productCode != null) {
        return false;
      }
    } else if (!this.productCode.equals(other.productCode)) {
      return false;
    }
    return true;
  }

  public String getAllowedAttributeId() {
    return this.allowedAttributeId;
  }

  public String getAttributeId() {
    return this.attributeId;
  }

  public String getBrand() {
    return this.brand;
  }

  public String getCategoryId() {
    return this.categoryId;
  }

  public String getDescription() {
    return this.description;
  }

  public String getDescriptiveAttributeValue() {
    return this.descriptiveAttributeValue;
  }

  public String getDescriptiveAttributeValueType() {
    return this.descriptiveAttributeValueType;
  }

  public Double getHeight() {
    return this.height;
  }

  public Double getLength() {
    return this.length;
  }

  public String getLongDescription() {
    return this.longDescription;
  }

  public String getName() {
    return this.name;
  }

  public String getProductAttributeName() {
    return this.productAttributeName;
  }

  public String getProductCode() {
    return this.productCode;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public Double getShippingWeight() {
    return this.shippingWeight;
  }

  public String getStoreId() {
    return this.storeId;
  }

  public String getUniqueSellingPoint() {
    return this.uniqueSellingPoint;
  }

  public String getUom() {
    return this.uom;
  }

  public Double getWeight() {
    return this.weight;
  }

  public Double getWidth() {
    return this.width;
  }

  public boolean isActivated() {
    return this.activated;
  }

  public void setActivated(boolean activated) {
    this.activated = activated;
  }

  public void setAllowedAttributeId(String allowedAttributeId) {
    this.allowedAttributeId = allowedAttributeId;
  }

  public void setAttributeId(String attributeId) {
    this.attributeId = attributeId;
  }

  public void setBrand(String brand) {
    this.brand = brand;
  }

  public void setCategoryId(String categoryId) {
    this.categoryId = categoryId;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setDescriptiveAttributeValue(String descriptiveAttributeValue) {
    this.descriptiveAttributeValue = descriptiveAttributeValue;
  }

  public void setDescriptiveAttributeValueType(String descriptiveAttributeValueType) {
    this.descriptiveAttributeValueType = descriptiveAttributeValueType;
  }

  public void setHeight(Double height) {
    this.height = height;
  }

  public void setLength(Double length) {
    this.length = length;
  }

  public void setLongDescription(String longDescription) {
    this.longDescription = longDescription;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setProductAttributeName(String productAttributeName) {
    this.productAttributeName = productAttributeName;
  }

  public void setProductCode(String productCode) {
    this.productCode = productCode;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setShippingWeight(Double shippingWeight) {
    this.shippingWeight = shippingWeight;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public void setUniqueSellingPoint(String uniqueSellingPoint) {
    this.uniqueSellingPoint = uniqueSellingPoint;
  }

  public void setUom(String uom) {
    this.uom = uom;
  }

  public void setWeight(Double weight) {
    this.weight = weight;
  }

  public void setWidth(Double width) {
    this.width = width;
  }

  @Override
  public String toString() {
    return String.format(
        "Product [productCode = %s, name=%s, length=%s, width=%s, height=%s, weight=%s, "
            + "shippingWeight=%s, description=%s, longDescription=%s, brand=%s, uniqueSellingPoint=%s, "
            + "uom=%s, activated=%s]",
        this.productCode, this.name, this.length, this.width, this.height, this.weight,
        this.shippingWeight, this.longDescription, this.brand, this.uniqueSellingPoint, this.uom,
        this.activated);
  }

}
