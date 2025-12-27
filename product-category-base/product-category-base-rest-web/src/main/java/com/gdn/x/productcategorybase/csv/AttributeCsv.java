package com.gdn.x.productcategorybase.csv;

import org.supercsv.cellprocessor.FmtBool;
import org.supercsv.cellprocessor.Optional;
import org.supercsv.cellprocessor.ParseBool;
import org.supercsv.cellprocessor.ParseEnum;
import org.supercsv.cellprocessor.ParseInt;
import org.supercsv.cellprocessor.constraint.NotNull;
import org.supercsv.cellprocessor.ift.CellProcessor;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.productcategorybase.AttributeType;

public class AttributeCsv {

  public static final String[] INPUT_HEADER = new String[] {"storeId", "attributeType", "name", "mandatory",
      "searchable", "allowedAttributeValue", "allowedAttributeSequence"};
  public static final String[] ACTUAL_HEADER =
      new String[] {"Store id", "Attribute type", "Attribute Name", "Mandatory", "Searchable",
          "Allowed Attribute Value", "Allowed Attribute Value Sequence", "Attribute id", "Allowed Attribute Value Id"};
  public static final String[] OUTPUT_HEADER = new String[] {"storeId", "attributeType", "name", "mandatory",
      "searchable", "allowedAttributeValue", "allowedAttributeSequence", "id", "allowedAttributeValueId"};
  public static final CellProcessor[] INPUT_PROCESSORS =
      new CellProcessor[] {new NotNull(), new ParseEnum(AttributeType.class), new NotNull(), new ParseBool(),
          new ParseBool(), new Optional(), new Optional(new ParseInt())};
  public static final CellProcessor[] OUTPUT_PROCESSORS =
      new CellProcessor[] {new NotNull(), new NotNull(), new NotNull(), new FmtBool("TRUE", "FALSE"),
          new FmtBool("TRUE", "FALSE"), new Optional(), new Optional(), new NotNull(), new Optional()};
  private String storeId;
  private String name;
  private AttributeType attributeType;
  private boolean searchAble;

  private boolean mandatory;

  private String allowedAttributeValue;

  private Integer allowedAttributeSequence;

  private String id;

  private String allowedAttributeValueId;

  public AttributeCsv() {
    // nothing to do here
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
    AttributeCsv other = (AttributeCsv) obj;
    if (this.attributeType != other.attributeType) {
      return false;
    }
    if (this.mandatory != other.mandatory) {
      return false;
    }
    if (this.name == null) {
      if (other.name != null) {
        return false;
      }
    } else if (!this.name.equals(other.name)) {
      return false;
    }
    if (this.searchAble != other.searchAble) {
      return false;
    }
    if (this.storeId == null) {
      if (other.storeId != null) {
        return false;
      }
    } else if (!this.storeId.equals(other.storeId)) {
      return false;
    }
    return true;
  }


  public Integer getAllowedAttributeSequence() {
    return this.allowedAttributeSequence;
  }

  public String getAllowedAttributeValue() {
    return this.allowedAttributeValue;
  }

  public String getAllowedAttributeValueId() {
    return this.allowedAttributeValueId;
  }

  public AttributeType getAttributeType() {
    return this.attributeType;
  }

  public String getId() {
    return this.id;
  }

  public String getName() {
    return this.name;
  }

  public String getStoreId() {
    return this.storeId;
  }

  public boolean isMandatory() {
    return this.mandatory;
  }

  public boolean isSearchAble() {
    return this.searchAble;
  }

  public void setAllowedAttributeSequence(Integer allowedAttributeSequence) {
    this.allowedAttributeSequence = allowedAttributeSequence;
  }

  public void setAllowedAttributeValue(String allowedAttributeValue) {
    this.allowedAttributeValue = allowedAttributeValue;
  }

  public void setAllowedAttributeValueId(String allowedAttributeValueId) {
    this.allowedAttributeValueId = allowedAttributeValueId;
  }

  public void setAttributeType(AttributeType attributeType) {
    this.attributeType = attributeType;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setMandatory(boolean mandatory) {
    this.mandatory = mandatory;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setSearchAble(boolean searchAble) {
    this.searchAble = searchAble;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  @Override
  public String toString() {
    return String.format(
        "AttributeCsv [name=%s, attributeType=%s, searchAble=%s, mandatory=%s, "
            + "allowedAttributeValue=%s, toString()=%s]",
        this.name, this.attributeType, this.searchAble, this.mandatory, this.allowedAttributeValue,
        super.toString());
  }

}
