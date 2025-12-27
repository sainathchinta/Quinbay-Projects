package com.gdn.x.productcategorybase.csv;

import org.supercsv.cellprocessor.FmtBool;
import org.supercsv.cellprocessor.Optional;
import org.supercsv.cellprocessor.ParseBool;
import org.supercsv.cellprocessor.ParseInt;
import org.supercsv.cellprocessor.constraint.NotNull;
import org.supercsv.cellprocessor.ift.CellProcessor;

import com.gdn.common.base.GdnObjects;


public class CategoryCsv {
  public static final String[] INPUT_HEADER =
      new String[] {"storeId", "description", "name", "needIdentity", "sequence", "shortDescription", "warranty",
          "catalogId", "parentCategory", "attributeId", "attributeSequence", "mainDefiningAttribute", "USP"};
  public static final String[] ACTUAL_HEADER = new String[] {"Store Id", "Description", "Category Name", "Installation",
      "Sequence", "Short Description", "Warranty", "Catalog Id", "Parent Category", "Attribute Id",
      "Attribute Sequence", "Is Main Defining Attribute", "Is USP", "Category Id"};
  public static final String[] OUTPUT_HEADER =
      new String[] {"storeId", "description", "name", "needIdentity", "sequence", "shortDescription", "warranty",
          "catalogId", "parentCategory", "attributeId", "attributeSequence", "mainDefiningAttribute", "USP", "id"};
  public static final CellProcessor[] INPUT_PROCESSORS = new CellProcessor[] {new NotNull(), new NotNull(),
      new NotNull(), new ParseBool(), new ParseInt(), new NotNull(), new ParseBool(), new NotNull(), new Optional(),
      new NotNull(), new ParseInt(), new ParseBool(), new ParseBool()};
  public static final CellProcessor[] OUTPUT_PROCESSORS =
      new CellProcessor[] {new NotNull(), new NotNull(), new NotNull(), new FmtBool("TRUE", "FALSE"), new NotNull(),
          new NotNull(), new FmtBool("TRUE", "FALSE"), new NotNull(), new Optional(), new NotNull(), new NotNull(),
          new FmtBool("TRUE", "FALSE"), new FmtBool("TRUE", "FALSE"), new NotNull()};

  private String catalogId;
  private String attributeId;
  private String storeId;
  private boolean USP;
  private boolean mainDefiningAttribute;
  private String id;
  private Integer attributeSequence;
  private String parentCategory;
  private String name;
  private Integer sequence;
  private String shortDescription;
  private String description;
  private boolean warranty;
  private boolean needIdentity;
  private boolean activated;

  public CategoryCsv() {}

  public CategoryCsv(String storeId, String name, Integer sequence) {
    this.name = name;
    this.sequence = sequence;
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
    CategoryCsv other = (CategoryCsv) obj;
    if (this.catalogId == null) {
      if (other.catalogId != null) {
        return false;
      }
    } else if (!this.catalogId.equals(other.catalogId)) {
      return false;
    }
    if (this.name == null) {
      if (other.name != null) {
        return false;
      }
    } else if (!this.name.equals(other.name)) {
      return false;
    }
    if (this.needIdentity != other.needIdentity) {
      return false;
    }
    if (this.sequence == null) {
      if (other.sequence != null) {
        return false;
      }
    } else if (!this.sequence.equals(other.sequence)) {
      return false;
    }
    if (this.shortDescription == null) {
      if (other.shortDescription != null) {
        return false;
      }
    } else if (!this.shortDescription.equals(other.shortDescription)) {
      return false;
    }
    if (this.warranty != other.warranty) {
      return false;
    }
    return true;
  }

  public String getAttributeId() {
    return this.attributeId;
  }

  public Integer getAttributeSequence() {
    return this.attributeSequence;
  }

  public String getCatalogId() {
    return this.catalogId;
  }

  public String getDescription() {
    return this.description;
  }

  public String getId() {
    return this.id;
  }

  public String getName() {
    return this.name;
  }

  public String getParentCategory() {
    return this.parentCategory;
  }

  public Integer getSequence() {
    return this.sequence;
  }

  public String getShortDescription() {
    return this.shortDescription;
  }

  public String getStoreId() {
    return this.storeId;
  }

  public boolean isActivated() {
    return this.activated;
  }

  public boolean isMainDefiningAttribute() {
    return this.mainDefiningAttribute;
  }

  public boolean isNeedIdentity() {
    return this.needIdentity;
  }

  public boolean isUSP() {
    return this.USP;
  }

  public boolean isWarranty() {
    return this.warranty;
  }

  public void setActivated(boolean activated) {
    this.activated = activated;
  }

  public void setAttributeId(String attributeId) {
    this.attributeId = attributeId;
  }

  public void setAttributeSequence(Integer attributeSequence) {
    this.attributeSequence = attributeSequence;
  }


  public void setCatalogId(String catalogId) {
    this.catalogId = catalogId;
  }

  public void setDescription(String description) {
    this.description = description;
  }

  public void setId(String id) {
    this.id = id;
  }

  public void setMainDefiningAttribute(boolean isMainDefiningAttribute) {
    this.mainDefiningAttribute = isMainDefiningAttribute;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void setNeedIdentity(boolean needIdentity) {
    this.needIdentity = needIdentity;
  }

  public void setParentCategory(String parentCategory) {
    this.parentCategory = parentCategory;
  }

  public void setSequence(Integer sequence) {
    this.sequence = sequence;
  }

  public void setShortDescription(String shortDescription) {
    this.shortDescription = shortDescription;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public void setUSP(boolean isUSP) {
    this.USP = isUSP;
  }

  public void setWarranty(boolean warranty) {
    this.warranty = warranty;
  }

  @Override
  public String toString() {
    return String.format(
        "Category [parentCategory=%s, name=%s, sequence=%s, shortDescription=%s, description=%s, "
            + "toString()=%s]",
        this.parentCategory, this.name, this.sequence, this.shortDescription,
        this.description == null ? "" : new String(this.description), super.toString());
  }

}
