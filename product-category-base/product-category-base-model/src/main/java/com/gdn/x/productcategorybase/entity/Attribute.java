package com.gdn.x.productcategorybase.entity;

import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.AttributeType;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = Attribute.TABLE_NAME, uniqueConstraints = {
    @UniqueConstraint(columnNames = {Attribute.COLUMN_ATTRIBUTE_CODE})})
public class Attribute extends GdnBaseEntity {

  private static final long serialVersionUID = -2712691191433652133L;

  public static final String TABLE_NAME = "PCC_ATTRIBUTE";
  public static final String COLUMN_ATTRIBUTE_NAME = "NAME";
  public static final String COLUMN_ATTRIBUTE_CODE = "ATTRIBUTE_CODE";
  public static final String COLUMN_ATTRIBUTE_TYPE = "ATTRIBUTE_TYPE";
  public static final String COLUMN_SIZE_ATTRIBUTE = "SIZE_ATTRIBUTE";
  public static final String COLUMN_VALUE_TYPES = "VALUE_TYPES";
  public static final String COLUMN_VALUE_TYPE_ATTRIBUTE = "VALUE_TYPE_ATTRIBUTE";
  public static final String COLUMN_DATA_TYPE = "DATA_TYPE";
  public static final String COLUMN_IS_SEARCHABLE = "IS_SEARCH_ABLE";
  public static final String COLUMN_MANDATORY = "MANDATORY";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_DESCRIPTION_SEARCH = "DESCRIPTION_SEARCH";
  public static final String COLUMN_IS_SKU_VALUE = "IS_SKU_VALUE";
  public static final String COLUMN_EXAMPLE = "EXAMPLE";
  public static final String COLUMN_IS_BASIC_VIEW = "IS_BASIC_VIEW";
  public static final String COLUMN_NAME_ENGLISH = "NAME_ENGLISH";
  public static final String COLUMN_DESCRIPTION_ENGLISH = "DESCRIPTION_ENGLISH";
  public static final String COLUMN_SORT_TYPE = "SORT_TYPE";
  public static final String COLUMN_SCREENING_MANDATORY = "SCREENING_MANDATORY";
  public static final String COLUMN_VARIANT_CREATING_UI = "VARIANT_CREATING_UI";
  public static final String COLUMN_VARIANT_CREATION = "VARIANT_CREATION";
  public static final String COLUMN_DS_ATTRIBUTE_NAME = "DS_ATTRIBUTE_NAME";
  public static final String COLUMN_MUST_SHOW_ON_CUSTOMER_SIDE = "MUST_SHOW_ON_CUSTOMER_SIDE";
  public static final String COLUMN_ATTRIBUTE_IMAGE_URL = "ATTRIBUTE_IMAGE_URL";
  public static final String COLUMN_DS_EXTRACTION = "DS_EXTRACTION";
  public static final String COLUMN_HIDE_FOR_SELLER = "HIDE_FOR_SELLER";
  public static final String COLUMN_HIDE_FOR_CUSTOMER = "HIDE_FOR_CUSTOMER";
  public static final String COLUMN_MULTI_LANGUAGE = "MULTI_LANGUAGE";

  @Column(name = Attribute.COLUMN_ATTRIBUTE_NAME)
  private String name;

  @Column(name = Attribute.COLUMN_ATTRIBUTE_CODE)
  private String attributeCode;

  @Enumerated(EnumType.STRING)
  @Column(name = Attribute.COLUMN_ATTRIBUTE_TYPE)
  private AttributeType attributeType;

  @Column(name = Attribute.COLUMN_IS_SEARCHABLE, nullable = false)
  private boolean searchAble = false;

  @OneToMany(mappedBy = "attribute", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<AllowedAttributeValue>();

  @OneToMany(mappedBy = "attribute", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues =
      new ArrayList<PredefinedAllowedAttributeValue>();

  @OneToMany(mappedBy = "attribute", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<CategoryAttribute> categoryAttributes = new ArrayList<CategoryAttribute>();

  @Column(name = Attribute.COLUMN_MANDATORY)
  private boolean mandatory = false;

  @Column(name = Attribute.COLUMN_SIZE_ATTRIBUTE)
  private boolean sizeAttribute = false;

  @Column(name = Attribute.COLUMN_VALUE_TYPES)
  private String valueTypes;

  @Column(name = Attribute.COLUMN_ATTRIBUTE_IMAGE_URL)
  private String attributeImageUrl;

  @Column(name = Attribute.COLUMN_VALUE_TYPE_ATTRIBUTE)
  private boolean valueTypeAttribute;

  @Column(name = Attribute.COLUMN_DESCRIPTION)
  private byte[] description;

  @Column(name = Attribute.COLUMN_DESCRIPTION_SEARCH)
  private String  descriptionSearch;

  @Column(name = Attribute.COLUMN_IS_SKU_VALUE)
  private boolean skuValue = false;

  @Column(name = Attribute.COLUMN_EXAMPLE)
  private String example;

  @Column(name = Attribute.COLUMN_IS_BASIC_VIEW)
  private boolean isBasicView;

  @Column(name = Attribute.COLUMN_NAME_ENGLISH)
  private String nameEnglish;

  @Column(name = Attribute.COLUMN_DESCRIPTION_ENGLISH)
  private byte[] descriptionEnglish;

  @Enumerated(EnumType.STRING)
  @Column(name = Attribute.COLUMN_SORT_TYPE)
  private AttributeSortType sortType;

  @Column(name = Attribute.COLUMN_SCREENING_MANDATORY)
  private boolean screeningMandatory;

  @Column(name = Attribute.COLUMN_VARIANT_CREATING_UI)
  private boolean variantCreatingUI;

  @Column(name = Attribute.COLUMN_VARIANT_CREATION)
  private boolean variantCreation = false;

  @Column(name = Attribute.COLUMN_DS_ATTRIBUTE_NAME)
  private String dsAttributeName;

  @Column(name = Attribute.COLUMN_MUST_SHOW_ON_CUSTOMER_SIDE)
  private boolean mustShowOnCustomerSide = false;

  @Column(name = Attribute.COLUMN_DS_EXTRACTION)
  private boolean dsExtraction;

  @Column(name = Attribute.COLUMN_HIDE_FOR_SELLER)
  private boolean hideForSeller;

  @Column(name = Attribute.COLUMN_HIDE_FOR_CUSTOMER)
  private boolean hideForCustomer;

  @Column(name = Attribute.COLUMN_MULTI_LANGUAGE)
  private boolean multiLanguage;

  public Attribute(String name, AttributeType attributeType, boolean searchAble, String storeId) {
    this(name, attributeType, searchAble, true, storeId);
  }

  public Attribute(String name, AttributeType attributeType, boolean searchAble,
      boolean isBasicView, String storeId) {
    this.name = name;
    this.attributeType = attributeType;
    this.searchAble = searchAble;
    this.isBasicView = isBasicView;
    this.setStoreId(storeId);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("Attribute{");
    sb.append("name='").append(name).append('\'');
    sb.append(", attributeCode='").append(attributeCode).append('\'');
    sb.append(", attributeType=").append(attributeType);
    sb.append(", sizeAttribute=").append(sizeAttribute);
    sb.append(", valueTypes=").append(valueTypes);
    sb.append(", attributeImageUrl=").append(attributeImageUrl);
    sb.append(", valueTypeAttribute=").append(valueTypeAttribute);
    sb.append(", searchAble=").append(searchAble);
    sb.append(", allowedAttributeValues=").append(allowedAttributeValues);
    sb.append(", predefinedAllowedAttributeValues=").append(predefinedAllowedAttributeValues);
    sb.append(", categoryAttributes=").append(categoryAttributes);
    sb.append(", mandatory=").append(mandatory);
    sb.append(", description=").append(Arrays.toString(description));
    sb.append(", descriptionSearch=").append(descriptionSearch);
    sb.append(", skuValue=").append(skuValue);
    sb.append(", example='").append(example).append('\'');
    sb.append(", isBasicView=").append(isBasicView);
    sb.append(", nameEnglish='").append(nameEnglish).append('\'');
    sb.append(", descriptionEnglish=").append(Arrays.toString(descriptionEnglish));
    sb.append(", sortType=").append(sortType);
    sb.append(", screeningMandatory=").append(screeningMandatory);
    sb.append(", variantCreatingUI=").append(variantCreatingUI);
    sb.append(", variantCreation=").append(variantCreation);
    sb.append(", dsAttributeName=").append(dsAttributeName);
    sb.append(", mustShowOnCustomerSide=").append(mustShowOnCustomerSide);
    sb.append(", dsExtraction=").append(dsExtraction);
    sb.append(", hideForSeller=").append(hideForSeller);
    sb.append(", hideForCustomer=").append(hideForCustomer);
    sb.append(", multiLanguage=").append(multiLanguage);
    sb.append('}');
    return sb.toString();
  }
}
