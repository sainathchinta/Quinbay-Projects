package com.gdn.x.productcategorybase.entity;

import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.ExtractionType;
import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.EnumType;
import jakarta.persistence.Enumerated;
import jakarta.persistence.FetchType;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import jakarta.persistence.UniqueConstraint;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.apache.commons.collections.CollectionUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Setter
@Getter
@NoArgsConstructor
@AllArgsConstructor
@Entity
@Table(name = Category.TABLE_NAME, uniqueConstraints = {@UniqueConstraint(columnNames = {Category.COLUMN_CATALOG_ID,
    Category.COLUMN_PARENT_CATEGORY, Category.COLUMN_SEQUENCE, Category.COLUMN_CATEGORY_CODE, GdnBaseEntity.STORE_ID})})
public class Category extends GdnBaseEntity {

  private static final long serialVersionUID = 5560699758471442022L;

  public static final String TABLE_NAME = "PCC_CATEGORY";
  public static final String COLUMN_CATALOG_TYPE = "CATALOG_TYPE";
  public static final String COLUMN_CATALOG_ID = "CATALOG_ID";
  public static final String COLUMN_PARENT_CATEGORY = "PARENT_CATEGORY_ID";
  public static final String COLUMN_SEQUENCE = "SEQUENCE";
  public static final String COLUMN_NAME = "NAME";
  public static final String COLUMN_CATEGORY_CODE = "CATEGORY_CODE";
  public static final String COLUMN_SHORT_DESCRIPTION = "SHORT_DESCRIPTION";
  public static final String COLUMN_DESCRIPTION = "DESCRIPTION";
  public static final String COLUMN_DEFAULT_DESCRIPTION = "DEFAULT_DESCRIPTION";
  public static final String COLUMN_STATE = "STATE";
  public static final String COLUMN_DISPLAY = "DISPLAY";
  public static final String COLUMN_LOGISTIC_ADJUSTMENT = "LOGISTIC_ADJUSTMENT";
  public static final String COLUMN_WARRANTY = "IS_WARRANTY";
  public static final String COLUMN_INSTALLATION = "IS_INSTALLATION";
  public static final String COLUMN_ACTIVATED = "IS_ACTIVATED";
  public static final String COLUMN_VIEWABLE = "IS_VIEWABLE";
  public static final String COLUMN_INTERNAL_ACTIVATION_INTERVAL = "INTERNAL_ACTIVATION_INTERVAL";
  public static final String COLUMN_NAME_ENGLISH = "NAME_ENGLISH";
  public static final String COLUMN_DESCRIPTION_ENGLISH = "DESCRIPTION_ENGLISH";
  public static final String DANGEROUS_GOODS_LEVEL = "DANGEROUS_GOODS_LEVEL";
  public static final String COLUMN_UMKM = "UMKM";
  public static final String GENERIC_TEMPLATE_ELIGIBLE = "GENERIC_TEMPLATE_ELIGIBLE";
  public static final String COLUMN_WHOLESALE_PRICE_CONFIG_ENABLED = "WHOLESALE_PRICE_CONFIG_ENABLED";
  public static final String COLUMN_DOCUMENT_TYPE = "DOCUMENT_TYPE";
  public static final String COLUMN_OSC_ID = "OSC_ID";
  public static final String COLUMN_OSC_UPDATED_DATE = "OSC_UPDATED_DATE";
  public static final String COLUMN_OSC_UPDATED_BY = "OSC_UPDATED_BY";
  public static final String COLUMN_EXRACTION_TYPE = "EXTRACTION_TYPE";
  public static final String COLUMN_B2B_EXCLUSIVE = "B2B_EXCLUSIVE";
  public static final String COLUMN_HALAL_CATEGORY = "HALAL_CATEGORY";
  public static final String BOPIS_ELIGIBLE = "BOPIS_ELIGIBLE";

  @ManyToOne
  @JoinColumn(name = Category.COLUMN_CATALOG_ID)
  private Catalog catalog;

  @Column(name = COLUMN_CATALOG_ID, insertable = false, updatable = false)
  private String catalogId;

  @ManyToOne(optional = true)
  @JoinColumn(name = Category.COLUMN_PARENT_CATEGORY)
  private Category parentCategory;

  @Column(name = Category.COLUMN_PARENT_CATEGORY, insertable = false, updatable = false)
  private String parentCategoryId;

  @Column(name = Category.COLUMN_NAME)
  private String name;

  @Column(name = Category.COLUMN_CATEGORY_CODE)
  private String categoryCode;

  @Column(name = Category.COLUMN_SEQUENCE)
  private Integer sequence;

  @Column(name = Category.COLUMN_SHORT_DESCRIPTION)
  private String shortDescription;

  @Column(name = Category.COLUMN_DESCRIPTION)
  private byte[] description;

  @Column(name = Category.COLUMN_DEFAULT_DESCRIPTION)
  private byte[] defaultDescription;

  @Column(name = Category.COLUMN_STATE)
  private String state;

  @Column(name = Category.COLUMN_DISPLAY)
  private boolean display;

  @Column(name = Category.COLUMN_LOGISTIC_ADJUSTMENT)
  private Integer logisticAdjustment;

  @Column(name = Category.COLUMN_WARRANTY)
  private boolean warranty;

  @Column(name = Category.COLUMN_INSTALLATION)
  private boolean needIdentity;

  @Column(name = Category.COLUMN_ACTIVATED)
  private boolean activated = false;

  @Column(name = Category.COLUMN_VIEWABLE)
  private boolean viewable = false;

  @Column(name = Category.COLUMN_INTERNAL_ACTIVATION_INTERVAL)
  private Integer internalActivationInterval;

  @Column(name = Category.COLUMN_NAME_ENGLISH)
  private String nameEnglish;

  @Column(name = Category.COLUMN_DESCRIPTION_ENGLISH)
  private byte[] descriptionEnglish;

  @Column(name = Category.DANGEROUS_GOODS_LEVEL)
  private int dangerousGoodsLevel = 0;

  @Column(name = Category.COLUMN_UMKM)
  private boolean umkm;

  @Column(name = Category.GENERIC_TEMPLATE_ELIGIBLE)
  private boolean genericTemplateEligible = false;

  @Column(name = Category.COLUMN_WHOLESALE_PRICE_CONFIG_ENABLED)
  private boolean wholesalePriceConfigEnabled;

  @Column(name = Category.COLUMN_DOCUMENT_TYPE)
  private String documentType;

  @OneToMany(mappedBy = "category", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<CategoryAttribute> categoryAttributes = new ArrayList<CategoryAttribute>();

  @OneToMany(mappedBy = "category", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<ProductCategory> productCategories = new ArrayList<ProductCategory>();

  @OneToMany(mappedBy = "salesCategory", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<CategoryReference> masterCategoryReferences = new ArrayList<CategoryReference>();

  @OneToMany(mappedBy = "masterCategory", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<CategoryReference> salesCategoryReferences = new ArrayList<CategoryReference>();

  @OneToMany(mappedBy = "category", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<CategoryRestrictedKeyword> categoryRestrictedKeywords = new ArrayList<>();

  @ManyToOne
  @JoinColumn(name = Category.COLUMN_OSC_ID)
  private OriginalSalesCategory originalSalesCategory;

  @Column(name = Category.COLUMN_OSC_UPDATED_DATE)
  private Date oscUpdatedDate;

  @Column(name = Category.COLUMN_OSC_UPDATED_BY)
  private String oscUpdatedBy;

  @Enumerated(EnumType.STRING)
  @Column(name = Category.COLUMN_EXRACTION_TYPE)
  private ExtractionType extractionType;

  @Column(name = Category.COLUMN_B2B_EXCLUSIVE)
  private boolean b2bExclusive;

  @Column(name = Category.COLUMN_HALAL_CATEGORY)
  private boolean halalCategory;

  @Column(name = Category.BOPIS_ELIGIBLE)
  private boolean bopisEligible;

  public boolean isGenericTemplateEligible() {
    return genericTemplateEligible;
  }

  public void setGenericTemplateEligible(boolean genericTemplateEligible) {
    this.genericTemplateEligible = genericTemplateEligible;
  }

  public Category(String storeId, String name, Integer sequence) {
    this.setStoreId(storeId);
    this.name = name;
    this.sequence = sequence;
  }

  public Category(String storeId, String categoryCode, String name) {
    this.setStoreId(storeId);
    this.name = name;
    this.categoryCode = categoryCode;
  }

  public List<CategoryReference> getAllSalesCategoryReferences() {
    return salesCategoryReferences;
  }

  public List<CategoryReference> getSalesCategoryReferences() {
    if (CollectionUtils.isEmpty(salesCategoryReferences)) {
      return salesCategoryReferences;
    } else {
      return salesCategoryReferences.stream().filter(salesCategoryRef ->
          filterSalesCategoryByCatalogCode(CatalogType.SALES_CATALOG, salesCategoryRef)
      ).collect(Collectors.toList());
    }
  }

  public List<CategoryReference> getB2bSalesCategoryReferences() {
    if (CollectionUtils.isEmpty(salesCategoryReferences)) {
      return new ArrayList<>();
    } else {
      return salesCategoryReferences.stream().filter(salesCategoryRef ->
          filterSalesCategoryByCatalogCode(CatalogType.B2B_SALES_CATALOG, salesCategoryRef)
      ).collect(Collectors.toList());
    }
  }

  public List<CategoryReference> getHalalSalesCategoryReferences() {
    if (CollectionUtils.isEmpty(salesCategoryReferences)) {
      return new ArrayList<>();
    } else {
      return salesCategoryReferences.stream().filter(this::filterHalalSalesCategory)
      .collect(Collectors.toList());
    }
  }

  private boolean filterSalesCategoryByCatalogCode(CatalogType catalogType, CategoryReference salesCategoryReference) {
    if (Objects.nonNull(salesCategoryReference.getSalesCategory()) &&
        Objects.nonNull(salesCategoryReference.getSalesCategory().getCatalog()) &&
        catalogType.equals(salesCategoryReference.getSalesCategory().getCatalog().getCatalogType())){
      return true;
    }
    return false;
  }

  private boolean filterHalalSalesCategory(CategoryReference salesCategoryReference) {
    return Objects.nonNull(salesCategoryReference.getSalesCategory()) &&
            salesCategoryReference.getSalesCategory().isHalalCategory();
  }

  public void setSalesCategoryReferences(List<CategoryReference> salesCategoryReferences) {
    this.salesCategoryReferences = salesCategoryReferences;
  }

  @Override
  public String toString() {
    return String.format("Category [catalog=%s, parentCategory=%s, name=%s, categoryCode=%s, sequence=%s, "
            + "shortDescription=%s, description=%s, viewable=%s, internalActivationInterval=%s, "
            + "nameEnglish=%s, descriptionEnglish=%s, umkm=%s, dangerousGoodsLevel=%s, genericTemplateEligible=%s, "
            + "documentType=%s, extractionType=%s, b2bExclusive=%s, halalCategory=%s, toString()=%s]",
        this.catalog, this.parentCategory, this.name, this.categoryCode, this.sequence, this.shortDescription,
        this.description == null ? "" : new String(this.description), this.viewable, this.internalActivationInterval,
        this.nameEnglish, this.descriptionEnglish, this.umkm, this.dangerousGoodsLevel, this.genericTemplateEligible,
        this.documentType, this.extractionType, this.b2bExclusive, this.halalCategory, super.toString());
  }

}
