package com.gdn.x.product.model.entity;

import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.index.Indexed;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

import java.util.List;
import java.util.Map;

/**
 * Created by keshashah on 14/12/17.
 */
@Document(collection = PristineDataItem.DOCUMENT_NAME)
public class PristineDataItem extends GdnBaseMongoEntity {
  private static final long serialVersionUID = 1151470815765151060L;
  public static final String DOCUMENT_NAME = "prd_pristine";

  @Indexed(unique = true)
  @Field(value = ProductFieldNames.PRISTINE_ID)
  private String pristineId;

  @Indexed
  @Field(value = ProductFieldNames.PRISTINE_MASTER_ID)
  private String pristineMasterId;

  @Transient
  private String pcbProductItemId;

  @Transient
  private String pristineModel;

  @Indexed
  @Field(value = ProductFieldNames.PRISTINE_PRODUCT_CONDITION)
  private String productCondition;

  @Indexed
  @Field(value = ProductFieldNames.PRISTINE_PRODUCT_NAME)
  private String pristineProductName;

  @Field(value = ProductFieldNames.PRISTINE_BRAND)
  private String pristineBrand;

  @Field(value = ProductFieldNames.PRISTINE_CATEGORY)
  private String pristineCategory;

  @Field(value = ProductFieldNames.PRISTINE_DEFAULT_PRODUCT_CODE)
  private String defaultProductCode;

  @Indexed
  @Field(value = ProductFieldNames.PRISTINE_SHORT_ID)
  private String pristineShortId;

  @Field(value = ProductFieldNames.SALES_CATEGORY_SEQUENCES)
  private List<SalesCategorySequence> salesCategorySequences;

  @Field(value = ProductFieldNames.PRISTINE_CATEGORIES_HIERARCHY)
  private List<ItemCatalogVO> pristineCategoriesHierarchy;

  @Transient
  private String productNameBySimilarParameters;

  @Transient
  private List<SalesCategorySequence> oldSalesCategorySequences;

  @Field(value = ProductFieldNames.PRISTINE_LISTING_ATTRIBUTES)
  private Map<String, String> pristineListingAttributes;

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  public String getPristineMasterId() {
    return pristineMasterId;
  }

  public void setPristineMasterId(String pristineMasterId) {
    this.pristineMasterId = pristineMasterId;
  }

  public String getPcbProductItemId() {
    return pcbProductItemId;
  }

  public void setPcbProductItemId(String pcbProductItemId) {
    this.pcbProductItemId = pcbProductItemId;
  }

  public String getPristineModel() {
    return pristineModel;
  }

  public void setPristineModel(String pristineModel) {
    this.pristineModel = pristineModel;
  }

  public String getProductCondition() {
    return productCondition;
  }

  public void setProductCondition(String productCondition) {
    this.productCondition = productCondition;
  }

  public String getPristineProductName() {
    return pristineProductName;
  }

  public void setPristineProductName(String pristineProductName) {
    this.pristineProductName = pristineProductName;
  }

  public String getPristineBrand() {
    return pristineBrand;
  }

  public void setPristineBrand(String pristineBrand) {
    this.pristineBrand = pristineBrand;
  }

  public String getPristineCategory() {
    return pristineCategory;
  }

  public void setPristineCategory(String pristineCategory) {
    this.pristineCategory = pristineCategory;
  }

  public List<SalesCategorySequence> getSalesCategorySequences() {
    return salesCategorySequences;
  }

  public void setSalesCategorySequences(List<SalesCategorySequence> salesCategorySequences) {
    this.salesCategorySequences = salesCategorySequences;
  }

  public List<ItemCatalogVO> getPristineCategoriesHierarchy() {
    return pristineCategoriesHierarchy;
  }

  public void setPristineCategoriesHierarchy(List<ItemCatalogVO> pristineCategoriesHierarchy) {
    this.pristineCategoriesHierarchy = pristineCategoriesHierarchy;
  }

  public String getProductNameBySimilarParameters() {
    return productNameBySimilarParameters;
  }

  public void setProductNameBySimilarParameters(String productNameBySimilarParameters) {
    this.productNameBySimilarParameters = productNameBySimilarParameters;
  }

  public Map<String, String> getPristineListingAttributes() {
    return pristineListingAttributes;
  }

  public void setPristineListingAttributes(Map<String, String> pristineListingAttributes) {
    this.pristineListingAttributes = pristineListingAttributes;
  }

  public String getDefaultProductCode() {
    return defaultProductCode;
  }

  public void setDefaultProductCode(String defaultProductCode) {
    this.defaultProductCode = defaultProductCode;
  }

  public String getPristineShortId() {
    return pristineShortId;
  }

  public void setPristineShortId(String pristineShortId) {
    this.pristineShortId = pristineShortId;
  }

  public List<SalesCategorySequence> getOldSalesCategorySequences() {
    return oldSalesCategorySequences;
  }

  public void setOldSalesCategorySequences(List<SalesCategorySequence> oldSalesCategorySequences) {
    this.oldSalesCategorySequences = oldSalesCategorySequences;
  }
}
