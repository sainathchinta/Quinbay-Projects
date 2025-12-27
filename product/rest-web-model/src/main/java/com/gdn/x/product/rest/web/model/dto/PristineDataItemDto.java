package com.gdn.x.product.rest.web.model.dto;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * Created by govind on 29/08/2017 AD.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineDataItemDto implements Serializable {


  private static final long serialVersionUID = 6030804132583048667L;
  private String pristineId;
  private String pristineMasterId;
  private String pcbProductItemId;
  private String pristineProductName;
  private String pristineBrand;
  private String pristineModel;
  private String pristineCategory;
  private String productCondition;
  private String pristineShortId;
  private String defaultProductCode;
  private List<SalesCategorySequenceDTO> salesCategorySequences;
  private List<ItemCatalogDTO> pristineCategoriesHierarchy;
  private Map<String,String> pristineListingAttributes;
  private String productNameBySimilarParameters;

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
  }

  public String getPcbProductItemId() {
    return pcbProductItemId;
  }

  public void setPcbProductItemId(String pcbProductItemId) {
    this.pcbProductItemId = pcbProductItemId;
  }

  public String getPristineProductName() {
    return pristineProductName;
  }

  public void setPristineProductName(String pristineProductName) {
    this.pristineProductName = pristineProductName;
  }

  public String getPristineModel() {
    return pristineModel;
  }

  public void setPristineModel(String pristineModel) {
    this.pristineModel = pristineModel;
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

  public String getPristineMasterId() {
    return pristineMasterId;
  }

  public void setPristineMasterId(String pristineMasterId) {
    this.pristineMasterId = pristineMasterId;
  }

  public List<SalesCategorySequenceDTO> getSalesCategorySequences() {
    return salesCategorySequences;
  }

  public void setSalesCategorySequences(List<SalesCategorySequenceDTO> salesCategorySequences) {
    this.salesCategorySequences = salesCategorySequences;
  }

  public String getProductCondition() {
    return productCondition;
  }

  public void setProductCondition(String productCondition) {
    this.productCondition = productCondition;
  }

  public List<ItemCatalogDTO> getPristineCategoriesHierarchy() {
    return pristineCategoriesHierarchy;
  }

  public void setPristineCategoriesHierarchy(List<ItemCatalogDTO> pristineCategoriesHierarchy) {
    this.pristineCategoriesHierarchy = pristineCategoriesHierarchy;
  }

  public Map<String, String> getPristineListingAttributes() {
    return pristineListingAttributes;
  }

  public void setPristineListingAttributes(Map<String, String> pristineListingAttributes) {
    this.pristineListingAttributes = pristineListingAttributes;
  }

  public String getProductNameBySimilarParameters() {
    return productNameBySimilarParameters;
  }

  public void setProductNameBySimilarParameters(String productNameBySimilarParameters) {
    this.productNameBySimilarParameters = productNameBySimilarParameters;
  }

  public String getPristineShortId() {
    return pristineShortId;
  }

  public void setPristineShortId(String pristineShortId) {
    this.pristineShortId = pristineShortId;
  }

  public String getDefaultProductCode() {
    return defaultProductCode;
  }

  public void setDefaultProductCode(String defaultProductCode) {
    this.defaultProductCode = defaultProductCode;
  }

  @Override public String toString() {
    final StringBuilder sb = new StringBuilder("PristineDataItemDto{");
    sb.append("pristineId='").append(pristineId).append('\'');
    sb.append(", pcbProductItemId='").append(pcbProductItemId).append('\'');
    sb.append(", pristineMasterId='").append(pristineMasterId).append('\'');
    sb.append(", pristineProductName='").append(pristineProductName).append('\'');
    sb.append(", pristineBrand='").append(pristineBrand).append('\'');
    sb.append(", pristineModel='").append(pristineModel).append('\'');
    sb.append(", productCondition='").append(productCondition).append('\'');
    sb.append(", pristineCategory='").append(pristineCategory).append('\'');
    sb.append(", pristineShortId='").append(pristineShortId).append('\'');
    sb.append(", salesCategorySequences='").append(salesCategorySequences).append('\'');
    sb.append(", pristineCategoriesHierarchy='").append(pristineCategoriesHierarchy).append('\'');
    sb.append(", productNameBySimilarParameters='").append(productNameBySimilarParameters).append('\'');
    sb.append(", pristineListingAttributes=").append(pristineListingAttributes);
    sb.append('}');
    return sb.toString();
  }
}
