package com.gdn.x.product.domain.event.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * Created by govind on 29/08/2017 AD.
 */

@JsonIgnoreProperties(ignoreUnknown = true)
public class PristineDataItemEventModel extends GdnBaseDomainEventModel implements Serializable {


  private static final long serialVersionUID = 6030804132583048667L;
  private String pcbProductItemId;
  private String pristineId;
  private String pristineMasterId;
  private String pristineProductName;
  private String pristineModel;
  private String productCondition;
  private String pristineBrand;
  private String pristineCategory;
  private String defaultProductCode;
  private String pristineShortId;
  private Map<String,String> pristineListingAttributes;
  private List<ItemCatalogVO> pristineCategoriesHierarchy;
  private List<SalesCategorySequence> salesCategorySequences;
  private List<SalesCategorySequence> oldSalesCategorySequences;


  public String getPcbProductItemId() {
    return pcbProductItemId;
  }

  public void setPcbProductItemId(String pcbProductItemId) {
    this.pcbProductItemId = pcbProductItemId;
  }

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

  public Map<String, String> getPristineListingAttributes() {
    return pristineListingAttributes;
  }

  public void setPristineListingAttributes(Map<String, String> pristineListingAttributes) {
    this.pristineListingAttributes = pristineListingAttributes;
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

  public List<ItemCatalogVO> getPristineCategoriesHierarchy() {
    return pristineCategoriesHierarchy;
  }

  public void setPristineCategoriesHierarchy(List<ItemCatalogVO> pristineCategoriesHierarchy) {
    this.pristineCategoriesHierarchy = pristineCategoriesHierarchy;
  }

  public List<SalesCategorySequence> getSalesCategorySequences() {
    return salesCategorySequences;
  }

  public void setSalesCategorySequences(List<SalesCategorySequence> salesCategorySequences) {
    this.salesCategorySequences = salesCategorySequences;
  }

  public List<SalesCategorySequence> getOldSalesCategorySequences() {
    return oldSalesCategorySequences;
  }

  public void setOldSalesCategorySequences(List<SalesCategorySequence> oldSalesCategorySequences) {
    this.oldSalesCategorySequences = oldSalesCategorySequences;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("pcbProductItemId", pcbProductItemId)
        .append("pristineId", pristineId).append("pristineMasterId", pristineMasterId)
        .append("pristineProductName", pristineProductName).append("pristineModel", pristineModel)
        .append("productCondition", productCondition).append("pristineBrand", pristineBrand)
        .append("pristineCategory", pristineCategory)
        .append("defaultProductCode", defaultProductCode).append("pristineShortId", pristineShortId)
        .append("pristineListingAttributes", pristineListingAttributes)
        .append("pristineCategoriesHierarchy", pristineCategoriesHierarchy)
        .append("oldSalesCategorySequences", oldSalesCategorySequences)
        .append("salesCategorySequences", salesCategorySequences).toString();
  }
}
