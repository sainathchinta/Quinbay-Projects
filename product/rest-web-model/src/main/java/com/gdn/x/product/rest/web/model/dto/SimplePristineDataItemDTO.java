package com.gdn.x.product.rest.web.model.dto;

import java.io.Serializable;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * Created by govind on 16/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimplePristineDataItemDTO implements Serializable{

  private static final long serialVersionUID = 235585955158293218L;
  private String pristineId;
  private String pristineProductName;
  private String pristineBrand;
  private String pristineCategory;
  private String pristineMasterId;
  private List<SalesCategorySequenceDTO> salesCategorySequences;
  private List<ItemCatalogDTO> pristineCategoriesHierarchy;

  public String getPristineId() {
    return pristineId;
  }

  public void setPristineId(String pristineId) {
    this.pristineId = pristineId;
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

  public List<SalesCategorySequenceDTO> getSalesCategorySequences() {
    return salesCategorySequences;
  }

  public void setSalesCategorySequences(List<SalesCategorySequenceDTO> salesCategorySequences) {
    this.salesCategorySequences = salesCategorySequences;
  }

  public List<ItemCatalogDTO> getPristineCategoriesHierarchy() {
    return pristineCategoriesHierarchy;
  }

  public void setPristineCategoriesHierarchy(List<ItemCatalogDTO> pristineCategoriesHierarchy) {
    this.pristineCategoriesHierarchy = pristineCategoriesHierarchy;
  }

  public String getPristineMasterId() {
    return pristineMasterId;
  }

  public void setPristineMasterId(String pristineMasterId) {
    this.pristineMasterId = pristineMasterId;
  }

  @Override
  public String toString() {
    return new ToStringBuilder(this).append("pristineId", pristineId)
        .append("pristineProductName", pristineProductName).append("pristineBrand", pristineBrand)
        .append("pristineCategory", pristineCategory)
        .append("pristineMasterId", pristineMasterId)
        .append("salesCategorySequences", salesCategorySequences)
        .append("pristineCategoriesHierarchy", pristineCategoriesHierarchy).toString();
  }
}
