package com.gdn.x.productcategorybase.dto.response;

import java.util.ArrayList;
import java.util.List;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.ToString;

@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class CategoryDetailResponse extends CategoryResponse {

  private static final long serialVersionUID = 1431504560402063741L;
  private List<CategoryAttributeResponse> categoryAttributes = new ArrayList<CategoryAttributeResponse>();
  private List<CategoryReferenceResponse> masterCategoryReferences = new ArrayList<CategoryReferenceResponse>();
  private List<CategoryReferenceResponse> salesCategoryReferences = new ArrayList<CategoryReferenceResponse>();
  private List<CategoryReferenceResponse> b2bSalesCategoryReferences = new ArrayList<CategoryReferenceResponse>();
  private List<CategoryReferenceResponse> halalSalesCategoryReferences = new ArrayList<>();

  public CategoryDetailResponse() {}

  public CategoryDetailResponse(CategoryResponse response) {
    super(response.getName(), response.getCategoryCode(), response.getSequence(), response.getShortDescription(),
        response.getDescription(), response.getDefaultDescription(), response.getState(), response.isDisplay(),
        response.getLogisticAdjustment(), response.isWarranty(), response.isNeedIdentity(), response.isViewable(),
        response.getCatalog(), response.getParentCategoryId(), response.getInternalActivationInterval());
  }

  public List<CategoryAttributeResponse> getCategoryAttributes() {
    return this.categoryAttributes;
  }

  public List<CategoryReferenceResponse> getMasterCategoryReferences() {
    return this.masterCategoryReferences;
  }

  public List<CategoryReferenceResponse> getSalesCategoryReferences() {
    return this.salesCategoryReferences;
  }

  public void setCategoryAttributes(List<CategoryAttributeResponse> categoryAttributes) {
    this.categoryAttributes = categoryAttributes;
  }

  public void setMasterCategoryReferences(List<CategoryReferenceResponse> masterCategoryReferences) {
    this.masterCategoryReferences = masterCategoryReferences;
  }

  public void setSalesCategoryReferences(List<CategoryReferenceResponse> salesCategoryReferences) {
    this.salesCategoryReferences = salesCategoryReferences;
  }

  public List<CategoryReferenceResponse> getB2bSalesCategoryReferences() {
    return b2bSalesCategoryReferences;
  }

  public void setB2bSalesCategoryReferences(List<CategoryReferenceResponse> b2bSalesCategoryReferences) {
    this.b2bSalesCategoryReferences = b2bSalesCategoryReferences;
  }

  public List<CategoryReferenceResponse> getHalalSalesCategoryReferences() {
    return halalSalesCategoryReferences;
  }

  public void setHalalSalesCategoryReferences(List<CategoryReferenceResponse> halalSalesCategoryReferences) {
    this.halalSalesCategoryReferences = halalSalesCategoryReferences;
  }
}
