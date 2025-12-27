package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.List;
import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.SalesCategorySequence;

/**
 * Created by govind on 05/08/2018 AD.
 */
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude(value = JsonInclude.Include.NON_EMPTY)
public class SimplePristineDataItemVO implements Serializable{

  private static final long serialVersionUID = -9104404281782873290L;
  private String pristineId;
  private String pristineProductName;
  private String pristineBrand;
  private String pristineCategory;
  private String pristineMasterId;
  private List<SalesCategorySequence> salesCategorySequences;
  private List<ItemCatalogVO> pristineCategoriesHierarchy;

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

  public String getPristineMasterId() {
    return pristineMasterId;
  }

  public void setPristineMasterId(String pristineMasterId) {
    this.pristineMasterId = pristineMasterId;
  }

  @JsonIgnore
  public static SimplePristineDataItemVO toSimplePristineDataItemVO(PristineDataItem pristineDataItem){
    SimplePristineDataItemVO simplePristineDataItemVO = new SimplePristineDataItemVO();
    simplePristineDataItemVO.setPristineId(pristineDataItem.getPristineId());
    simplePristineDataItemVO.setPristineBrand(pristineDataItem.getPristineBrand());
    simplePristineDataItemVO.setPristineCategory(pristineDataItem.getPristineCategory());
    simplePristineDataItemVO.setPristineMasterId(pristineDataItem.getPristineMasterId());
    simplePristineDataItemVO.setPristineProductName(pristineDataItem.getPristineProductName());
    simplePristineDataItemVO.setPristineCategoriesHierarchy(pristineDataItem.getPristineCategoriesHierarchy());
    simplePristineDataItemVO.setSalesCategorySequences(pristineDataItem.getSalesCategorySequences());
    return simplePristineDataItemVO;
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
