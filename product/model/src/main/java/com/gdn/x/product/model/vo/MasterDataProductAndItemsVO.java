package com.gdn.x.product.model.vo;

import java.io.Serializable;
import java.util.Map;

import com.gdn.common.base.GdnObjects;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;

public class MasterDataProductAndItemsVO implements Serializable {

  private static final long serialVersionUID = 1L;

  private MasterDataProduct masterDataProduct;
  private Map<String, MasterDataItem> masterDataItems;
  private Map<String, String> categoryCodeAndCategoryNameMap;

  public MasterDataProductAndItemsVO() {}


  public MasterDataProductAndItemsVO(MasterDataProduct masterDataProduct,
      Map<String, MasterDataItem> masterDataItems) {
    super();
    this.masterDataProduct = masterDataProduct;
    this.masterDataItems = masterDataItems;
  }

  public MasterDataProductAndItemsVO(MasterDataProduct masterDataProduct, Map<String, MasterDataItem> masterDataItems,
      Map<String, String> categoryCodeAndCategoryNameMap) {
    this.masterDataProduct = masterDataProduct;
    this.masterDataItems = masterDataItems;
    this.categoryCodeAndCategoryNameMap = categoryCodeAndCategoryNameMap;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  public Map<String, MasterDataItem> getMasterDataItems() {
    return this.masterDataItems;
  }

  public MasterDataProduct getMasterDataProduct() {
    return this.masterDataProduct;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }


  public void setMasterDataItems(Map<String, MasterDataItem> masterDataItems) {
    this.masterDataItems = masterDataItems;
  }

  public void setMasterDataProduct(MasterDataProduct masterDataProduct) {
    this.masterDataProduct = masterDataProduct;
  }

  public Map<String, String> getCategoryCodeAndCategoryNameMap() {
    return categoryCodeAndCategoryNameMap;
  }

  public void setCategoryCodeAndCategoryNameMap(Map<String, String> categoryCodeAndCategoryNameMap) {
    this.categoryCodeAndCategoryNameMap = categoryCodeAndCategoryNameMap;
  }

  @Override
  public String toString() {
    return String.format(
        "MasterDataProductAndItemsVO [masterDataProduct=%s, masterDataItems=%s, categoryCodeAndCategoryNameMap=%s, toString()=%s]",
        this.masterDataProduct, this.masterDataItems, categoryCodeAndCategoryNameMap, super.toString());
  }
}
