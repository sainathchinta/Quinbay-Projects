package com.gdn.x.product.domain.event.model;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.entity.GdnBaseDomainEventModel;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;

@JsonIgnoreProperties(ignoreUnknown = true)
public class SolrAddProductEvent extends GdnBaseDomainEventModel implements Serializable {

  private static final long serialVersionUID = -8871622112158372693L;
  private MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo;
  private List<String> productSkus;
  private String storeId;
  private boolean clearCache;

  public boolean isClearCache() {
    return clearCache;
  }

  public void setClearCache(boolean clearCache) {
    this.clearCache = clearCache;
  }

  public List<String> getProductSkus() {
    return productSkus;
  }

  public void setProductSkus(List<String> productSkus) {
    this.productSkus = productSkus;
  }

  public String getStoreId() {
    return storeId;
  }

  public void setStoreId(String storeId) {
    this.storeId = storeId;
  }

  public MasterDataDetailWithProductAndItemsResponseVo getMasterDataDetailWithProductAndItemsResponseVo() {
    return masterDataDetailWithProductAndItemsResponseVo;
  }

  public void setMasterDataDetailWithProductAndItemsResponseVo(
      MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo) {
    this.masterDataDetailWithProductAndItemsResponseVo = masterDataDetailWithProductAndItemsResponseVo;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("SolrAddProductEvent{");
    sb.append("masterDataDetailWithProductAndItemsResponseVo=").append(masterDataDetailWithProductAndItemsResponseVo);
    sb.append(", productSkus=").append(productSkus);
    sb.append(", storeId='").append(storeId).append('\'');
    sb.append(", clearCache=").append(clearCache);
    sb.append('}');
    return sb.toString();
  }
}
