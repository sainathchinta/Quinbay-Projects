package com.gdn.x.product.model.vo;

import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;

import java.io.Serializable;
import java.util.List;

public class ItemAndBundlingInfoVO extends BaseResponse implements Serializable {

  private static final long serialVersionUID = 4046977369559966496L;
  private List<ItemInfoVO> items;
  private List<PromoBundlingVO> promoBundlings;

  public ItemAndBundlingInfoVO() {
  }

  public ItemAndBundlingInfoVO(List<ItemInfoVO> items, List<PromoBundlingVO> promoBundlings) {
    this.items = items;
    this.promoBundlings = promoBundlings;
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  public List<ItemInfoVO> getItems() {
    return items;
  }

  public List<PromoBundlingVO> getPromoBundlings() {
    return promoBundlings;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  public void setItems(List<ItemInfoVO> items) {
    this.items = items;
  }

  public void setPromoBundlings(List<PromoBundlingVO> promoBundlings) {
    this.promoBundlings = promoBundlings;
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemAndBundlingInfoVO{");
    sb.append("items=").append(items);
    sb.append(", promoBundlings=").append(promoBundlings);
    sb.append('}');
    return sb.toString();
  }
}
