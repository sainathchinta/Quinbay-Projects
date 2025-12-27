package com.gdn.x.product.rest.web.model.response;

import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.ItemInfoDTO;
import com.gdn.x.product.rest.web.model.dto.PromoBundlingDTO;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class ItemAndBundlingInfoResponse extends BaseResponse implements Serializable {
  private static final long serialVersionUID = 1111428750720102980L;
  private List<ItemInfoDTO> items = new ArrayList<ItemInfoDTO>();
  private List<PromoBundlingDTO> promoBundlings = new ArrayList<PromoBundlingDTO>();

  public List<ItemInfoDTO> getItems() {
    return items;
  }

  public void setItems(List<ItemInfoDTO> items) {
    this.items = items;
  }

  public List<PromoBundlingDTO> getPromoBundlings() {
    return promoBundlings;
  }

  public void setPromoBundlings(List<PromoBundlingDTO> promoBundlings) {
    this.promoBundlings = promoBundlings;
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }

  @Override
  public boolean equals(Object obj) {
    return GdnObjects.equals(obj, this);
  }

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemAndBundlingInfoResponse{");
    sb.append("items=").append(items);
    sb.append(", promoBundlings=").append(promoBundlings);
    sb.append('}');
    return sb.toString();
  }
}
