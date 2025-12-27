package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.web.base.BaseResponse;
import lombok.Builder;

@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemTypeResponse extends BaseResponse {

  private static final long serialVersionUID = -8429558003436952215L;
  private boolean offlineItem;

  public boolean isOfflineItem() {
    return offlineItem;
  }

  public void setOfflineItem(boolean offlineItem) {
    this.offlineItem = offlineItem;
  }

  @Override
  public boolean equals(Object object) {
    return GdnObjects.equals(this, object);
  }

  @Override
  public int hashCode() {
    return GdnObjects.hashCode(this);
  }


  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemTypeResponse{");
    sb.append("offlineItem=").append(offlineItem);
    sb.append('}');
    return sb.toString();
  }
}
