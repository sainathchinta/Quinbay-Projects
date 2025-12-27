package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemInfo;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemInfoResponseV2 extends BaseResponse {

  private static final long serialVersionUID = -4551155664082522806L;

  private String merchantCode;
  private String itemSku;
  private String productSku;
  private String merchantSku;
  private String itemCode;
  private boolean isSynchronized;
  private String itemCatentryId;
  private MasterDataItemInfo masterDataItem;
  private PristineDataItemDto pristineDataItem;
  private Boolean isLateFulfillment;
  private String ticketTemplateCode;
  private String etdNote;
  private boolean cncActivated;
  private String uniqueId;
  private boolean isSubscribable;
  private boolean archived;

  @Override
  public String toString() {
    final StringBuilder sb = new StringBuilder("ItemInfoResponse{");
    sb.append("merchantCode='").append(merchantCode).append('\'');
    sb.append(", itemSku='").append(itemSku).append('\'');
    sb.append(", productSku='").append(productSku).append('\'');
    sb.append(", merchantSku='").append(merchantSku).append('\'');
    sb.append(", itemCode='").append(itemCode).append('\'');
    sb.append(", isSynchronized=").append(isSynchronized);
    sb.append(", itemCatentryId='").append(itemCatentryId).append('\'');
    sb.append(", masterDataItem=").append(masterDataItem);
    sb.append(", pristineDataItem=").append(pristineDataItem);
    sb.append(", isLateFulfillment=").append(isLateFulfillment);

    sb.append(", ticketTemplateCode='").append(ticketTemplateCode).append('\'');
    sb.append(", etdNote='").append(etdNote).append('\'');
    sb.append(", cncActivated=").append(cncActivated);
    sb.append(", uniqueId='").append(uniqueId).append('\'');
    sb.append(", isSubscribable=").append(isSubscribable);
    sb.append(", archived=").append(archived);
    sb.append('}');
    return sb.toString();
  }
}
