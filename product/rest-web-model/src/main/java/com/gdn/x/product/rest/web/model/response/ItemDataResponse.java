package com.gdn.x.product.rest.web.model.response;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemDataResponse extends BaseResponse {
  private String merchantCode;
  private String itemSku;
  private String productSku;
  private String merchantSku;
  private String itemCode;
  private String itemCatentryId;
  private MasterDataItemDTO masterDataItem;
  private PristineDataItemDto pristineDataItem;
  private String ticketTemplateCode;
  private String etdNote;
  private boolean off2OnChannelActive;
  private String uniqueId;
  private boolean disableUnSync;
  private String sourceItemCode;
  private boolean markForDelete;
  private boolean freeSample;
  private boolean archivedBeforeSuspension;
  private boolean subscribable;
  private boolean lateFulfillment;
  private boolean contentChanged;
  private boolean isSynchronized;
  private boolean archived;
  private Long version;
}
