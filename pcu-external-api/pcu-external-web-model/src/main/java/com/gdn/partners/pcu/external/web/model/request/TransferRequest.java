package com.gdn.partners.pcu.external.web.model.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import com.gdn.common.web.base.BaseRequest;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
public class TransferRequest extends BaseRequest {

  private String productSku;
  private String itemSku;
  private String itemCode;
  private String itemName;
  private String sourceProductSku;
  private String sourceItemSku;
  private String sourceItemCode;
  private String sourceItemName;
  private String merchantCode;
  private String merchantType;
  private String warehouseCode;
  private int stockQuota;
}
