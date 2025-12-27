package com.gdn.mta.bulk.models;

import com.gda.mta.product.dto.BaseRequest;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
public class TransferRequest extends BaseRequest {

  private static final long serialVersionUID = -1504805592072118973L;

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
