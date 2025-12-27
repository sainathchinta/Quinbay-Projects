package com.gdn.mta.product.valueobject;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@JsonIgnoreProperties(
    ignoreUnknown = true
)
@Data
@AllArgsConstructor
@ToString
@NoArgsConstructor
public class InventoryStockInfoDTO extends BaseResponse {
  private static final long serialVersionUID = 1960444570065351273L;
  private String webMerchantCode;
  private String webProductSku;
  private int webTotalAvailableStock;
  private int webTotalOriginalStock;
  private int warehouseTotalAvailableStock;
  private int warehouseTotalOriginalStock;
  private int totalStock;
}



