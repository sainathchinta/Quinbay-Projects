package com.gdn.mta.bulk.models.download.responsedata;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BulkPriceProductTypeTaggingRequest implements Serializable {
  private String storeId;
  private String itemSku;
  private String pickupPointCode;
  private String productTypeTagging;
  private String deleteProductTypeTagging;
  private String id;
  private Integer excelRowNumber;
}
