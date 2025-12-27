package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkSkuLevelRebateRequestData implements Serializable {
  private String itemSku;
  private String pickupPointCode;
  private String rebate;
  private int excelRowNumber;
}
