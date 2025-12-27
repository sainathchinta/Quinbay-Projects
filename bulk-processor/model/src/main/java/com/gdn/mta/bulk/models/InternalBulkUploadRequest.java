package com.gdn.mta.bulk.models;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class InternalBulkUploadRequest {
  private String productCode;
  private String productName;
  private String brand;
  private String length;
  private String width;
  private String height;
  private String weight;
  private String dangerousGoodsLevel;
}
