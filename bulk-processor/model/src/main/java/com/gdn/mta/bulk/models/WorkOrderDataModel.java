package com.gdn.mta.bulk.models;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class WorkOrderDataModel {
  private String sourceItemSku;
  private String destinationItemSku;
  private String sourceItemName;
  private String destinationItemName;
  private String warehouseCode;
  private String stock;
  private String bundleType;
  private List<ChildSkuAndCogsMapping> childSkuAndCogsMapping = new ArrayList<>();
}
