package com.gdn.mta.product.entity;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemNote {
  public String itemSku;
  public String skuCode;
  public int itemNumber;
  public String itemName;
  public List<String> vendorNotes;
  public List<String> vendorErrorFields;
  public List<String> merchantModifiedFields;
}
