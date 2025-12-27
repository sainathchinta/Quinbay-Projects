package com.gda.mta.product.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class ItemNotesDto {
  private String itemSku;
  private String skuCode;
  private Integer itemNumber;
  private String itemName;
  private List<String> vendorNotes = new ArrayList<>();
  private List<String> vendorErrorFields = new ArrayList<>();
  private List<String> merchantModifiedFields = new ArrayList<>();
}
