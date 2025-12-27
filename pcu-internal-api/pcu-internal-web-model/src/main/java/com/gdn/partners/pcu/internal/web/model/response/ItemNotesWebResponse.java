package com.gdn.partners.pcu.internal.web.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ItemNotesWebResponse {

  private String itemSku;
  private String skuCode;
  private Integer itemNumber;
  private String itemName;
  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
}
