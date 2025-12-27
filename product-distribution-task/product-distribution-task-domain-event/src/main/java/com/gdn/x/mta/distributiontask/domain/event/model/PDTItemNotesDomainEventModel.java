package com.gdn.x.mta.distributiontask.domain.event.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class PDTItemNotesDomainEventModel {
  private String skuCode;
  private String itemName;
  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
}
