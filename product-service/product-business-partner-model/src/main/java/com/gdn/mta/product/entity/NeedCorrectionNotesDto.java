package com.gdn.mta.product.entity;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class NeedCorrectionNotesDto {
  public List<String> vendorNotes;
  public List<String> imageReason;
  public List<String> commonImageReason;
  public List<String> vendorErrorFields;
  public List<String> merchantModifiedFields;
  public String contentAdditionalNotes;
  public boolean allVariants;
  public String imagesAdditionalNotes;
  public List<ItemNote> itemNotes;
  private List<String> allModifiedFields;
}
