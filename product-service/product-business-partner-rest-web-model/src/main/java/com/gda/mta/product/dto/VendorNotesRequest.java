package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class VendorNotesRequest implements Serializable {
  private static final long serialVersionUID = -4895088075333096290L;

  private List<String> vendorNotes = new ArrayList<>();
  private List<String> vendorErrorFields = new ArrayList<>();
  private List<String> merchantModifiedFields = new ArrayList<>();
  private String contentAdditionalNotes;
  private Boolean allVariants;
  private String imagesAdditionalNotes;
  private List<String> imageReason = new ArrayList<>();
  private List<String> commonImageReason = new ArrayList<>();
  private List<ItemNotesDto> itemNotes = new ArrayList<>();
  private List<String> allModifiedFields = new ArrayList<>();
}
