package com.gdn.partners.pcu.internal.web.model.response;

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
public class NeedRevisionNotesWebResponse {

  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
  private String contentAdditionalNotes;
  private Boolean allVariants;
  private String imagesAdditionalNotes;
  private List<String> imageReason;
  private List<ItemNotesWebResponse> itemNotes;
}
