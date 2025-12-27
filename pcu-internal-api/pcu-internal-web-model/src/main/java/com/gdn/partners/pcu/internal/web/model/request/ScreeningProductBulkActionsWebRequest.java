package com.gdn.partners.pcu.internal.web.model.request;

import java.util.ArrayList;
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
public class ScreeningProductBulkActionsWebRequest {

  private List<String> productCodes = new ArrayList<>();
  private String assignTo;
  private String assignedBy;
  private String correctionReason;
  private String additionalNotes;
  private String rejectionReason;
  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
  private String contentAdditionalNotes;
  private boolean allVariants;
  private String imagesAdditionalNotes;
  private List<String> imageReason;
  private List<String> commonImageReason;
  private List<ItemNotesWebRequest> itemNotes;
  private String needRevisionType;
}
