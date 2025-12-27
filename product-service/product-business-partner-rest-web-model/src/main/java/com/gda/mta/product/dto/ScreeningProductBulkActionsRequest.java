package com.gda.mta.product.dto;

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
public class ScreeningProductBulkActionsRequest extends NeedRevisionNotes {

  private static final long serialVersionUID = -4510544499440489829L;
  private List<String> productCodes = new ArrayList<>();
  private String assignTo;
  private String assignedBy;
  private String correctionReason;
  private String additionalNotes;
  private String rejectionReason;
  private List<String> vendorNotes;
  private String contentAdditionalNotes;
  private List<String> imageReason;
  private List<String> commonImageReason;
  private String imagesAdditionalNotes;
  private List<String> vendorErrorFields;
  private Boolean allVariants = false;
  private boolean screeningAction;
}
