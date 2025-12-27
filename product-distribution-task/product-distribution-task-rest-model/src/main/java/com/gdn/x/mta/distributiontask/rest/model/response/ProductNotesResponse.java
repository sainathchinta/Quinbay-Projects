package com.gdn.x.mta.distributiontask.rest.model.response;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductNotesResponse {

  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
  private String contentAdditionalNotes;
  private boolean allVariants;
  private String imagesAdditionalNotes;
  private List<String> imageReason;
  private List<String> commonImageReason;
  private List<String> modifiedFields;
  private String lastModified;
}
