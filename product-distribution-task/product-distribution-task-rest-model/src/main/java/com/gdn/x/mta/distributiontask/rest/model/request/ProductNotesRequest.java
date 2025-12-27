package com.gdn.x.mta.distributiontask.rest.model.request;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@ToString
public class ProductNotesRequest extends BaseRequest {

  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
  private String contentAdditionalNotes;
  private Boolean allVariants;
  private String imagesAdditionalNotes;
  private List<String> imageReason;
  private List<String> commonImageReason;
  private List<String> modifiedFields;
  private String lastModified;
}
