package com.gdn.partners.pcu.internal.web.model.response;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductRevisionHistoryWebResponse {

  private String id;
  private String storeId;
  private String correctionReason;
  private String additionalNotes;
  private String createdBy;
  private Date createdDate;
  private String updatedBy;
  private Date updatedDate;
  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
  private String contentAdditionalNotes;
  private Boolean allVariants;
  private String imagesAdditionalNotes;
  private List<String> imageReason;
  private List<ItemNotesWebResponse> itemNotes;
}
