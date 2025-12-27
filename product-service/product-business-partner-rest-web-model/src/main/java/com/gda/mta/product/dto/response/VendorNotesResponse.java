package com.gda.mta.product.dto.response;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.gda.mta.product.dto.ItemNotesDto;
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
@JsonInclude
public class VendorNotesResponse  extends BaseResponse {
  private static final long serialVersionUID = 1172525319033737818L;

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
