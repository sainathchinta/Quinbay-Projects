package com.gda.mta.product.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.common.web.base.BaseResponse;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class ProductRevisionInfoResponse extends BaseResponse {

  private static final long serialVersionUID = -6973581230248133777L;

  private String correctionReason;
  private String additionalNotes;
  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
  private String contentAdditionalNotes;
  private boolean allVariants;
  private String imagesAdditionalNotes;
  private List<String> imageReason;
  private List<String> commonImageReason;
  private List<ItemNeedRevisionNotes> itemNotes;
}
