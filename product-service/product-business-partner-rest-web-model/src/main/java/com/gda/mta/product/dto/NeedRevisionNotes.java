package com.gda.mta.product.dto;

import java.io.Serializable;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(ignoreUnknown = true)
public class NeedRevisionNotes implements Serializable {
  private static final long serialVersionUID = 517716075353929150L;

  private List<String> vendorNotes;
  private List<String> vendorErrorFields;
  private String contentAdditionalNotes;
  private boolean allVariants;
  private String imagesAdditionalNotes;
  private List<String> imageReason;
  private List<String> commonImageReason;
  private List<ItemNeedRevisionNotes> itemNotes;
}
