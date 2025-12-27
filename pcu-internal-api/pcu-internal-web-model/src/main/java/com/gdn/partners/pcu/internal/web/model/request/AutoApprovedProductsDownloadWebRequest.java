package com.gdn.partners.pcu.internal.web.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonInclude(JsonInclude.Include.ALWAYS)
@JsonIgnoreProperties(ignoreUnknown = true)
public class AutoApprovedProductsDownloadWebRequest {

  private String keyword;
  private String categoryCode;
  private String sortOrder;
  private String sellerCode;
  private boolean b2bActivated;
  private boolean b2cActivated;
  private String assignedTo;
  private List<String> productCodeList = new ArrayList<>();
}
