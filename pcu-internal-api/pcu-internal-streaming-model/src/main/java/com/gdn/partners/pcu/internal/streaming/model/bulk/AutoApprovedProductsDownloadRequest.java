package com.gdn.partners.pcu.internal.streaming.model.bulk;

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
public class AutoApprovedProductsDownloadRequest extends BulkDownloadRequest{
  private String keyword;
  private String categoryCode;
  private String assignedTo;
  private String sellerCode;
  private Boolean b2bActivated;
  private String sortOrder;
  private List<String> productCodeList = new ArrayList<>();
}
