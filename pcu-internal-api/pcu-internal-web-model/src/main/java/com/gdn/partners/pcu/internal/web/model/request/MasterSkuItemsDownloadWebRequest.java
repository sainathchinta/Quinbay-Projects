package com.gdn.partners.pcu.internal.web.model.request;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterSkuItemsDownloadWebRequest {

  private String keyword;
  private String categoryCode;
  private Long startDate;
  private Long endDate;
  private List<String> itemSkuList = new ArrayList<>();
}
