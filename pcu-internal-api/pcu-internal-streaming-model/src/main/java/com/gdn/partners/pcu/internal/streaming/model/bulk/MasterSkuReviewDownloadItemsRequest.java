package com.gdn.partners.pcu.internal.streaming.model.bulk;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterSkuReviewDownloadItemsRequest extends BulkDownloadRequest {

  private String keyword;
  private String categoryCode;
  private Long startDate;
  private Long endDate;
  private int page;
  private int limit;
  private List<String> itemSkuList = new ArrayList<>();
}
