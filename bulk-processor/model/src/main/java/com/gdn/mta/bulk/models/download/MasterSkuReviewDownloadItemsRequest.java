package com.gdn.mta.bulk.models.download;

import java.io.Serializable;
import java.util.ArrayList;
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
public class MasterSkuReviewDownloadItemsRequest extends BulkDownloadRequest implements Serializable {
  private static final long serialVersionUID = -3262862553710634348L;
  private String keyword;
  private String categoryCode;
  private Long startDate;
  private Long endDate;
  private int page;
  private int limit;
  private List<String> itemSkuList = new ArrayList<>();
}
