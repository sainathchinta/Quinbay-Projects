package com.gdn.mta.bulk.models.download;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = true)
@JsonIgnoreProperties(ignoreUnknown = true)
public class MasterSkuInReviewDownloadRequest extends BulkDownloadRequest {
  private static final long serialVersionUID = -8979384937638032451L;
  private int page;
  private int size;
  private String assignedTo;
  private String keyword;
  private String categoryCode;
  private long startDate;
  private long endDate;
  private List<AnchorMappingRequest> clusterRequestList = new ArrayList<>();
}
