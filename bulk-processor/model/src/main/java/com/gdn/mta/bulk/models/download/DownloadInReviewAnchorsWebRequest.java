package com.gdn.mta.bulk.models.download;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DownloadInReviewAnchorsWebRequest implements Serializable {
  private static final long serialVersionUID = 6975687820922682414L;
  private String assignedTo;
  private String keyword;
  private String categoryCode;
  private long startDate;
  private long endDate;
  private List<AnchorMappingModel> clusterRequestList;
}
