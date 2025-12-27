package com.gdn.mta.bulk.models.download.responsedata;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class BulkMasterSkuInReviewDownloadResponse extends BulkDataResponse
  implements Serializable {
  private static final long serialVersionUID = -4902658325789091389L;
  private List<InReviewAnchorDownloadResponse> anchorDownloadResponseList;
}
