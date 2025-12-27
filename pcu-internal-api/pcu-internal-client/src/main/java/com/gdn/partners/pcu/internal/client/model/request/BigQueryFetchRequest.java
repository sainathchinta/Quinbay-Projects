package com.gdn.partners.pcu.internal.client.model.request;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
@JsonInclude
public class BigQueryFetchRequest {
  private boolean mskuReviewDeltaFetch;
  private boolean mSkuIdDateRangeFetch;
  private int startHourCount;
  private int endHourCount;
  private boolean anchorDetailsDeltaFetch;
  private boolean anchorDetailsRangeFetch;
  private boolean mskuIdItemSkuListFetch;
  private List<String> itemSkuList = new ArrayList<>();
}
