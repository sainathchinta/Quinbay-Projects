package com.gdn.mta.bulk.models.download;

import java.io.Serializable;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@JsonIgnoreProperties(ignoreUnknown = true)
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CampaignProductDownloadRequest extends BulkDownloadRequest implements Serializable {

  private static final long serialVersionUID = 1204043570895149587L;
  private CampaignItemSummaryRequest campaignItemSummaryRequest;
  private Map<String, Boolean> privilegedMap;
  private int page;
  private int size;
  private String promoType;
  private String recommendedWeek;
  private String campaignCode;

  @Override
  public String toString() {
    return "CampaignProductDownloadRequest{" + "campaignItemSummaryRequest=" + campaignItemSummaryRequest
        + ", privilegedMap=" + privilegedMap + ", page=" + page + ", size=" + size + ", promoType=" + promoType
        + ", recommendedWeek=" + recommendedWeek + ", campaignCode=" + campaignCode + '}';
  }
}
