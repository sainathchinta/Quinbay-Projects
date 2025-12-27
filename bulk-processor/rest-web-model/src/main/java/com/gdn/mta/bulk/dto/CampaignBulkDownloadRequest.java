package com.gdn.mta.bulk.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Data
@AllArgsConstructor
@NoArgsConstructor
@ToString
@Builder
@JsonIgnoreProperties(ignoreUnknown = true)
public class CampaignBulkDownloadRequest implements Serializable {
  private static final long serialVersionUID = 396683628860431917L;

  private String campaignCode;
  private int page;
}
